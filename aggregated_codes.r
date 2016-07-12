

#get subset of data
library("rjson")
x = readLines("yelp_academic_dataset_business.json")
business_data = replicate(length(x), list())
for(i in 1:length(x)){
  business_data[[i]] = fromJSON(x[i])
}

x = readLines("yelp_academic_dataset_review.json")
review_data = replicate(length(x), list())
for(i in 1:length(x)){
  review_data[[i]] = fromJSON(x[i])
}

x = readLines("yelp_academic_dataset_user.json")
user_data = replicate(length(x), list())
for(i in 1:length(x)){
  user_data[[i]] = fromJSON(x[i])
}

x = readLines("yelp_academic_dataset_checkin.json")
checkin_data = replicate(length(x), list())
for(i in 1:length(x)){
  checkin_data[[i]] = fromJSON(x[i])
}

user_id = sapply(1:length(user_data), function(i) user_data[[i]]$user_id)
names(user_data) = user_id

business_id = sapply(1:length(business_data), function(i) business_data[[i]]$business_id)
names(business_data)= business_id 



x = readLines("yelp_academic_dataset_tip.json")
tip_data = replicate(length(x), list())
for(i in 1:length(x)){
  tip_data[[i]] = fromJSON(x[i])
}

business_restaurant = rep(0, length(business_data))

business_restaurant = sapply(1:length(business_data), function(i){
  if (sum(grepl("restaurant",business_data[[i]]$categories, ignore.case = T)) > 0){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
})

#all business_id of restaurants from business data
restaurant_id = names(business_data)[business_restaurant]

review_businessid = sapply(1:length(review_data), function(i){
  review_data[[i]]$business_id
})

review_if_restaurant = review_businessid %in% restaurant_id

#new data contains only restaurant
bus_data = business_data[business_restaurant]
rew_data = review_data[review_if_restaurant]

city = sapply(1:length(bus_data), function(i){
  bus_data[[i]]$city
})
state = sapply(1:length(bus_data), function(i){
  bus_data[[i]]$state
})

#There are 270 cities in the dataset
address = paste(city,state,sep=',')
sort(table(address))

#We only analyze cities which has more than 1000 restaurants
#there are 7 cities left
#"Charlotte,NC"  "Edinburgh,EDH" "Las Vegas,NV"  "Montr√©al,QC"  
# "Phoenix,AZ"    "Pittsburgh,PA" "Scottsdale,AZ"
sub_address = names(which(table(address) > 1000))
if_sub = sapply(1:length(bus_data), function(i) {
  add = paste(bus_data[[i]]$city, bus_data[[i]]$state, sep=',')
  if(add %in% sub_address){
    return(TRUE)
  }else{
    return(FALSE)
  }
})

#the final data we want to do analysis
sub_business = bus_data[if_sub]   # 12956
sub_bus_id = names(sub_business)
review_if_sub = review_businessid %in% sub_bus_id
sub_review = review_data[review_if_sub] #725327

#split review text into words
reviews = lapply(1:length(sub_review), function(i){
  z = unlist(strsplit(sub_review[[i]]$text, ' |\n|\t|[[:punct:]]' ))
  z[z != '']
})


sub_user_id = sapply(1:length(sub_review), function(i){
  sub_review[[i]]$user_id
})
sub_user_id = unique(sub_user_id)
user_id = sapply(1:length(user_data), function(i){
  user_data[[i]]$user_id
})
sub_user = user_data[user_id %in% sub_user_id]   #218660




#difference between cities
#there are 7 cities left
#"Charlotte,NC"  "Edinburgh,EDH" "Las Vegas,NV"  "Montr√©al,QC"  
# "Phoenix,AZ"    "Pittsburgh,PA" "Scottsdale,AZ"
load('subdata.rda')
city = sapply(1:length(sub_business), function(i){
  sub_business[[i]]$city
})
state = sapply(1:length(sub_business), function(i){
  sub_business[[i]]$state
})
address = paste(city,state,sep=',')
sub_business$city_state = address
uniq_address = unique(address)
category_city = sapply(1:length(uniq_address), function(i){
  which(address == uniq_address[i])
})
names(category_city) = uniq_address

#the category of each restaurant in each city
category_rest = replicate(7, list())
for(i in 1:length(uniq_address)){
  tmp = unlist(sapply(1:length(category_city[[i]]), function(j) {
    sub_business[[category_city[[i]][j]]]$categories}))
  category_rest[[i]] = tmp[tmp !='Restaurants']
}
names(category_rest) = uniq_address


#the most popular kind of food in this city
famous_city = replicate(7,list())
for(i in 1:length(uniq_address)){
  famous_city[[i]] = sort(table(category_rest[[i]]),decreasing = T)[1:20] / length(category_rest[[i]])
}
names(famous_city) = uniq_address

#different kind of food in each city
food = matrix(rep(0,49),ncol = 7)
colnames(food) = c("Chinese",'Japanese','Indian','Italian', 'French', 'Mexican','Thai')
rownames(food) = uniq_address
for(i in 1:length(uniq_address)){
  tmp = table(category_rest[[i]]) / length(category_rest[[i]])
  food[i,] = tmp[colnames(food)]
}
food.frame = data.frame(category = rep(c("Chinese",'Japanese','Indian','Italian', 'French', 'Mexican','Thai'), 7)
                        , city = rep(uniq_address,each = 7))
food.frame$percent = as.vector(t(food))

library(ggplot2)
ggplot(data = food.frame, aes(x = city, y=percent, fill = category))+ 
  geom_bar(colour="black", stat="identity",
           position=position_dodge(),
           size=.3) +
  xlab("City") + ylab("Percentage") +
  ggtitle("Different kind of popular food in different cities")

##save in chinesefood.png
# Different cities have their respective food styles. For example, Phoenix has many Mexican
#restaurants. The reason is that Phoenix locates in southwest America, which is not
#far from Mexica. 


rate_rest = replicate(7, list())
for(i in 1:length(uniq_address)){
  tmp = unlist(sapply(1:length(category_city[[i]]), function(j) {
    sub_business[[category_city[[i]][j]]]$stars}))
  rate_rest[[i]] = tmp
}
names(rate_rest) = uniq_address


#analysis of chinese food in different cities
chinese_food = replicate(7,list())
for(i in 1:length(uniq_address)){
  tmp = unlist(sapply(1:length(category_city[[i]]), function(j) {
    a = sum(sub_business[[category_city[[i]][j]]]$categories %in% c('Chinese'))
    if(a > 0){
      return(TRUE)
    }else{
      return(FALSE)
    }}))
  chinese_food[[i]] = category_city[[i]][tmp]
}

rate_chinese = replicate(7, list())
for(i in 1:length(uniq_address)){
  tmp = unlist(sapply(1:length(chinese_food[[i]]), function(j) {
    sub_business[[chinese_food[[i]][j]]]$stars}))
  rate_chinese[[i]] = tmp
}
names(rate_chinese) = uniq_address

rate_chinese_frame = data.frame(city = unlist(sapply(1:7,function(i){
  rep(uniq_address[i],length(rate_chinese[[i]]))
})), stars = unlist(rate_chinese))

rate_chinese_frame$stars = as.factor(rate_chinese_frame$stars)
ggplot(rate_chinese_frame, aes(city, fill = stars))+ 
  geom_bar() + coord_flip()+
  ylab("Number of Restaurants") +xlab("City") +
  ggtitle("Different star of chinese food in different cities")



#analysis of Thai food in different cities
thai_food = replicate(7,list())
for(i in 1:length(uniq_address)){
  tmp = unlist(sapply(1:length(category_city[[i]]), function(j) {
    a = sum(sub_business[[category_city[[i]][j]]]$categories %in% c('Thai'))
    if(a > 0){
      return(TRUE)
    }else{
      return(FALSE)
    }}))
  thai_food[[i]] = category_city[[i]][tmp]
}

rate_thai = replicate(7, list())
for(i in 1:length(uniq_address)){
  tmp = unlist(sapply(1:length(thai_food[[i]]), function(j) {
    sub_business[[thai_food[[i]][j]]]$stars}))
  rate_thai[[i]] = tmp
}
names(rate_thai) = uniq_address

rate_thai_frame = data.frame(city = unlist(sapply(1:7,function(i){
  rep(uniq_address[i],length(rate_thai[[i]]))
})), stars = unlist(rate_thai))

rate_thai_frame$stars = as.factor(rate_thai_frame$stars)
ggplot(rate_thai_frame, aes(city, fill = stars))+ 
  geom_bar() + coord_flip()+
  ylab("Number of Restaurants") +xlab("City") +
  ggtitle("Different star of Thai food in different cities")



busid = sapply(1:length(sub_review), function(i){
  sub_review[[i]]$business_id
})
#all review stars about Thai food in different cities
s = lapply(1:7, function(j){
  index = thai_food[[j]]
  thai_busid = names(sub_business)[index]
  sub_thai_review = sub_review[busid %in% thai_busid]
  thai_review_stars = sapply(1:length(sub_thai_review),function(i){
    sub_thai_review[[i]]$stars
  })
  thai_frame = data.frame(city = rep(uniq_address[j], length(thai_review_stars)),
                          stars = thai_review_stars)
  return(thai_frame)
})
thai_review_frame = Reduce(function(x,y) merge(x,y,all = T), s)
thai_review_frame$stars = as.factor(thai_review_frame$stars)
ggplot(thai_review_frame, aes(city, fill = stars))+ 
  geom_bar() + coord_flip()+
  ylab("Number of Reviews") +xlab("City") +
  ggtitle("Different star of reviews on Thai food in different cities")





#analysis of Indian food in Pheonix

tmp = unlist(sapply(1:length(category_city[[3]]), function(j) {
  a = sum(sub_business[[category_city[[3]][j]]]$categories %in% c('Indian'))
  if(a > 0){
    return(TRUE)
  }else{
    return(FALSE)
  }}))
indian_food = category_city[[3]][tmp]


tmp = unlist(sapply(1:length(indian_food), function(j) {
  sub_business[[indian_food[j]]]$stars}))
rate_indian = tmp



#analysis of indian food in all these 7 cities

tmp = sapply(1:length(sub_business),function(j){
  a = sum(sub_business[[j]]$categories %in% c('Indian'))
  if(a > 0){
    return(TRUE)
  }else{
    return(FALSE)
  }})

indian_food = which(tmp > 0)


tmp = unlist(sapply(1:length(indian_food), function(j) {
  sub_business[[indian_food[j]]]$stars}))
rate_indian = floor(tmp)
indian_food_rate = replicate(5, list())
for(i in 1:5){
  indian_food_rate[[i]] = indian_food[rate_indian == i]
}


tmp = unlist(sapply(1:length(indian_food), function(j) {
  sub_business[[indian_food[j]]]$attributes$'Price Range'}))
price_indian = tmp
indian_food_price = replicate(4, list())
for(i in 1:4){
  indian_food_price[[i]] = indian_food[price_indian == i]
}

#all review stars about Indian food
m = indian_food_rate
s = lapply(1:length(m), function(j){
  index = m[[j]]
  cat_busid = names(sub_business)[index]
  sub_cat_review = sub_review[busid %in% cat_busid]
  cat_review_stars = sapply(1:length(sub_cat_review),function(i){
    sub_cat_review[[i]]$stars
  })
  cat_frame = data.frame(reststar = rep(j, length(cat_review_stars)),
                         stars = cat_review_stars)
  return(cat_frame)
})
cat_review_frame = Reduce(function(x,y) merge(x,y,all = T), s)
cat_review_frame$stars = as.factor(cat_review_frame$stars)
ggplot(cat_review_frame, aes(reststar, fill = stars))+ 
  geom_bar(binwidth = 0.4) + coord_flip()+
  ylab("Number of Reviews") +xlab("Stars of the restaurants") +
  ggtitle("Different star of reviews on Indian food")








##maps Phoenix, AZ
library(ggmap)
library(mapproj)

index = category_city[[3]]
phoenix_stars = sapply(1:length(index), function(i){
  sub_business[[i]]$stars
})
phoenix_stars = floor(phoenix_stars)
phoenix_long = sapply(1:length(index), function(i){
  sub_business[[i]]$longitude
})
phoenix_lat = sapply(1:length(index), function(i){
  sub_business[[i]]$latitude
})
phoenix_dat = data.frame(stars = phoenix_stars, lat = phoenix_lat,
                         long = phoenix_long)
phoenix_dat$stars = as.factor(phoenix_dat$stars)


map = get_map(
  location = 'Phoenix, AZ',
  zoom = 11,
  maptype = 'roadmap')

#good heat map
p = ggmap(map)
p = p + geom_point(data = phoenix_dat
                   , aes(x = long, y = lat),colour = 'red', alpha = 0.4, size = 2,
                   h = 1)
print(p)

p = p + geom_point(data = phoenix_dat
                   , aes(x = long, y = lat,colour = stars),size = 3)
p = p + theme( panel.grid.major = element_blank()
               , panel.grid.minor = element_blank()
               , axis.text = element_blank()
               , axis.title = element_blank()
               , axis.ticks = element_blank()
)
print(p)


map = get_map(
  location = 'Phoenix, AZ',
  zoom = 11,
  maptype = 'roadmap')

p = ggmap(map)

p = p + stat_density2d(aes(x = long, y=lat, fill = ..level..,show_guide=F),
                       data= phoenix_dat,
                       geom='polygon',
                       bins = 10,
                       size = 0.000001)
p = p + scale_fill_gradient(low = "blue", high = "green")

print(p)


#good heatmap
p = ggmap(map, extent = "device") + 
  geom_density2d(data = phoenix_dat, aes(x = long, y = lat), size = 0.3) + 
  stat_density2d(data = phoenix_dat,aes(x = long, y = lat, fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red", guide = FALSE) + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
print(p)


