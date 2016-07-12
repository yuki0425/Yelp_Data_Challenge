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
sub_business =sub_business[1:12956]
tmp = sapply(1:length(sub_business),function(j){
  a = sum(sub_business[[j]]$categories %in% c('Thai'))
  if(a > 0){
    return(TRUE)
  }else{
    return(FALSE)
  }})

indian_food = which(tmp)


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
  ggtitle("Different star of reviews on Thai food")








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
phoenix_dat$business_id = names(sub_business[index])


map = get_map(
  location = 'Phoenix, AZ',
  zoom = 11,
  maptype = 'roadmap')

#good heat map
p = ggmap(map, extent = "device") + 
  geom_density2d(data = phoenix_dat, aes(x = long, y = lat), size = 0.3) + 
  stat_density2d(data = phoenix_dat,aes(x = long, y = lat, fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red", guide = FALSE) + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
p = p + geom_point(data = phoenix_dat
                   , aes(x = long, y = lat),colour = 'red', alpha = 0.4, size = 2,
                   h = 1)
#p = p + ggtitle("Heatmap of Restaurants in Phonix")
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

print(p)


#headmap for reviews
busid = sapply(1:length(sub_review), function(i){
  sub_review[[i]]$business_id
})
index = category_city[[3]]
phonix_business_id = names(sub_business)[index]
phonix_review = sub_review[busid %in% phonix_business_id]
review_busid = sapply(1:length(phonix_review), function(i){
  phonix_review[[i]]$business_id
})

review_busid_index = sapply(1:length(phonix_review), function(i){
  which(grepl(review_busid[i], phonix_business_id))
})
lan_long = sapply(1:length(phonix_review), function(i){
  bus_id = sub_business[[index[review_busid_index[i]]]]$business_id
  long = sub_business[[index[review_busid_index[i]]]]$longitude
  lat = sub_business[[index[review_busid_index[i]]]]$latitude
  rev_stars = phonix_review[[i]]$stars
  
  return(c(long,lat,bus_id,rev_stars))
})
lan_long = as.data.frame(t(lan_long),stringsAsFactors = F)
colnames(lan_long) = c('long',"lat",'business_id','rev_stars')
lan_long$long = as.numeric(lan_long$long)
lan_long$lat = as.numeric(lan_long$lat)
lan_long$rev_stars = as.numeric(lan_long$rev_stars)

map = get_map(
  location = 'Phoenix, AZ',
  zoom = 11,
  maptype = 'roadmap')

#good heat map
p = ggmap(map, extent = "device") + 
  geom_density2d(data = lan_long, aes(x = long, y = lat), size = 0.3) + 
  stat_density2d(data = lan_long,aes(x = long, y = lat, fill = ..level.., alpha = ..level..), 
                 size = 0.1, bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "black", high = "red", guide = FALSE) + 
  scale_alpha(range = c(0, 1), guide = FALSE)
p = p + geom_point(data = lan_long
                   , aes(x = long, y = lat),colour = 'red', alpha = 0.4, size = 2,
                   h = 1)
#p = p + ggtitle("Heatmap of reviews in Phonix")
print(p)






#prediction

phonix_star_pred =  aggregate(lan_long[, 4], list(lan_long$business_id), mean)
colnames(phonix_star_pred) = c('business_id','review_stars')

tmp = sapply(1:dim(phonix_star_pred)[1], function(i){
  a = which(grepl(phonix_star_pred$business_id[i], phonix_business_id))
  stars = sub_business[[index[a]]]$stars
  long = sub_business[[index[a]]]$longitude
  lat = sub_business[[index[a]]]$latitude
  review_count = sub_business[[index[a]]]$review_count
  smoking = sub_business[[index[a]]]$attributes$Smoking
  if(length(smoking) == 0) smoking=NA
  wifi = sub_business[[index[a]]]$attributes$'Wi-Fi'
  if(length(wifi) == 0) wifi=NA
  noise_level = sub_business[[index[a]]]$attributes$'Noise Level'
  if(length(noise_level) == 0) noise_level=NA
  reservations = sub_business[[index[a]]]$attributes$'Takes Reservations'
  if(length(reservations) == 0) reservations=NA
  delivery = sub_business[[index[a]]]$attributes$'Delivery'
  if(length(delivery) == 0) delivery=NA
  parking = (sum(unlist(sub_business[[index[a]]]$attributes$Parking))>0)
  tv = sub_business[[index[a]]]$attributes$'Has TV'
  if(length(tv) == 0) tv=NA
  attire = sub_business[[index[a]]]$attributes$Attire
  if(length(attire) == 0) attire=NA
  waiter =  sub_business[[index[a]]]$attributes$'Waiter Service'
  if(length(waiter) == 0) waiter=NA
  alcohol =   sub_business[[index[a]]]$attributes$Alcohol
  if(length(alcohol) == 0) alcohol=NA
  return(c(stars, long, lat, review_count, 
           smoking, wifi, noise_level,
           reservations, delivery, 
           parking, tv, attire, 
           waiter, alcohol))
})

z = as.data.frame(t(tmp), stringsAsFactors =F)
colnames(z) = c('stars','long','lat','review_count','smoking',
                'wifi','noise_level', 'reservations', 'delivery',
                'parking','tv','attire','waiter','alcohol')

phonix_star_pred = cbind(phonix_star_pred,z)
phonix_star_pred = phonix_star_pred[,c(2,3,6, 9, 12,16)]
sub_index = sapply(1:dim(phonix_star_pred)[1], function(i){
  num_na = sum(is.na(phonix_star_pred[i,]))
  if(num_na > 0){
    return(FALSE)
  }else{
    return(TRUE)
  }
})
phonix_star_pred = phonix_star_pred[sub_index,]

phonix_star_pred$stars = as.numeric(phonix_star_pred$stars)
phonix_star_pred$long =as.numeric(as.character(phonix_star_pred$long))
phonix_star_pred$lat =as.numeric(as.character(phonix_star_pred$lat))
phonix_star_pred$review_count =as.numeric(as.character(phonix_star_pred$review_count))
phonix_star_pred$smoking = as.factor(phonix_star_pred$smoking)
phonix_star_pred$wifi = as.factor(phonix_star_pred$wifi)
phonix_star_pred$noise_level = as.factor(phonix_star_pred$noise_level)
phonix_star_pred$reservations[phonix_star_pred$reservations == '0' & 
                                !is.na(phonix_star_pred$reservations)] = 
  FALSE
phonix_star_pred$reservations[phonix_star_pred$reservations == '1' & 
                                !is.na(phonix_star_pred$reservations)] = 
  TRUE
phonix_star_pred$reservations = as.factor(phonix_star_pred$reservations)
phonix_star_pred$delivery[phonix_star_pred$delivery == '0' & 
                            !is.na(phonix_star_pred$delivery)] = 
  FALSE
phonix_star_pred$delivery[phonix_star_pred$delivery == '1' & 
                            !is.na(phonix_star_pred$delivery)] = 
  TRUE
phonix_star_pred$delivery = as.factor(phonix_star_pred$delivery)
phonix_star_pred$tv[phonix_star_pred$tv == '0' & 
                      !is.na(phonix_star_pred$tv)] = 
  FALSE
phonix_star_pred$tv[phonix_star_pred$tv == '1' & 
                      !is.na(phonix_star_pred$tv)] = 
  TRUE
phonix_star_pred$tv = as.factor(phonix_star_pred$tv)
phonix_star_pred$attire = as.factor(phonix_star_pred$attire)
phonix_star_pred$waiter[phonix_star_pred$waiter == '0' & 
                          !is.na(phonix_star_pred$waiter)] = 
  FALSE
phonix_star_pred$waiter[phonix_star_pred$waiter == '1' & 
                          !is.na(phonix_star_pred$waiter)] = 
  TRUE
phonix_star_pred$waiter = as.factor(phonix_star_pred$waiter)
phonix_star_pred$alcohol = as.factor(phonix_star_pred$alcohol)
phonix_star_pred$parking[phonix_star_pred$parking == '0' & 
                           !is.na(phonix_star_pred$parking)] = 
  FALSE
phonix_star_pred$parking[phonix_star_pred$parking == '1' & 
                           !is.na(phonix_star_pred$parking)] = 
  TRUE
phonix_star_pred$parking = as.factor(phonix_star_pred$parking)



train = sample(1:dim(phonix_star_pred)[1], 1800)
train_set = phonix_star_pred[train,-1]
test_set = phonix_star_pred[-train,-1]
fit1 = glm(stars~., data = train_set)
fit = glm(stars~., data = phonix_star_pred[,-c(1,2,6)])

#fit = glm(stars~review_count + noise_level +  parking + alcohol, data = train_set[,-1])
pred = predict(fit1, test_set,type= 'response')
pred = sapply(1:length(pred), function(i){
  a = c(1,1.5,2, 2.5, 3, 3.5, 4, 4.5, 5)
  distance = abs(a - pred[i])
  a[which.min(distance)]
})
table(pred, test_set[,1])
