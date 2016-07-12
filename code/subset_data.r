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



save(sub_business, sub_review, file = 'subdata.rda')
save(sub_user,file = 'subuser.rda')

