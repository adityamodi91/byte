#importing dataset
library(readr)
library(forcats)
library(ggplot2)
library(dplyr)
library(corrplot)

#Reading the data
train <- read_csv("~/R/byte/train.csv")

# counting percentage NA values in every column of Dataset

# NA values
na_count <-sapply(train, function(y) (sum(length(which(is.na(y))))))
na_count

#deleting coulmns having more than 50% NA values
train$square_feet=NULL
train$security_deposit=NULL

#Understanding Data
View(train)

# As we know state is always Newyork (NY), so we can delete the rows where state is not NY and can delete this coulmn
unique(train$state)

# all values are NY or ny or Ny, deleting this coulmn
train$state=NULL

# As we can check in data smart_location is combination of city and state so that variable will not be useful, deleting
train$smart_location=NULL

#checking Market variable
sum(train$market== "New York", na.rm=TRUE )

# 29265 valuea are New york and 681 NA vales so this column is also newyork
#deleting market coulmn
train$market=NULL

#first variable is only id not useful
train=train[,-1]

# We have deleted 5 variables from data


# we can check price, extra people and cleaning fee are character because of $ sign , 
#and host_acceptance_rate, host_response_rate bacause of % sign converting them to numeric

train$extra_people=round(as.numeric(gsub("\\$", "", (train$extra_people))),0)
train$price=round(as.numeric(gsub("\\$", "", (train$price))),0)
train$cleaning_fee=round(as.numeric(gsub("\\$", "", (train$cleaning_fee))),0)
train$host_response_rate=as.numeric(gsub("%", "", (train$host_response_rate) ))
train$host_acceptance_rate=as.numeric(gsub("%", "", (train$host_acceptance_rate) ))


#convert zipcode to factor
train$zipcode=as.factor(train$zipcode)


#Converting all integer to numeric
integer_vars <- lapply(train, class) == "integer"
train[, integer_vars] <- lapply(train[, integer_vars], as.numeric)


#converting  character to factor variable
character_vars <- lapply(train, class) == "character"
train[, character_vars] <- lapply(train[, character_vars], as.factor)

#getting corelation of data
num.cols <- sapply(train, is.numeric)
cor.data <- cor(train[,num.cols], use="pairwise.complete.obs")
corrplot(cor.data,method='color')


#dividing coulmns host_varification, street and amenities into different columns
host_verification=split(train$host_verifications)
host_verification<- data.frame(sapply(data.frame(host_verification), as.factor))

amenities=spliltAminities(train$amenities)
amenities <- data.frame(sapply(data.frame(amenities), as.factor))

street<-splitStreet(train$street)
street <- data.frame(sapply(data.frame(street), as.factor))

train$street=NULL
train$amenities=NULL
train$host_verifications=NULL
train=cbind(train, amenities, host_verification, street)

# Now little text converting in columns neighbourhood_cleansed and city
train$neighbourhood_cleansed=tolower(gsub(" ", '', train$neighbourhood_cleansed))
train$street=tolower(gsub(" ", '', train$street))
train$city=as.factor(city_function(train$city))
train$city<-city_levels(train$city)
train$city[which(is.na(train$city))] = "newyork" 
train$property_type=as.factor(property_type_fuction(train$property_type))
train$property_type[which(is.na(train$property_type))]="apartment"

#making unknown zip to 0
train$zipcode <- as.character(train$zipcode)
train$zipcode[which(is.na(train$zipcode))]="0"
train$zipcode <- as.factor(train$zipcode)

# categorical Variables
table(train$host_response_time)
table(train$host_is_superhost)
table(train$host_has_profile_pic)

# only 82 rows are false , so not important variable
train$host_has_profile_pic=NULL
train$None=NULL

table(train$host_identity_verified)
table(train$is_location_exact)


# deleting rows
# in categorical variable 3 rows are empty many palces deleting these rows
train<-train[-which(is.na(train$host_response_time)), ]
train<- train[-which(is.na(train$price)),]


# Filling NA values of all numerical variable  by their mean values
integer_vars <- lapply(train, is.numeric) 
integer_vars<- colnames(train)[unlist(integer_vars)]
for(i in integer_vars){
  
  train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
}

#getting corelation of data and compare graph after imputing values
num.cols <- sapply(train, is.numeric)
cor.data <- cor(train[,num.cols], use="pairwise.complete.obs")
corrplot(cor.data,method='color')



# We are done with data cleaning
#Expanding the training data
trainExpanded <- model.matrix(object=~., data=train)
trainExpanded <- as.data.frame(trainExpanded)
colnames(trainExpanded) <- sapply(colnames(trainExpanded), function(x) sub("^`(.*)`$", "\\1", x))

#Clearning col names for decison tree
colnames(trainExpanded) <- sapply(colnames(trainExpanded), function(x) gsub("\\(", "", x))
colnames(trainExpanded) <- sapply(colnames(trainExpanded), function(x) gsub("\\)", "", x))
colnames(trainExpanded) <- sapply(colnames(trainExpanded), function(x) gsub("\n", "", x))
colnames(trainExpanded) <- sapply(colnames(trainExpanded), function(x) gsub("-", "", x))
colnames(trainExpanded) <- sapply(colnames(trainExpanded), function(x) gsub(" ", "", x))

#Some Graphs
#histogram for guests_included
ggplot(data=train, aes(guests_included)) + 
  geom_histogram(binwidth = 1, 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Guests") +
  labs(x="Guests", y="Count")

train$level=seq(1:nrow(train))

#histogram for price
ggplot(data=train, aes(price)) + 
  geom_histogram(binwidth = 1, 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for price") +
  labs(x="price", y="Count")

#scatter plot for price
ggplot(data=train, aes(x=level, y=price)) + 
  geom_point( col="blue") + 
  labs(title="Scatter plot for Price") +
  labs(x="level", y="price")

#histogram for extra people cost
ggplot(data=train, aes(extra_people)) + 
  geom_histogram(binwidth = 5, 
                 breaks=seq(0, 100, by = 2),
                 col="red", 
                 fill="green", 
                 alpha = 1) + 
  labs(title="Histogram for extra people cost") +
  labs(x="cost", y="Count")

#Scatter plot for extra people cost
ggplot(data=train, aes(x=level, y=extra_people)) + 
  geom_point( col="blue") + 
  labs(title="Scatter plot for extra people cost") +
  labs(x="level", y="extra people cost")

train$level= NULL


#clear memory
rm(amenities)
rm(cor.data)
rm(host_verification)
rm(street)





