#importing dataset
library(readr)
library(forcats)


prediction <- read_csv("~/R/byte/predict.csv")

# counting percentage NA values in every column of Dataset

# NA values
na_count <-sapply(prediction, function(y) (sum(length(which(is.na(y))))))
na_count

#deleting coulmns that were removed from the training set

prediction$square_feet=NULL
prediction$security_deposit=NULL
prediction$state=NULL
prediction$smart_location=NULL
prediction$market=NULL
#first variable is only id not useful
prediction=prediction[,-1]
prediction$host_has_profile_pic=NULL
prediction$None=NULL


# we can check price, extra people and cleaning fee are character because of $ sign , 
#and host_acceptance_rate, host_response_rate bacause of % sign converting them to numeric

prediction$extra_people=round(as.numeric(gsub("\\$", "", (prediction$extra_people))),0)
prediction$price=round(as.numeric(gsub("\\$", "", (prediction$price))),0)
prediction$cleaning_fee=round(as.numeric(gsub("\\$", "", (prediction$cleaning_fee))),0)
prediction$host_response_rate=as.numeric(gsub("%", "", (prediction$host_response_rate) ))
prediction$host_acceptance_rate=as.numeric(gsub("%", "", (prediction$host_acceptance_rate) ))

#convert zipcode to factor
prediction$zipcode=as.factor(prediction$zipcode)

#Converting all integer to numeric
integer_vars <- lapply(prediction, class) == "integer"
prediction[, integer_vars] <- lapply(prediction[, integer_vars], as.numeric)

#converting  character to factor variable
character_vars <- lapply(prediction, class) == "character"
prediction[, character_vars] <- lapply(prediction[, character_vars], as.factor)

#getting corelation of data
num.cols <- sapply(prediction, is.numeric)
cor.data <- cor(prediction[,num.cols], use="pairwise.complete.obs")
corrplot(cor.data,method='color')


#dividing coulmns host_varification, street and amenities into different columns
host_verification=split(prediction$host_verifications)
host_verification<- data.frame(sapply(data.frame(host_verification), as.factor))

amenities=spliltAminities(prediction$amenities)
amenities <- data.frame(sapply(data.frame(amenities), as.factor))

street<-splitStreet(prediction$street)
street <- data.frame(sapply(data.frame(street), as.factor))


prediction$street=NULL
prediction$amenities=NULL
prediction$host_verifications=NULL

prediction=cbind(prediction, amenities, host_verification, street)

# Now little text converting in columns neighbourhood_cleansed and city
prediction$neighbourhood_cleansed=tolower(gsub(" ", '', prediction$neighbourhood_cleansed))
prediction$street=tolower(gsub(" ", '', prediction$street))
prediction$city=as.factor(city_function(prediction$city))
prediction$city<-city_levels(prediction$city)
prediction$city[which(is.na(prediction$city))] = "newyork" 
prediction$property_type=as.factor(property_type_fuction(prediction$property_type))
prediction$property_type[which(is.na(prediction$property_type))]="apartment"

#making unknown zip to 0
prediction$zipcode <- as.character(prediction$zipcode)
prediction$zipcode[which(is.na(prediction$zipcode))]="0"
prediction$zipcode <- as.factor(prediction$zipcode)

# categorical Variables
table(prediction$host_response_time)
table(prediction$host_is_superhost)
table(prediction$host_has_profile_pic)
table(prediction$host_identity_verified)
table(prediction$is_location_exact)


# deleting rows
# in categorical variable 3 rows are empty many palces deleting these rows
prediction<-prediction[-which(is.na(prediction$host_response_time)), ]

# Filling NA values of all numerical variable  by their mean values
#Taking only common columns from train
index <- which(colnames(prediction) %in% colnames(train))
prediction <- prediction[,index]
integer_vars <- lapply(prediction, is.numeric) 
integer_vars<- colnames(prediction)[unlist(integer_vars)]

#Filling the mean value with the mean value of training
for(i in integer_vars){
  
  prediction[is.na(prediction[,i]), i] <- mean(train[,i], na.rm = TRUE)
}


#Expanding the predictioning data
predictionExpanded <- model.matrix(object=~., data=prediction)
predictionExpanded <- as.data.frame(predictionExpanded)
colnames(predictionExpanded) <- sapply(colnames(predictionExpanded), function(x) sub("^`(.*)`$", "\\1", x))

#Clearning col names for decison tree
colnames(predictionExpanded) <- sapply(colnames(predictionExpanded), function(x) gsub("\\(", "", x))
colnames(predictionExpanded) <- sapply(colnames(predictionExpanded), function(x) gsub("\\)", "", x))
colnames(predictionExpanded) <- sapply(colnames(predictionExpanded), function(x) gsub("\n", "", x))
colnames(predictionExpanded) <- sapply(colnames(predictionExpanded), function(x) gsub("-", "", x))
colnames(predictionExpanded) <- sapply(colnames(predictionExpanded), function(x) gsub(" ", "", x))


#Making the column names for prediction set same as that of training set
orginal_col=colnames(trainExpanded)
predict_col = colnames(predictionExpanded)
columnNotInTest <- orginal_col[which(!(orginal_col %in% predict_col))]

missingColumnData <- matrix(0L, nrow = nrow(prediction), ncol = length(columnNotInTest))
colnames(missingColumnData) <- columnNotInTest
missingColumnData <- as.data.frame(missingColumnData)
predictionExpanded <- cbind(predictionExpanded,missingColumnData)

#Removing varaibles not in training
columnsInTrain <- colnames(predictionExpanded) %in% colnames(trainExpanded)
predictionExpanded <- predictionExpanded[,columnsNotInTrain]

new_d <- select(trainExpanded, -(predict_col))

#Cleaning memory
rm(amenities)
rm(cor.data)
rm(host_verification)
rm(street)





