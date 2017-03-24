#utility functions

library(tm)
library(stringr)

#function to split host_varrification
split<-function(data) {
  
  
  data <- as.character(data)
  data=gsub("\\[", '', data)
  data=gsub("]", '', data)
  data=gsub(",", '', data)
  data=gsub(" ", '', data)
  dataSplit <- strsplit(data, "'")
  allWords <- unique(unlist(dataSplit))
  dtm <- lapply(allWords, function(amenity){
    as.vector(sapply(dataSplit, function(amenities_row) {
      return(amenity %in% amenities_row)
    }))
  })
  out <- do.call(cbind, dtm)
  colnames(out) <- allWords
  return(out)
  
}


#Function to split aminities
spliltAminities <- function(data)  {
  data <- as.character(data)
  data=gsub("\\{", '', data)
  data=gsub("}", '', data)
  data=gsub(" ", '', data)
  data=gsub("\"", "", data)
  dataSplit <-sapply(data, function(x) strsplit(x, ","))
  allWords <- unique(unlist(dataSplit))
  dtm <- lapply(allWords, function(amenity){
    as.vector(sapply(dataSplit, function(amenities_row) {
      return(amenity %in% amenities_row)
    }))
  })
  out <- do.call(cbind, dtm)
  colnames(out) <- allWords
  return(out)
  
}

#Function to split street
splitStreet <- function (data)  {
  
  data <- as.character(data)
  data <- tolower(data)
  dataSplit <- strsplit(data, ",")
  
  streetname = sapply(dataSplit, function(x) {  
    if(length(x)>3) 
    {  
      return (x[1])
    }
    else return (x[1])
    
  }   )
  
  
}

# to convert negative values to zero
to_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}


# function to conert city
city_function <- function (data) {
  
  data <- as.character(data)
  data=gsub("\\(", '', data)
  data=gsub(")", '', data)
  data=gsub(" ", '', data)
  data=gsub(",", '', data)
  data=tolower(data)
  
  return(data)
  
}
  
#function to decrease factor of property_type
property_type_fuction <- function( data)  {
    
  data <- as.character(data)
   
  for (i in 1:length(data) ) {
    
    if (! is.na(data[i])){ 
      
    if (data[i] == 'Boat') 
    {  data[i] <- 'other' }
    else if (data[i] == 'Bungalow')
    {  data[i]='other' }
    else if (data[i] == 'Cabin')
    {  data[i]='other' }
    else if (data[i] == 'Camper/RV')
    {  data[i]='other' }
    else if (data[i] == 'Castle')
    {  data[i]='other' }
    else if (data[i] == 'Chalet')
    {  data[i]='other' }
    else if (data[i] == 'Earth House')
    {  data[i]='other' }
    else if (data[i] == 'Hut')
    {  data[i]='other' }
    else if (data[i] == 'Tent')
    {  data[i]='other' }
    else if (data[i] == 'Villa')
    {  data[i]='other' }
    else if (data[i] == 'Dorm')
    {  data[i]='other' }
    else if (data[i] == 'Other')
    {  data[i]='other' }
  
    }
  }
  
  return(data)
  
}
  
#function to decrease factors of city
city_levels <- function(city)  {
  
  
  city<-as.character(city)
  
  city[city %in% c("bronxnewyorkus", "brookly", "brooklyn", "brooklyn\nan/ll/lol/", "brooklyn\nbrooklyn", "brooklyn\nbrooklynsheepsheadbay", "brooklyn/bushwick","brooklynheights", "brooklynnewyork" , "brooklynny", "brooklynwilliamsburg", "brookyln" ,"brookyn" )]<-"brooklyn"
  city[city %in% c("astoria" , "astoria/lic" ,"astoria/queens", "astorian.y.c." ,"astorianewyork","astoriaqueens", "astoriaqueensnyusa")]<-"astoria"
  city[city %in% c("newyor" ,"newyork" ,"newyorkbh" ,"newyorkbrooklyn" ,"newyorkcity","nuevayork" ,"ny" ,"nyc" ,"nycwoodhavenqueens" ,"my" )] <- "newyork"
  city[city %in% c("??????-????????","?????????","??????","?????????","","atlanticbeach",
                   "auburndale" , "bellerose" , "bklynnybushwick/ridgewood" , 
                   "bx" , "cambriaheight" , "carrollgardensbrooklynnycchelseanewyork" , 
                   "cobblehillbrooklyn" , "crownheights" , "district:eastnewyork" , "eastelmhust" , 
                   "elmhurts" , "elmuhrust" , "flushing/kewgardenshillsforesthills/corona", 
                   "fortgreenebrooklyn" , "forthamilton" , "fushingglendale\nglendale" , "gravesendbrooklyn" , "greatneckgreenpointbrooklynharlem" , 
                   "howardbeach" , "jacksonhgts" , "jacksonhightskensingtonkewgardenhills\nnewyork" , "kewgardensqueens" , "kingsbridgeny" , "kipsbay" , 
                   "littleneck" , "lowereastside" , "mountvernon" , "ozonepark" , "ozoneparkqueens" , "ozonpark" , "richmondhills" , "ridgwood" , 
                   "riverdalebronxny" , "saintalbans" , "saintalbansqueensstaten" , "statenislandnewyorksunnyside." , "unionsquareeastvillagenewyork?????????" , "briarwood" , 
                   "bushwickbrooklyncityislandfloralpark" , "jamaicaqueens" , "manhattannewyork" , "parkchester" , "queensastoria" , "richmondhill" , "rosedale" , "st.albans" , 
                   "valleystream" , "?????????bk" , "eastwilliamsburghollis" , "kewgardens" , "rockawayparkgreenpoint" , "queensvillage" , "whitestone" , "maspeth" , "middlevillage" , 
                   "williamsburg" , "woodhaven" , "yonkers" , "bayside" , "freshmeadows" , "riverdalecorona" , "farrockaway" , "lic" , "rockawaybeach" , "williamsburgbrooklyn" )] <- "other"
  city[city %in% c( "bklynnybushwick/ridgewood" , "carrollgardensbrooklynnyc" , 
                    "chelseanewyorkflushing/kewgardenshillsforesthills/corona" , "fushing" , 
                    "glendale\nglendale" , "greatneckgreenpointbrooklynharlem" , 
                    "jacksonhights" , "kensington" , "kewgardenhills\nnewyork" , "saintalbansqueens" , 
                    "staten" , "statenislandnewyork" , "sunnyside." , "unionsquareeastvillagenewyork" , 
                    "bushwickbrooklyn" , "cityisland" , "floralpark" , "bk" , 
                    "eastwilliamsburghollisrockawaypark" , "greenpoint" ,
                    "riverdalecorona" )] <- "other"
  city[city %in% c( "2" , "bklynny" , "bushwick/ridgewood", "chelseanewyork", "eastwilliamsburg", "hollis", 
                    "flushing/kewgardenshills" , "foresthills/corona" , "greatneck" , "greenpointbrooklyn",
                    "harlem" , " eastwilliamsburg" , " ollis" , "rockawaypark" ) ]<- "other"
  
  city=as.factor(city)
  return(city)
}
    
    
    
#error in regression model    
error <- function(pred, actual) {
  
  results <- cbind(pred, actual) 
  colnames(results) <- c('pred','real')
  results <- as.data.frame(results)
  
  results$pred <- sapply(results$pred,to_zero)
  mse <- mean((results$real-results$pred)^2)
  return(mse)
  
}

#error in guests included regression    
error2 <- function(pred, actual) {
  
  pred<- sapply(pred, function(x) floor(x))
  pred <- sapply(pred, function(x) ifelse(x < 0, 0, x))
  results <- cbind(pred, actual) 
  colnames(results) <- c('pred','real')
  results <- as.data.frame(results)
  mse <- mean((results$real-results$pred)^2)
  return(mse)
  
}

#accuracy in guest included classifier
accuracy <- function(pred, actual) {
  
  pred<- sapply(pred, function(x) round(x))
  pred <- sapply(pred, function(x) ifelse(x < 0, 0, x))
  results <- table(pred, actual) 
  print(results)
  results <- as.matrix(results)
  error_table <- sum(diag(results))/sum(results)
  return(error_table)
  
}
# for strings
tokenizer <- function(string)
{
  return(unlist(strsplit(as.character(string), " ")))
}
  

