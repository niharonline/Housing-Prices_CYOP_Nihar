##initiating required packages and libraries.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

install.packages("naniar")
install.packages("corrplot")

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(naniar)
library(corrplot)

##Importing Bangalore Data set

urlbangalore = "https://raw.githubusercontent.com/niharonline/Housing-Prices_CYOP_Nihar/main/Bangalore.csv"
Bangalore<-read_csv(url(urlbangalore))

##Importing Chennai Data set

urlchennai = "https://raw.githubusercontent.com/niharonline/Housing-Prices_CYOP_Nihar/main/Chennai.csv"
Chennai<-read_csv(url(urlchennai))


## similarly importing the data set for other cities
urldelhi = "https://raw.githubusercontent.com/niharonline/Housing-Prices_CYOP_Nihar/main/Delhi.csv"

urlhyderabad = "https://raw.githubusercontent.com/niharonline/Housing-Prices_CYOP_Nihar/main/Hyderabad.csv"

urlkolkata = "https://raw.githubusercontent.com/niharonline/Housing-Prices_CYOP_Nihar/main/Kolkata.csv"

urlmumbai = "https://raw.githubusercontent.com/niharonline/Housing-Prices_CYOP_Nihar/main/Mumbai.csv"

Delhi<-read_csv(url(urldelhi))
Hyderabad<-read_csv(url(urlhyderabad))
Kolkata<-read_csv(url(urlkolkata))
Mumbai<-read_csv(url(urlmumbai))



#adding respective cities to all the data sets.
# we will create a column city which will mention the city for the respective entry.

Bangalore$city <- "Bangalore"
Chennai$city <- "Chennai"
Delhi$city <- "Delhi"
Hyderabad$city <- "Hyderabad"
Kolkata$city <- "Kolkata"
Mumbai$city <- "Mumbai"



#combining the six data sets.

Housing_data <- rbind(Bangalore, Chennai, Delhi, Hyderabad, Kolkata, Mumbai)
head(Housing_data)
str(Housing_data)

colnames(Housing_data)[4] <- "Bedrooms"

## here we replace the entries with value 9 with N/A

Housing_data <- Housing_data %>% replace_with_na_all(condition = ~.x == 9)
summary(Housing_data)

## seperating column names in a data frame
column_all <- colnames(Housing_data) 
column_all

## only retaining columns with categorical variables.
column_factors <- column_all[-c(1,2,3)]


## transforming columns to categorical variables.
Housing_data[,column_factors] <- lapply(Housing_data[,column_factors] , factor)

str(Housing_data) 


##to add the ppa variable we use the cbind function

Housing_data <- cbind(Housing_data, PPA = Housing_data$Price/Housing_data$Area)


## Plotting the PPA values
ggplot(Housing_data, aes(x=PPA)) + geom_histogram(bins=100, aes(y=..density..), colour="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666")



##running a log transform on the data set.

Housing_data <- cbind(Housing_data, log_PPA = log(Housing_data$PPA))

## Plotting the log PPA values

ggplot(Housing_data, aes(x=log_PPA)) + geom_histogram(bins=100, aes(y=..density..), colour="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666")

## boxplot of the PPA values in the housing_data set to identify outliers
boxplot(Housing_data$log_PPA)

##we will add a column with the z_scores of the log_PPA values

Housing_data <- cbind(Housing_data, z_scores = (Housing_data$log_PPA - mean(Housing_data$log_PPA))/sd(Housing_data$log_PPA))


## we use the below code to eliminate the NA.

Housing_data_clean <- na.omit(Housing_data)


##removing the na entry from the larger data set

Housing_data <- Housing_data %>% drop_na('No. of Bedrooms')


## partitioning the whole data

##data partition

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = Housing_data$log_PPA, times = 1, p = 0.1, list = FALSE)
edx <- Housing_data[-test_index,]
temp <- Housing_data[test_index,]

# ensuring that the city and locations are covered in the validation set;
validation <- temp %>% semi_join(edx, by = "city") %>% semi_join(edx, by = "Location")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)



##exploratory analysis of the data at hand. 

head(edx)
str(edx)
summary(edx)



## plot of the price distribution by city

city_sum <- edx %>% group_by(city) %>% summarize(PPA = mean(PPA))
city_sum <- city_sum[order(-city_sum$PPA),]

barplot(city_sum$PPA, names.arg=city_sum$city,  xlab="Cities",ylab="Average price per unit area", col="light blue", main="Price Variation by City",border="black")


##creating city contrast matrix

Contrast_matrix_city <- model.matrix(~city, data = Housing_data_clean)

##adding the contrast matrix to the data set

Housing_data_clean <- cbind(Housing_data_clean, Contrast_matrix_city[,-1])

##creating bedroom contrast matrix

Contrast_matrix_bedroom <- model.matrix(~Bedrooms, data = Housing_data_clean)

##adding the contrast matrix to the data set

Housing_data_clean <- cbind(Housing_data_clean, Contrast_matrix_bedroom[,-1])


##creating the variable set data frame

column_all <- colnames(Housing_data_clean)
column_all



##We dont need the columns with price, area, location, city, ppa and z scores, there are no 7 bedroom homes hence bedrooms7 is also removed. 

column_variables <- column_all[-c(1,2,3,4,41,42,44,55)]
column_variables

##we now create the variable set
Variables_set <- Housing_data_clean[,column_variables]

## transforming the variable set to integer.
Variables_set[] <- lapply(Variables_set,as.integer)



##calculating the correlation.

Variables_set.cor <- cor(Variables_set)

##creating a heatmap of the data

palette = colorRampPalette(c("green", "white", "red")) (20)

heatmap(x = Variables_set.cor, col = palette, symm = TRUE)


## if the sum of individual furniture is 9 (since absence is marked by integer 1) or more, we will place 1 in the column, else it will be zero.
Variables_set <- cbind(Variables_set, Furniture_sum = Variables_set$BED + Variables_set$Microwave +Variables_set$TV + Variables_set$DiningTable + Variables_set$Sofa + Variables_set$WashingMachine + Variables_set$Gasconnection + Variables_set$Wardrobe + Variables_set$Refrigerator + Variables_set$Wifi)


##replacing furniture with a categorical value

Variables_set$furniture <- ifelse(Variables_set$Furniture_sum > 10, 1, 0)


## if the sum of individual furniture is 9 (since absence is marked by integer 1) or more, we will place 1 in the column, else it will be zero.
Variables_set <- cbind(Variables_set, Feature_sum = Variables_set$GolfCourse+ Variables_set$Cafeteria + Variables_set$ATM + Variables_set$ShoppingMall + Variables_set$Hospital + Variables_set$School + Variables_set$LiftAvailable + Variables_set$PowerBackup + Variables_set$Intercom + Variables_set$SportsFacility + Variables_set$VaastuCompliant + Variables_set$StaffQuarter + Variables_set$RainWaterHarvesting + Variables_set$CarParking + Variables_set$`24X7Security`+ Variables_set$MultipurposeRoom + Variables_set$IndoorGames + Variables_set$LandscapedGardens + Variables_set$JoggingTrack + Variables_set$`Children'splayarea`+Variables_set$ClubHouse + Variables_set$Gymnasium + Variables_set$SwimmingPool)


##replacing furniture with a categorical value

Variables_set$feature <- ifelse(Variables_set$Feature_sum > 25 , 1, 0)


## creating a variable set with limited variables

lim_column <- c("Resale", "feature", "furniture", "log_PPA", "cityChennai", "cityDelhi", "cityHyderabad", "cityKolkata", "cityMumbai", "Bedrooms2", "Bedrooms3", "Bedrooms4", "Bedrooms5", "Bedrooms6", "Bedrooms8")

new_set <- as.data.frame(Variables_set[,lim_column])


##checking the correlation in the limited set

new_set.cor <- cor(new_set)

##creating a heatmap of the data

palette = colorRampPalette(c("green", "white", "red")) (20)

heatmap(x = new_set.cor, col = palette, symm = TRUE)

##adding the furniture and feature column to the housing data. 

Housing_data_clean <- cbind(Housing_data_clean, furniture = Variables_set$furniture)

Housing_data_clean <- cbind(Housing_data_clean, feature = Variables_set$feature)

Housing_data_clean$Resale <- as.numeric(Housing_data_clean$Resale)





##partitioning the clean data set

##data partition

set.seed(1, sample.kind="Rounding")
test_index_clean <- createDataPartition(y = Housing_data_clean$log_PPA, times = 1, p = 0.1, list = FALSE)
edx_clean <- Housing_data_clean[-test_index_clean,]
temp_clean <- Housing_data_clean[test_index_clean,]

# ensuring that the city and locations are covered in the validation set;
validation_clean <- temp_clean %>% semi_join(edx_clean, by = "city") %>% semi_join(edx_clean, by = "Location")

# Add rows removed from validation set back into edx set
removed_clean <- anti_join(temp_clean, validation_clean)
edx_clean <- rbind(edx_clean, removed_clean)


#writing a linear regression model with only cities

model_1 <- lm(log_PPA~cityChennai+ cityDelhi+ cityHyderabad+ cityKolkata + cityMumbai, data = edx_clean)
summary(model_1)

##saving rsquare in a table

Rsqr_model1 <- summary(model_1)$r.squared
Rsqr_table <- tibble(method="Model 1: cities", R_sqr = Rsqr_model1)
Rsqr_table

#writing a linear regression model with only cities

model_2 <- lm(log_PPA~cityChennai+ cityDelhi+ cityHyderabad+ cityKolkata + cityMumbai + Bedrooms2 + Bedrooms3 + Bedrooms4 + Bedrooms5 + Bedrooms6 + Bedrooms8, data = edx_clean)
summary(model_2)


##saving rsquare in a table

Rsqr_model2 <- summary(model_2)$r.squared
Rsqr_table <- bind_rows(Rsqr_table, data_frame(method="Model 2: cities + Bedroom", R_sqr = Rsqr_model2))
Rsqr_table

#writing a linear regression model with only cities

model_3 <- lm(log_PPA~cityChennai+ cityDelhi+ cityHyderabad+ cityKolkata + cityMumbai + Bedrooms2 + Bedrooms3 + Bedrooms4 + Bedrooms5 + Bedrooms6 + Bedrooms8 + Resale, data = edx_clean)
summary(model_3)

##saving rsquare in a table

Rsqr_model3 <- summary(model_3)$r.squared
Rsqr_table <- bind_rows(Rsqr_table, data_frame(method="Model 3: cities + Bedroom + Resale", R_sqr = Rsqr_model3))
Rsqr_table

#writing a linear regression model with only cities

model_4 <- lm(log_PPA~cityChennai+ cityDelhi+ cityHyderabad+ cityKolkata + cityMumbai + Bedrooms2 + Bedrooms3 + Bedrooms4 + Bedrooms5 + Bedrooms6 + Bedrooms8 + Resale + furniture, data = edx_clean)
summary(model_4)

##saving rsquare in a table

Rsqr_model4 <- summary(model_4)$r.squared
Rsqr_table <- bind_rows(Rsqr_table, data_frame(method="Model 4: cities, bedrooms, resale + furniture", R_sqr = Rsqr_model4))
Rsqr_table




#writing a linear regression model with only cities

model_5 <- lm(log_PPA~cityChennai+ cityDelhi+ cityHyderabad+ cityKolkata + cityMumbai + Bedrooms2 + Bedrooms3 + Bedrooms4 + Bedrooms5 + Bedrooms6 + Bedrooms8 + Resale + furniture + feature, data = edx_clean)
summary(model_5)

##saving rsquare in a table

Rsqr_model5 <- summary(model_5)$r.squared
Rsqr_table <- bind_rows(Rsqr_table, data_frame(method="Model 5: cities, bedroom, resale, furniture, feature", R_sqr = Rsqr_model5))
Rsqr_table






## we first predict the the value

edx_clean <- cbind(edx_clean, prediction = 0.159*edx_clean$cityChennai + 0.206*edx_clean$cityDelhi - 0.0332*edx_clean$cityHyderabad - 0.2221*edx_clean$cityKolkata + 0.657*edx_clean$cityMumbai + 0.101*edx_clean$Bedrooms2 + 0.269*edx_clean$Bedrooms3 + 0.542*edx_clean$Bedrooms4 + 0.78*edx_clean$Bedrooms5 + 0.2147*edx_clean$Bedrooms6 + 0.3729*edx_clean$Bedrooms8 + 0.104*edx_clean$Resale + 0.1536*edx_clean$furniture + 0.12069*edx_clean$feature )




##RMSE crossvalidation

RMSE_cv <- RMSE(edx_clean$log_PPA, edx_clean$prediction)

RMSE_cv

exp(RMSE_cv)


## we first predict the the value

validation_clean <- cbind(validation_clean, prediction = 0.159*validation_clean$cityChennai + 0.206*validation_clean$cityDelhi - 0.0332*validation_clean$cityHyderabad - 0.2221*validation_clean$cityKolkata + 0.657*validation_clean$cityMumbai + 0.101*validation_clean$Bedrooms2 + 0.269*validation_clean$Bedrooms3 + 0.542*validation_clean$Bedrooms4 + 0.78*validation_clean$Bedrooms5 + 0.2147*validation_clean$Bedrooms6 + 0.3729*validation_clean$Bedrooms8 + 0.104*validation_clean$Resale + 0.1536*validation_clean$furniture + 0.12069*validation_clean$feature )





RMSE_v <- RMSE(validation_clean$log_PPA, validation_clean$prediction)

RMSE_v



#################### RMSE VALIDATION ###################################

exp(RMSE_v)





