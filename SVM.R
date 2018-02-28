library(e1071)
library(sqldf)

#Input files
input_file <- read.csv("final_boys_modeling_days_bet_races2.csv")
input_test_1 <- read.csv("Book1.csv")


#Converting required factors to numeric
input_file$ELEVATION_num <- as.numeric(as.character(input_file$ELEVATION))
input_file$Temp_Celcius_num <- as.numeric(as.character(input_file$Temp_Celcius))
input_file$Dew_Point_Celc_num <- as.numeric(as.character(input_file$Dew_Point_Celc))

input_test_1$ELEVATION_num <- as.numeric(as.character(input_test_1$ELEVATION))
input_test_1$Temp_Celcius_num <- as.numeric(as.character(input_test_1$Temp_Celcius))
input_test_1$Dew_Point_Celc_num <- as.numeric(as.character(input_test_1$Dew_Point_Celc))


#Transforming and using ZStandardisation
input_file$ELEVATION_transform <- (input_file$ELEVATION_num - mean(input_file$ELEVATION_num, na.rm = TRUE))/
  sd(input_file$ELEVATION_num, na.rm = TRUE)
input_file$Temp_Celcius_transform <- (input_file$Temp_Celcius_num - mean(input_file$Temp_Celcius_num, na.rm = TRUE))/
  sd(input_file$Temp_Celcius_num, na.rm = TRUE)
input_file$Dew_Point_Celc_transform <- (input_file$Dew_Point_Celc_num - mean(input_file$Dew_Point_Celc_num, na.rm = TRUE))/
  sd(input_file$Dew_Point_Celc_num, na.rm = TRUE)


input_test_1$ELEVATION_transform <- (input_test_1$ELEVATION_num - mean(input_test_1$ELEVATION_num, na.rm = TRUE))/
  sd(input_test_1$ELEVATION_num, na.rm = TRUE)
input_test_1$Temp_Celcius_transform <- (input_test_1$Temp_Celcius_num - mean(input_test_1$Temp_Celcius_num, na.rm = TRUE))/
  sd(input_test_1$Temp_Celcius_num, na.rm = TRUE)
input_test_1$Dew_Point_Celc_transform <- (input_test_1$Dew_Point_Celc_num - mean(input_test_1$Dew_Point_Celc_num, na.rm = TRUE))/
  sd(input_test_1$Dew_Point_Celc_num, na.rm = TRUE)

#Discarding NAs
aaa <- subset(input_file, is.na(ELEVATION_num) == FALSE)
aaa_tr <- subset(input_test, is.na(ELEVATION_num) == FALSE)

training_data <- aaa[1:58892,c(8,33,35,39,40,41)]
training_data

test_test <- input_test_1[,c(33,35,39,40,41)]


#TRAINING
#remove target from x for pred
#y has the actual time
# full training_data will be used to build model
# x will be run to evaluate model (without time_sec)

x <- subset(training_data, select=-time_sec)
y <- training_data$time_sec


#
#this is training on target time_sec with full training data including
#time_sec column to build model
our_svm_model <- svm(time_sec ~., data = training_data)
summary(our_svm_model)

#now to look at prediction of model using test set
pred <- predict(our_svm_model, test_test)


#Calculating the MAPE error
mape <- function(actual,pred)
{
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

mape_error <- mape(input_test_1$time_sec,pred)
mape_error
pred

plot(input_test_1$time_sec,pred)
