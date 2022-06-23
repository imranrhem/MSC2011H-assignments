# ASSIGNMENT 3  - BTC1859H
# IMRAN RHEMTULLA
# JUNE 23, 2022

setwd("C://Users/Ronny/iCloudDrive/Mbiotech/BTC1859H/Assignments/Assignment3")

library(UsingR)

babies_df <- babies

######QUESTION 1######

# Replacement of weights = 999 with NA according to documents provided with data

babies_df$wt[babies_df$wt == 999] <- NA
babies_df$wt1[babies_df$wt1 == 999] <- NA
# Scatterplot of mother's weight (lbs) and baby's birth weight(oz)


plot(babies_df$wt1, babies_df$wt, xlab = "Prepregnancy Weight of Mother (lbs)", 
     ylab = "Birth Weight of Baby (oz)", font.lab = 2, cex.lab = 1.5, cex.main = 2,
     main = "Mother's Prepregnancy Weight versus Baby's Birth Weight",
     ylim = c(50,190))

######QUESTION 2######

# Linear regression model of baby's birth weight and mother's prepregnancy weight

weight_model <- lm(wt~wt1, data = babies_df)
summary(weight_model)

######QUESTION 3######

# Creation of data using the variable for mothers weight

msX_weight <- data.frame(wt1 = c(160))

# Prediction of expected value using the prediction interval the measurement involves a new subject not already in the data of the model

predict(weight_model, newdata = msX_weight, int = "p")

######QUESTION 4######

# Creation of data using the variable for mothers weight

obste_value <- data.frame(wt1 = c(160))

# Prediction of expected value using the confidence interval of the model 

predict(weight_model, newdata = obste_value, int = "c")

######QUESTION 6######

cor.test(babies_df$wt, babies_df$wt1, use = "complete.obs", method = "pearson")

