library(ggplot2)
library(forecast)
library(fma)
library(expsmooth)
library(fpp2)
library(biglm)
library(e1071)

# Sets working directory
setwd("/Users/donier/Desktop/Pollution")

# Reads csv dataset
pollution.data <- read.csv("2014Dataset.csv")

# Sets values
Temperature <- pollution.data$TEMP
PM2.5 <- pollution.data$pm2.5
WindSpeed <- pollution.data$Iws
DewPoint <- pollution.data$DEWP
CumulWindSpeed <- round(pollution.data$Iws) 
# Cumulated Wind Speed, rounds values up or down (0.5 or more rounds up)
No <- pollution.data$No
Pressure <- pollution.data$PRES

# -------------------- 

# Density Plot PM2.5
plot(density(pollution.data$pm2.5,  na.rm = TRUE), main="Density Plot: PM2.5", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(pollution.data$pm2.5)), 2))  # density plot for 'speed'
polygon(density(pollution.data$pm2.5, na.rm = TRUE), col="red")
skewness(PM2.5, na.rm = TRUE)

# Density Plot Temperature
plot(density(pollution.data$DEWP), main="Density Plot: Temperature", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(pollution.data$DEWP), 2)))  # density plot for 'dist'
polygon(density(pollution.data$DEWP), col="red")
skewness(Temperature, na.rm = TRUE)

# ------------------------

# Basic Numeric Analysis
summary(PM2.5)
x <- rnorm(8661, mean=97.73, sd=15)
hist(x, probability=TRUE)
xx <- seq(min(x), max(x), length=100)
lines(xx, dnorm(xx, mean=100, sd=15))



ggplot(data=pollution.data, aes(x=No, y=pm2.5)) + geom_line(stat="identity") + scale_x_continuous(breaks=seq(1,8760,336)) + 
  geom_smooth(method='lm',formula=No ~ pm2.5)

# --------------------------- #

# Linear Regression between Temperature and Dew Point
DewTemp <- read.csv("2014DataSet2Month.csv")
Dew <- DewTemp$DEWP
Temp <- DewTemp$TEMP

linearMod <- lm(Dew ~ Temp, data=DewTemp)  # build linear regression model on full data
print(linearMod)

plot(Temp, Dew, pch = 16, cex = 0.3, col = "black", main = "Temperature plotted against Dew Point Values", xlab = "Temperature", ylab = "Dew Point", abline(-13.8432, 0.1624, col="red"))
# --------------------------- #


# --------------------------- #
# Model Calculations

modelSummary <- summary(linearMod)  # capture model summary as an object
print(modelSummary)

modelCoeffs <- modelSummary$coefficients  # model coefficients

beta.estimate <- modelCoeffs["Temp", "Estimate"]  # Beta Estimate
std.error <- modelCoeffs["Temp", "Std. Error"]  # Standard Error
t_value <- beta.estimate/std.error  # calculate t statistic
print(t_value)

p_value <- 2*pt(-abs(t_value), df=nrow(DewTemp)-ncol(DewTemp))  # calc p Value
f_statistic <- summary(linearMod)$fstatistic  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)


# Pm2.5 correlation
cor(PM2.5, Temperature, use="complete.obs", method="pearson") # calculate correlation between PM2.5 and Temperature
cor(PM2.5, DewPoint, use="complete.obs", method="pearson") # calculate correlation between PM2.5 and Dew Point
cor(PM2.5, CumulWindSpeed, use="complete.obs", method="pearson") # calculate correlation between PM2.5 and Wind Speed
cor(PM2.5, Pressure, use="complete.obs", method="pearson") # calculate correlation between PM2.5 and Wind Speed

scatter.smooth(x=Temperature, y=PM2.5, main="Temperature ~ PM2.5", cex=0.3, pch=1, col="#CCCCCC")  # scatterplot

scatter.smooth(x=DewPoint, y=PM2.5, main="Dew Point ~ PM2.5", cex=0.3, pch=1, col="#CCCCCC")  # scatterplot

scatter.smooth(x=CumulWindSpeed, y=PM2.5, main="Cumulative Wind Speed ~ PM2.5", cex=0.3, pch=1, col="#CCCCCC")  # scatterplot

# Weather Correlation
cor(Pressure, Temperature, use="complete.obs", method="spearman") # calculate correlation between Pressure and Temperature
cor(Pressure, DewPoint, use="complete.obs", method="spearman") # calculate correlation between Pressure and Dew Point
cor(Pressure, CumulWindSpeed, use="complete.obs", method="spearman") # calculate correlation between Pressure and Cumulative Wind Speed

cor(DewPoint, Temperature, use="complete.obs", method="spearman") # calculate correlation between Dew Point and Temperature
cor(DewPoint, CumulWindSpeed, use="complete.obs", method="spearman") # calculate correlation between Dew Point and Cumulative Wind Speed

cor(CumulWindSpeed, Temperature, use="complete.obs", method="spearman") # calculate correlation between Temperature and Cumulative Wind Speed


scatter.smooth(x=Pressure, y=Temperature, main="Temperature ~ Pressure", cex=0.3, pch=1, col="#CCCCCC")  # scatterplot
scatter.smooth(x=DewPoint, y=Pressure, main="Dew Point ~ Pressure", cex=0.3, pch=1, col="#CCCCCC")  # scatterplot
scatter.smooth(x=CumulWindSpeed, y=Pressure, main="Cumulative Wind Speed ~ Pressure", cex=0.3, pch=1, col="#CCCCCC")  # scatterplot
scatter.smooth(x=Temperature, y=DewPoint, main="Temperature ~ Dew Point", cex=0.3, pch=1, col="#CCCCCC")  # scatterplot
scatter.smooth(x=DewPoint, y=CumulWindSpeed, main="Dew Point ~ Cumulative Wind Speed", cex=0.3, pch=1, col="#CCCCCC")  # scatterplot
scatter.smooth(x=CumulWindSpeed, y=Temperature, main="Temperature ~ Cumulative Wind Speed", cex=0.3, pch=1, col="#CCCCCC")  # scatterplot


# Using Spearman's correlation 
cor(PM2.5, Temperature, use="complete.obs", method="spearman") # calculate correlation between PM2.5 and Temperature
cor(PM2.5, DewPoint, use="complete.obs", method="spearman") # calculate correlation between PM2.5 and Dew Point
cor(PM2.5, CumulWindSpeed, use="complete.obs", method="spearman") # calculate correlation between PM2.5 and Wind Speed
cor(Pressure, Temperature, use="complete.obs", method="spearman") # calculate correlation between PM2.5 and Wind Speed

# Using Kendall's correlation 
cor(PM2.5, Temperature, use="complete.obs", method="kendall") # calculate correlation between PM2.5 and Temperature
cor(PM2.5, DewPoint, use="complete.obs", method="kendall") # calculate correlation between PM2.5 and Dew Point
cor(PM2.5, CumulWindSpeed, use="complete.obs", method="kendall") # calculate correlation between PM2.5 and Wind Speed

# Correlation
cor.test(PM2.5, Temperature) # calculate correlation between PM2.5 and Temperature
cor.test(PM2.5, DewPoint) # calculate correlation between PM2.5 and Dew Point
cor.test(PM2.5, CumulWindSpeed) # calculate correlation between PM2.5 and Wind Speed


# PM2.5 Data Summary
summary(PM2.5)

#Standard Deviation
sd(PM2.5, na.rm=TRUE) 
sd(Temperature, na.rm=TRUE) 

# Training 

tempdew <- read.csv("TempDew2.csv") # Using dataset containing only the two variables involved in linear regression

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(tempdew), 0.16349151137*nrow(tempdew))  # only using the first 2 months of data (16.3% of entire data set)
trainingData <- tempdew[trainingRowIndex, ]  # model training data
testData  <- tempdew[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(DEWP ~ TEMP, data=trainingData)  # build the model
DewPointPred <- predict(lmMod, testData)  # predict distance

summary (lmMod)

actuals_preds <- data.frame(cbind(actuals=tempdew$DEWP, predicteds=DewPointPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
print(correlation_accuracy)
head(actuals_preds)
