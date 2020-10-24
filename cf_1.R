# Reading data



setwd("E:/R_Datasets/")

r_e <- read.csv(file = 'real_estate.csv')
head(r_e)

#### correlation

cor(r_e)


cor(r_e, method = c("pearson", "kendall", "spearman"))

summary(cor(r_e))


install.packages("corrplot")
library(corrplot)

corrplot(cor(r_e, method = c("pearson", "kendall", "spearman")))


library(PerformanceAnalytics)
chart.Correlation(r_e, histogram=TRUE, pch=19)

## HEatmap
summary(r_e)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = r_e, col = col, symm = TRUE)

## simple Linear Regression #### 

lmtemp <- lm(formula = Y.house.price.of.unit.area~X4.number.of.convenience.stores, data = r_e)

plot(lmtemp, pch = 16, col = "blue")

abline(lmtemp)


summary(lmtemp)

plot(lmtemp$residuals, pch = 16, col = "red")



#### Multiple Linear Regression ### 


lmtemp2 <- lm(formula = Y.house.price.of.unit.area~X4.number.of.convenience.stores + 
                X5.latitude + X6.longitude +X2.house.age+
                X3.distance.to.the.nearest.MRT.station+
                No+
                X4.number.of.convenience.stores, data = r_e)

plot(lmtemp2, pch = 16, col = "blue")

abline(lmtemp2)


summary(lmtemp2)

plot(lmtemp2$residuals, pch = 16, col = "red")




##### Experiments ####

AIC(lmtemp2)

### Rmse Calculation ##### 

library(Metrics)
rmse(actual = r_e$Y.house.price.of.unit.area, predicted = lmtemp2$fitted.values)

### histogram of Residuals 

hist(lmtemp2$residuals, color = "grey")

### Z score standardization

r_e$X3.distance.to.the.nearest.MRT.station <- (r_e$X3.distance.to.the.nearest.MRT.station - mean(r_e$X3.distance.to.the.nearest.MRT.station)) / sd(r_e$X3.distance.to.the.nearest.MRT.station)



scaled.r_e <- scale(r_e$X4.number.of.convenience.stores + 
                      r_e$X5.latitude + r_e$X6.longitude +r_e$X2.house.age+
                      r_e$X3.distance.to.the.nearest.MRT.station+
                      r_e$No+
                      r_e$X4.number.of.convenience.stores)



r_e$X5.latitude  <- as.data.frame(scale(r_e$X5.latitude))
r_e$X6.longitude  <- as.data.frame(scale(r_e$X6.longitude))


lm.fit = lm(r_e$Y.house.price.of.unit.area ~ ., data = r_e)
summary(lm.fit)

plot(lmtemp3, pch = 16, col = "blue")

abline(lmtemp3)


summary(lmtemp3)

plot(lmtemp3$residuals, pch = 16, col = "red")
