setwd("E:/R_Datasets/")

df <- read.csv(file = 'insurance.csv')
head(df)

df$smoker <- as.numeric(df$smoker)
df$region <- as.numeric(df$region)


df$age <- as.numeric(df$age)
df$children <- as.numeric(df$children)

df_new <- df
df2 <- df_new[ -c(2) ]

df2$bmi <- df2$bmi - mean(df2$bmi)

#### Correlation Calculation ##### 

cor(df2,method = c("pearson","kendall","spearman"))

summary(cor(df2,method = c("pearson","kendall","spearman")))

install.packages("corrplot")
library(corrplot)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(df2, col=col, symm=TRUE)


library(ggcorrplot)


#### Train_test Data split



# Random sample indexes
train_index <- sample(1:nrow(df2), 0.90 * nrow(df2))
test_index <- setdiff(1:nrow(df2), train_index)

# Build X_train, y_train, X_test, y_test
X_train <- df2[train_index, -15]
y_train <- df2[train_index, "charges"]

X_test <- df2[test_index, -15]
y_test <- df2[test_index, "charges"]



x2_train <- X_train[1:5]
y2_train <- X_train[6]

X2_test <- X_test[1:5]
y2_test <- X_test[6]


### Linear Regression ##### 



model_lm <- lm(formula = y2_train$charges~x2_train$age +
                 x2_train$bmi + x2_train$children + x2_train$smoker + x2_train$region)

plot(model_lm, pch = 20, col = "blue")

abline(model_lm)


summary(model_lm)


### plot Residuals #### 

plot(model_lm$residuals, pch = 16, col = "red")

print(model_lm$coefficients)

print(model_lm$rank)

print(model_lm$fitted.values)

AIC(model_lm) 

BIC(model_lm) 
#### Prediction and accuracy #####

y_pred <- predict(model_lm, X_test)

actuals_preds <- data.frame(cbind(actuals=y_test, predicteds=y_pred))

## correlation accuracy ####
correlation_accuracy <- cor(actuals_preds)

print(correlation_accuracy)

head(actuals_preds)

#### min_max _accuracy ####### 
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  

print(min_max_accuracy)


#### Distribution of Y_pred ### 


hist(model_lm$residuals, color = "grey")

summary(model_lm)











model_lm <- lm(formula = y2_train$charges~x2_train$age +
                 x2_train$bmi + x2_train$children + x2_train$smoker + x2_train$region,
               subset=(1:length(height)!=(50,60,70,80,80,100)))

plot(model_lm, pch = 20, col = "blue")

abline(model_lm)


summary(model_lm)



#### Improvement of Residuals ###### 

resids = model_lm$residuals

trans <- log10(resids-1.0001*min(resids))
qqnorm(trans)
qqline(trans)


plot(trans, pch = 20, col = "blue")

hist(trans, color = "grey")
