library(ISLR2)
library(readxl)
df_c <- read_excel("Columbus.xlsx")
df_c <- df_c[, -40] # Deleting the severerisk column as it has very few data points.
names(df_c)
attach(df_c)

# Variable selection is important when dealing with many variables and relatively few rows (108,40) in this case.

library(glmnet)
#Converting into matrix format
x=model.matrix(brightness ~ . - (Year + Month + Lat + Lon + Occurences + bright_t31) ,df_c, na.action = "na.pass")[,-1]
y=df_c$brightness

#Scaling the parameters
x=scale(x)

df <- cbind(x,y)
par(mfrow = c(1,1))

# Lasso Regression
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda", ylab = "Lasso Coefficients")
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso, ylab = "Test MSE (Columbus)")
lasso.best.lambda=cv.lasso$lambda.min # find the best lambda value corresponding to min cv.error
log(lasso.best.lambda) # best lambda is 0.2
min(cv.lasso$cvm) # min cv.error

predict(fit.lasso, type = "coefficients", s = 0.2)

# Predictors from Lasso are: Dust, SoilMoisture, WindVelocity, Transpire, confidence,
# humidity, precipprob, snowdepth, sealevelpressure, cloudcover, and moonphase.


#Using Denver data as test data.
df_d <- read_excel("Denver.xlsx")
df_d <- df_d[, -40]
names(df_d) 
X=model.matrix(brightness ~ . - (Year + Month + Lat + Lon + Occurences + bright_t31) ,df_d, na.action = "na.pass")[,-1]
Y=df_d$brightness

X=scale(X)

df <- cbind(X,Y)
lasso.pred <- predict(fit.lasso, s = 2, newx = df[, -15])
mean((lasso.pred - Y)^2)
#Actual test MSE is 232 which is not bad! Please note that this model requires lot of tuning.




#Let's perform classification!
# For classification code confidence using threshold of 50% and remove brightness from predictors.
df_c <- read_excel("Columbus_Class.xlsx")
df_c <- df_c[, -40]
names(df_c)
attach(df_c)


# Logistic regression
glm.fit=glm(fire ~ (Dust + SoilMoisture + WindVelocity + Transpire + brightness +
                    humidity + precipprob + snowdepth + sealevelpressure + cloudcover + moonphase), data=df_c,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")#predict response probability 
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,1,0)
table(glm.pred, fire)#confusion table
mean(glm.pred == fire)#classification accuracy of 97% from logistic regression
sum(fire==1)/length(fire) #classification accuracy if we classify every observation to 1

#Basic logistic regression model is giving 97% accuracy. I will use Columbus data as the test set for this model and viceversa.
df_d <- read_excel("Denver_Class.xlsx")
df_d <- df_d[, -40]
names(df_d)
glm.probs=predict(glm.fit, data = df_d[, -16], type="response")#predict response probability using Columbus dataset
glm.pred=ifelse(glm.probs>0.5,1,0)
table(glm.pred, df_d$fire)#confusion table
mean(glm.pred == df_d$fire)# test classification accuracy from logistic regression
#Actual test accuracy is 67% which is good!

# Install and load the pROC package
install.packages("pROC")
library(pROC)
roc_data <- roc(df_c$fire, glm.pred)
plot(roc_data, main = "Columbus Model")

# Validation of problem statement.
# We let the model do the optimal variable selection and as you can see it reflects the real world facts about the two regions.
# By getting pretty good testing results we believe we are going in the right direction.
# Extracting appropriate data is crucial and we were successful in doing so. 


# Future work and recommendations:
# 1) Adding more variables,
# 2) Improving the accuracy of variable selection, and 
# 3) Improving Logistic Regression accuracy.
# Finally, we strongly believe a robust Logistic Regression model will help in understanding wildfire!







