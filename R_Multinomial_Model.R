## Project Obesity

# data loading
df <- read.csv('./ObesityDataSet_raw_and_data_sinthetic.csv')

# changing obestity level into 4 category
df$NObeyesdad <- replace(df$NObeyesdad, df$NObeyesdad=='Normal_Weight', 'Normal')
df$NObeyesdad <- replace(df$NObeyesdad, df$NObeyesdad=='Insufficient_Weight', 'Insuficient')
df$NObeyesdad <- replace(df$NObeyesdad, df$NObeyesdad=='Overweight_Level_I', 'Overweight')
df$NObeyesdad <- replace(df$NObeyesdad, df$NObeyesdad=='Overweight_Level_II', 'Overweight')
df$NObeyesdad <- replace(df$NObeyesdad, df$NObeyesdad=='Obesity_Type_I', 'Obesity')
df$NObeyesdad <- replace(df$NObeyesdad, df$NObeyesdad=='Obesity_Type_II', 'Obesity')
df$NObeyesdad <- replace(df$NObeyesdad, df$NObeyesdad=='Obesity_Type_III', 'Obesity')

# 1. EDA
# we found the FCVC, NCP, CH2O, FAF and TUE are not categories
summary(df)

# the mentioned columns has been imputed by original author
# we need to impute into category data type by round
df$FCVC <- round(df$FCVC, digits=0)
df$NCP <- round(df$NCP, digits=0)
df$CH2O <- round(df$CH2O, digits=0)
df$FAF <- round(df$FAF, digits=0)
df$TUE <- round(df$TUE, digits=0)


# changing data type
df$Gender <- factor(df$Gender)
df$family_history_with_overweight <- factor(df$family_history_with_overweight)
df$FAVC <- factor(df$FAVC)
#df$FCVC <- factor(df$FCVC)
#df$NCP <- factor(df$NCP)
df$CAEC <- factor(df$CAEC)
df$SMOKE <- factor(df$SMOKE)
#df$CH2O <- factor(df$CH2O)
df$SCC <- factor(df$SCC)
#df$FAF <- factor(df$FAF)
#df$TUE <- factor(df$TUE)
df$CALC <- factor(df$CALC)
df$MTRANS <- factor(df$MTRANS)
df$NObeyesdad <- factor(df$NObeyesdad)


# Plot numeric va NObeyesdad
plot(df$Gender, df$NObeyesdad)
plot(df$Age, df$NObeyesdad)
plot(df$family_history_with_overweight, df$NObeyesdad)
plot(df$FAVC, df$NObeyesdad)
plot(df$FCVC, df$NObeyesdad)
plot(df$NCP, df$NObeyesdad)
plot(df$CAEC, df$NObeyesdad)
plot(df$SMOKE, df$NObeyesdad)
plot(df$CH2O, df$NObeyesdad)
plot(df$SCC, df$NObeyesdad)
plot(df$FAF, df$NObeyesdad)
plot(df$TUE, df$NObeyesdad)
plot(df$CALC, df$NObeyesdad)
plot(df$MTRANS, df$NObeyesdad)


# plotting
# par(mfrow=c(4,5))
plot(df$Gender, main='Gender')
hist(df$Age, main='Age')
hist(df$Height, main='Height')
hist(df$Weight, main='Weight')
plot(df$family_history_with_overweight, main='Family History with Overweight')
plot(df$FAVC, main='Consume high-calorie foods frequently')
plot(df$FCVC, main='Number of meals where you usually eat vegetables')
plot(df$NCP, main='Number of main meals a day')
plot(df$CAEC, main='Eat food between meals')
plot(df$SMOKE, main='Smoke')
plot(df$CH2O, main='Liters of water you drink a day')
plot(df$SCC, main='Monitor the calories you consume daily')
plot(df$FAF, main='Frequency of days per week that you often have physical activity')
plot(df$TUE, main='Time of use of technological devices on a daily basis')
plot(df$CALC, main='Frequency of alcohol intake')
plot(df$MTRANS, main='Means of transportation that you use regularly')
plot(df$NObeyesdad, main='Level of obesity')


# train-test spilt
set.seed(1)
sample <- sample.int(n = nrow(df), size = floor(.80*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]


# multinom
# full model
# df <- subset(df, select=-c(Height,Weight))

library(nnet)
mod_multinom <- multinom(NObeyesdad ~ . - Weight - Height , data=train, maxit=200)
summary(mod_multinom)

predict <- predict(mod_multinom, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accracy: ',accuracy))


# stepwise
x <- step(mod_multinom, test='F')
summary(x)

pchisq(deviance(x)-deviance(mod_multinom), mod_multinom$edf - x$edf, 
       lower.tail=FALSE)

summary(mod_multinom)

# reduced model 1 : Gender
mod_multinom_r1 <- multinom(NObeyesdad ~ . - Height - Weight - Gender, 
                            data=train, maxit=200)
summary(mod_multinom_r1)

predict <- predict(mod_multinom_r1, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r1) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r1$edf, lower.tail=FALSE)


# reduced model 2 : CH2O
mod_multinom_r2 <- multinom(NObeyesdad ~ . - Height - Weight - CH2O, 
                            data=train, maxit=200)
summary(mod_multinom_r2)

predict <- predict(mod_multinom_r2, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r2) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r2$edf, lower.tail=FALSE)


# reduced model 3 : FAF
mod_multinom_r3 <- multinom(NObeyesdad ~ . - Height - Weight - FAF, 
                            data=train, maxit=200)
summary(mod_multinom_r3)

predict <- predict(mod_multinom_r3, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r3) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r3$edf, lower.tail=FALSE)


# reduced model 4 : TUE
mod_multinom_r4 <- multinom(NObeyesdad ~ . - Height - Weight - TUE, 
                            data=train, maxit=200)
summary(mod_multinom_r4)

predict <- predict(mod_multinom_r4, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r4) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r4$edf, lower.tail=FALSE)

# reduced model 5 : CALC
mod_multinom_r5 <- multinom(NObeyesdad ~ . - Height - Weight - CALC, 
                            data=train, maxit=200)
summary(mod_multinom_r5)

predict <- predict(mod_multinom_r5, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r5) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r5$edf, lower.tail=FALSE)

# reduced model 6 : SMOKE
mod_multinom_r6 <- multinom(NObeyesdad ~ . - Height - Weight - SMOKE, 
                            data=train, maxit=200)
summary(mod_multinom_r6)

predict <- predict(mod_multinom_r6, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r6) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r6$edf, lower.tail=FALSE)

# reduced model 7 : SCC
mod_multinom_r7 <- multinom(NObeyesdad ~ . - Height - Weight - SCC, 
                            data=train, maxit=200)
summary(mod_multinom_r7)

predict <- predict(mod_multinom_r7, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r7) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r7$edf, lower.tail=FALSE)

# reduced model 8 : NCP
mod_multinom_r8 <- multinom(NObeyesdad ~ . - Height - Weight - NCP, 
                            data=train, maxit=200)
summary(mod_multinom_r8)

predict <- predict(mod_multinom_r8, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r8) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r8$edf, lower.tail=FALSE)

# reduced model 9 : FCVC
mod_multinom_r9 <- multinom(NObeyesdad ~ . - Height - Weight - FCVC, 
                            data=train, maxit=200)
summary(mod_multinom_r9)

predict <- predict(mod_multinom_r9, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r9) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r9$edf, lower.tail=FALSE)

# reduced model 10 : MTRANS
mod_multinom_r10 <- multinom(NObeyesdad ~ . - Height - Weight - MTRANS, 
                            data=train, maxit=200)
summary(mod_multinom_r10)

predict <- predict(mod_multinom_r10, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r10) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r10$edf, lower.tail=FALSE)

# reduced model 11 : FAVC
mod_multinom_r11 <- multinom(NObeyesdad ~ . - Height - Weight - FAVC, 
                             data=train, maxit=200)
summary(mod_multinom_r11)

predict <- predict(mod_multinom_r11, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r11) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r11$edf, lower.tail=FALSE)

# reduced model 12 : Age
mod_multinom_r12 <- multinom(NObeyesdad ~ . - Height - Weight - Age, 
                             data=train, maxit=200)
summary(mod_multinom_r12)

predict <- predict(mod_multinom_r12, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r12) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r12$edf, lower.tail=FALSE)

# reduced model 13 : family_history_with_overweight
mod_multinom_r13 <- multinom(NObeyesdad ~ . - Height - Weight - Mfamily_history_with_overweight, 
                             data=train, maxit=200)
summary(mod_multinom_r13)

predict <- predict(mod_multinom_r13, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r13) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r13$edf, lower.tail=FALSE)

# reduced model 14 : CAEC
mod_multinom_r14 <- multinom(NObeyesdad ~ . - Height - Weight - CAEC, 
                             data=train, maxit=200)
summary(mod_multinom_r14)

predict <- predict(mod_multinom_r14, test)
actual <- test$NObeyesdad
accuracy <- mean(actual == predict)
print(paste('accuracy: ',accuracy))

pchisq(deviance(mod_multinom_r14) - deviance(mod_multinom), 
       mod_multinom$edf - mod_multinom_r14$edf, lower.tail=FALSE)

