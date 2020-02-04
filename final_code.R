getwd()
#load the dataset
suicidedata = read.csv("suicidestats.csv")
View(suicidedata)
#to check the first 6 values of the dataset
head(suicidedata)
#to understand the datatypes of the suicidedata
str(suicidedata)

#to generate the summary statistics of suicidedata
library(psych)
describe(suicidedata)

#finding the count of missing values for each variable
colSums(is.na(suicidedata))
#finding the rows which have missing values
complete.cases(suicidedata)
#filling the missing values based on estimated quantile values for suicide_no
suicidedata$suicides_no = apply(data.frame(suicidedata$suicides_no), 2, function(x){x <- replace(x, is.na(x), quantile(x, na.rm=T, p=c(0.50)))})
sum(is.na(suicidedata$suicides_no))
View(suicidedata)
#filling the missing values based on estimated quantile values for population
suicidedata$population = apply(data.frame(suicidedata$population), 2, function(x){x <- replace(x, is.na(x), quantile(x, na.rm=T, p=c(0.50)))})
sum(is.na(suicidedata$population))

#clean data
suicidedata$age = gsub(pattern = "years",replacement = "",x = suicidedata$age)
table(suicidedata$age)
#Making the data more presentable
suicidedata$age = ifelse(suicidedata$age == "5-14 ","Child",
                         ifelse(suicidedata$age == "15-24 ","Young_Adult",
                                ifelse(suicidedata$age== "25-34 ","Adult",
                                       ifelse(suicidedata$age== "35-54 ","Middle-Aged",
                                              ifelse(suicidedata$age == "55-74 ","Senior_Citizen",
                                                     ifelse(suicidedata$age == "75+ ","Old_Citizen","NA"))))))
suicidedata$country = as.factor(suicidedata$country)
suicidedata$year = as.factor(suicidedata$year)
suicidedata$sex = as.factor(suicidedata$sex)
suicidedata$age = as.factor(suicidedata$age)


#checking the outliers
boxplot(suicidedata$suicides_no, xlab = "suicide_no")
boxplot(suicidedata$population, xlab = "population")

#changing the data types to numeric 
suicidedata$suicides_no = as.numeric(suicidedata$suicides_no)
suicidedata$population = as.numeric(suicidedata$population)
#heatmap to plot the correlation between variables
num.data_suicide = suicidedata[complete.cases(suicidedata),] #checking the number of complete cases
num.data_suicide$sex = as.integer(factor(num.data_suicide$sex, labels = 1:length(unique(num.data_suicide$sex))))
num.data_suicide$country = as.integer(factor(num.data_suicide$country, 
                                             labels = 1:length(unique(num.data_suicide$country))))
num.data_suicide$age = as.integer(factor(num.data_suicide$age, labels = 1:length(unique(num.data_suicide$age))))
num.data_suicide$year = as.integer(factor(num.data_suicide$year, labels = 1:length(unique(num.data_suicide$year))))
str(num.data_suicide)
ggplot(suicidedata, aes(x=sex, y=suicides_no)) + stat_summary(fun.y="mean", geom="bar")
#creating a correlation matrix
cormatrix = round(cor(num.data_suicide),2)
library(reshape)
melted_cormatrix <- melt(cormatrix)
library(ggplot2)
#plotting a ggplot for the correlation matrix
ggplot(data = melted_cormatrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#the correlation matrix for the variables 
cormatrix


#plotting a graph between sex and number of suicides
plot(suicidedata$sex,suicidedata$suicides_no, xlab = "sex" , ylab = "suicide_no.")

#### linear modelling
##creating a training model
library(ISLR)
set.seed(152)
sub_data = floor(0.80*nrow(suicidedata))
sub_data
train_data_suicide = sample(seq_len(nrow(suicidedata)), size = sub_data)
train = suicidedata[train_data_suicide,]
test = suicidedata[-train_data_suicide,]
View(train)
lm_model = lm( suicides_no ~ ., data = train) #creating a model with the training dataset
summary(lm_model)
plot(lm_model)
lm_model_test = lm( suicides_no ~ ., data = test) #creating a model with the test dataset
summary(lm_model_test)
plot(lm_model_test)
alias(suicides_no ~., data = train)

library(car)
vif(lm_model) #checking the variable overcorrelatio 
View(suicidedata)

#performing backward selection to find a better model
backwardsuicide = step(lm_model,direction = "backward")
summary(backwardsuicide)

lm_model_3 = lm(suicides_no ~ country + sex + age + population, data = train) #creating a linear model based on backward selection
summary(lm_model_3)

#Gradient boosting optimization technique
install.packages("gbm")
install.packages("xgboost")
install.packages("h2o")


#using the gbm package 
library(gbm)
gbm.fit <- gbm(
  formula = suicides_no ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 1000,
  interaction.depth = 1,
  shrinkage = 0.01,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

summary(gbm.fit)

lm_model_train_new = lm( suicides_no ~ sex + population + country +age, data = train) 
summary(lm_model_train_new)

#using the gbm package 
library(gbm)
gbm.fit_3 <- gbm(
  formula = suicides_no ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.01,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
summary(gbm.fit_3)
lm_model_train_new_1 = lm( suicides_no ~ population + country +age +sex +year, data = train)
summary(lm_model_train_new_1)

#Using the sqaure-root transformation to reduce the skewness along with the significant varibales observed from gbm

lm_model_final = lm(sqrt(suicides_no) ~ population + country +age +sex , data = train)
summary(lm_model_final)
#optimized linear model for test dataset
lm_model_final_test = lm(sqrt(suicides_no) ~ population + country +age +sex , data = test)
summary(lm_model_final_test) 

plot(lm_model_final_test)

library(car)
vif(lm_model_final_test) #checking the multi-collinearity in the model 

##creating new observations for predict function
Qatar_pred = data.frame(country = "Qatar" , sex = "female" , age = "Adult" , population = 87644)
suicide.qatar = predict(lm_model_final_test, Qatar_pred)
(suicide.qatar)^2
#creating new observations for predict function
Spain_pred = data.frame(country = "Spain" , sex = "male" , age = "Middle-Aged" , population = 1873657)
suicide.spain = predict(lm_model_final_test, Spain_pred)
suicide.spain
(suicide.spain)^2

plot(suicidedata$country,suicidedata$suicides_no, xlab = "country" , ylab = "suicide_no.")

#exporting the clean dataset to tableau for better visualizations
write.csv(suicidedata,'C:\\Users\\shivi\\Desktop\\newdata.csv', row.names = FALSE)
