#identify your working directory 
getwd()

#import data----
data <- read.csv("./data/data.csv")
head(data)
dim(data)


#summary measures------ 
mean(data$age)
median(data$age)
min(data$age)
max(data$age)
summary(data)

#plots-------
hist(data$age)
boxplot(data$age)
plot(data$age, data$bmi)

##manipulate plots-------
boxplot(data$age, col="lightblue", ylab="Years", main="Boxplot of Age")


#hypothesis testing-----