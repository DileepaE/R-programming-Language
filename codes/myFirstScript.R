#Import data------
data <- read.csv("D:/OneDrive - Lancaster University/github/R-programming-Language/data/data.csv")
str(data)
head(data)
tail(data)


#descriptive analysis---- 
mean(data$age )
sd(data$age)
summary(data)
table(data$sex)
table(data$chd)

#plots--------------
hist(data$age)
hist(data$age, main="Histogram of Age", xlab = "Age", col="lightblue")
boxplot(data$age, main="Histogram of Age", ylab = "Age (years)", col="lightblue")
plot(data$sbp, data$chol )

table(data$sex)

par(mfrow=c(1,2))
plot(data$sbp[data$sex==1], data$chol[data$sex==1] )
plot(data$sbp[data$sex==2], data$chol[data$sex==2] )


par(mfrow=c(1,1))
plot(data$sbp[data$sex==1], data$chol[data$sex==1], col="blue" )
points(data$sbp[data$sex==2], data$chol[data$sex==2], col="green" )

# For nice graphs
# install.packages("ggplot2")
# library(ggplot2)


#line graph
plot(data$sbp[data$sex==1], data$chol[data$sex==1], col="blue",type="l" )
plot(data$sbp[1:10], data$chol[1:10], col="blue",type="l" )


x<-1:10
x
y <- 2*x + 5
y
plot(x,y,type="l")

y2 <- x^2
y2

plot(x,y2,type="l" )

y3 <- x^3
y3
plot(x,y3,type="l" )
lines(x,y2,type="l" ,add=T, col="green")
lines(x,y,type="l" ,add=T, col="blue")


#Hypothesis testing--------------
#correlation
plot(data$bmi, data$chol)
cor.test(data$bmi, data$chol)


#t test
density(data$sbp[data$sex==2])
density(data$sbp[data$sex==1])

plot(density(data$sbp[data$sex==1]),col="red")
lines(density(data$sbp[data$sex==2]), col="blue")

boxplot(data$sbp ~ data$sex)

?t.test
var.test(data$sbp ~ data$sex)
t.test(data$sbp ~ data$sex)
t.test(data$sbp ~ data$sex,, var.equal =T)


plot(density(data$dbp),col="red")
lines(density(data$sbp), col="blue")
var.test(data$sbp, data$dbp)
t.test(data$sbp, data$dbp)


table(data$sex)
table(data$sex, data$chd)
prop.table(table(data$sex, data$chd),1)*100
chisq.test(table(data$sex, data$chd))


#linear regression
plot(data$sbp, data$chol )
fit <- lm(chol ~ sbp, data=data )
fit
summary(fit)
abline(fit,col="red")


summary(data$sbp)
plot(data$sbp, data$chol, xlim = c(0,280) )
abline(fit,col="red")


fit2 <- lm(chol ~ sbp + sex, data=data )
fit2
summary(fit2)


boxplot(data$bmi ~ data$sex)
fit3 <- lm(bmi ~ sbp + sex, data=data )
fit3
summary(fit3)


str(data)
data$sex2 <- ifelse(data$sex==1, "female", "male")

fit3 <- lm(bmi ~ sbp + sex2, data=data )
summary(fit3)
plot(data$sbp, data$bmi)
lines(data$sbp, 18.76153+0.05921*data$sbp, col="red")
lines(data$sbp, 18.76153+0.05921*data$sbp-0.63388, col="blue")
summary(fit3)



