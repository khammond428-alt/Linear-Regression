## BANA 7052 Applied Linear Regression

alumni <- read.csv("~/Desktop/BANA 7052/alumni.csv")
head(alumni)

## Question 1a
str(alumni)
summary(alumni)

## Question 1b
boxplot(alumni$percent_of_classes_under_20,
        main = "Boxplot: Percent of Classes Under 20")

boxplot(alumni$alumni_giving_rate,
        main = "Boxplot: Alumni Giving Rate")

cor(alumni$percent_of_classes_under_20, alumni$alumni_giving_rate)

plot(alumni$percent_of_classes_under_20, alumni$alumni_giving_rate,
     xlab = "Percent of Classes Under 20",
     ylab = "Alumni Giving Rate",
     main = "Scatterplot of Alumni Giving Rate vs. Small-Class Percentage",
     pch = 19, col = "darkblue")

abline(lm(alumni_giving_rate ~ percent_of_classes_under_20, data = alumni),
       col = "red", lwd = 2)

## Question 1c
model <- lm(alumni_giving_rate ~ percent_of_classes_under_20, data = alumni)
summary(model)

## Question 2a
set.seed(7052)

n   <- 100
x   <- rnorm(n, mean = 2, sd = 0.1)   # X ~ N(2, 0.1)
eps <- rnorm(n, mean = 0, sd = 0.5)   # ε ~ N(0, 0.5)

y   <- 10 + 5 * x + eps               # Y = 10 + 5X + ε

## Question 2b 
plot(x, y,
     xlab = "X",
     ylab = "Y",
     main = "Simulated Data: Y = 10 + 5X + ε",
     pch = 19, col = "darkblue")

model <- lm(y ~ x)
summary(model)

summary(x)
summary(y)
boxplot(x, main = "Boxplot of X")
boxplot(y, main = "Boxplot of Y")
cor(x, y)
plot(x, y,
     pch = 19, col = "darkblue",
     xlab = "X",
     ylab = "Y",
     main = "Simulated Data: Y = 10 + 5X + ε")

## Question 2c
model <- lm(y ~ x)
summary(model)

mean(model$residuals^2)

## Question 2d
mean(x)
mean(y)

model <- lm(y ~ x)
summary(model)

plot(x, y,
     pch = 19, col = "darkblue",
     xlab = "X",
     ylab = "Y",
     main = "Regression Line and Sample Means")

abline(model, col = "red", lwd = 2)

points(mean(x), mean(y),
       pch = 19, col = "orange", cex = 2)

text(mean(x), mean(y),
     labels = "Sample Means (X̄, Ȳ)",
     pos = 4, col = "orange")



## Question 3a
e     <- residuals(model)
yhat  <- fitted(model)

sum(e)
sum(x * e)
sum(yhat * e)

## Question 3b

