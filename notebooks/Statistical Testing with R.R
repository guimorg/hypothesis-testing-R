# Let's see which datasets are already available from our vanilla R installation
data()

# Attaching the database lets us access each one of the variables
# without using the '$' atomic operator
attach(mtcars)

head(mtcars)
# If this doesn't work on RStudio, try running this command:
# data(mtcars)

?mtcars

# Getting statistical information about each variable
summary(mtcars)

mtcars$vs <- factor(mtcars$vs, levels = c(0, 1), labels = c("v-shaped", "straight"))
mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c("automatic", "manual"))
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$cyl <- factor(mtcars$cyl)
attach(mtcars)

summary(mtcars)

# To check for the covariation we will use a very simple approach of a scatter plot
x <- wt
y <- mpg

# We are using **raw R**, you will find many packages that perform
# all of these approaches and also a better viz in next lectures!
plot(
    x, y, 
    main = "Covariation for Weight and Miles Consumption",
    xlab = "Weight",
    ylab = "Miles/gallon",
    pch = 19, frame = FALSE
)
abline(lm(y ~ x, data = mtcars), col = "blue")

# First, let's test for the weight (x)
shapiro.test(x)

shapiro.test(y)

par(mfrow=c(1,2))
qqnorm(x, main="Normal Q-Q Plot wt"); qqline(x)
qqnorm(y, main="Normal Q-Q Plot mpg"); qqline(y)

par(mfrow=c(1,2))
hist(x,  main = "Histogram wt")
hist(y,  main = "Histogram mpg")

cor.test(x, y, method="pearson")

summary(mtcars$am)

shapiro.test(mpg)

qqnorm(mpg, main="Normal Q-Q Plot mpg"); qqline(mpg)
hist(mpg,  main = "Histogram manual Fuel Consumption")

boxplot(mpg~am, ylab="Miles/Gallon", xlab="Transmission")

mtcars.t.test <- t.test(mpg ~ am)
mtcars.t.test

mtcars.mpg.am.diff <- round(mtcars.t.test$estimate[1] - mtcars.t.test$estimate[2], 1)

# Confidence level as a %
conf.level <- attr(mtcars.t.test$conf.int, "conf.level") * 100

sprintf(
    "Our study finds that engines yields on average %.3f miles/gallon in the automatic transmission group compared to the manual transmission group (t-statistic %.3f, p=%.3f, %.0f %% CI [%.3f %.3f]miles/gallon",
    mtcars.mpg.am.diff,
    mtcars.t.test$statistic,
    mtcars.t.test$p.value,
    conf.level,
    mtcars.t.test$conf.int[1],
    mtcars.t.test$conf.int[2]
)

t.test(mpg ~ am, var.eq = T)
t.test(mpg~am, var.eq = T, alternative = "less")

qqnorm(hp, main="Normal Q-Q Plot hp"); qqline(hp)
hist(hp)
shapiro.test(hp)
boxplot(hp ~ am)
wilcox.test(hp ~ am, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F, exact=F)

mtcars$cyl <- as.factor(mtcars$cyl)
attach(mtcars)

# Creating the contigency table 
contingency <- table(carb, cyl)

contingency
prop.table(contingency)

chisq.test(contingency)
fisher.test(contingency)

summary(aov(lm(mpg~.,data=mtcars)))

fit <- lm(mpg ~ am, data=mtcars)
fit1 <- lm(mpg ~ am + cyl, data=mtcars)
fit2 <- lm(mpg ~ am + cyl + disp, data=mtcars)
fit3 <- lm(mpg ~ am + cyl + disp + wt, data=mtcars)
fit4 <- lm(mpg ~ am + cyl + disp + wt + hp, data=mtcars)

anovaTable <- anova(fit, fit1, fit2, fit3, fit4)
anovaTable

finalfit <- lm(mpg ~ am + cyl + wt + disp + wt + hp, data=mtcars); summary(finalfit)