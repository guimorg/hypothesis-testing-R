# Let's see which datasets are already available from our vanilla R installation
data()

# Attaching the database lets us access each one of the variables
# without using the '$' atomic operator
attach(mtcars)

head(mtcars)
# If this doesn't work on RStudio, try running this command:
# data(mtcars)

?mtcars
# Is the same as help(mtcars)

# Getting statistical information about each variable
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
qqnorm(x); qqline(x)
qqnorm(y); qqline(y)

cor.test(x, y, method="pearson")

# Here we are only making the values more human-readable
mtcars$fam <- factor(mtcars$am, levels=c(0,1), labels=c("automatic","manual"))

summary(mtcars$fam)

attach(mtcars)  # Need to reattach because of new variable

shapiro.test(mpg[fam == "manual"])
shapiro.test(mpg[fam == "automatic"])

par(mfrow=c(1,2))
qqnorm(mpg[fam == "manual"]); qqline(mpg[fam == "manual"])
qqnorm(mpg[fam == "automatic"]); qqline(mpg[fam == "automatic"])

boxplot(mpg ~ fam, ylab="Miles/Gallon", xlab="Transmission")

mtcars.t.test <- t.test(mpg ~ fam)
mtcars.t.test

mtcars.mpg.fam.diff <- round(mtcars.t.test$estimate[1] - mtcars.t.test$estimate[2], 1)

# Confidence level as a %
conf.level <- attr(mtcars.t.test$conf.int, "conf.level") * 100

# RStudio
Our study finds that engines yields on average `r mtcars.mpg.fam.diff` miles/gallong in the automatic transmission group compared to the manual transmission group (t-statistic `r round(mtcars.t.test$statistic, 2)`, p=`r round(mtcars.t.test$p.value, 3)`, `r conf.level`% CI [`r round(mtcars.t.test$conf.int,1)`]g)

t.test(mpg ~ fam, var.eq = T)

t.test(mpg~fam, var.eq = T, alternative = "less")

shapiro.test(hp[fam == "manual"])

shapiro.test(hp[fam == "automatic"])

boxplot(hp ~ fam)

wilcox.test(hp ~ fam, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F, exact=F)

mtcars$cyl <- as.factor(mtcars$cyl)
attach(mtcars)

# Creating the contigency table 
contingency <- table(carb, cyl)

contingency

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

