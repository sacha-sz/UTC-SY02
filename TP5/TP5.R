#     Partie 1

# Q1
donnees <- data.frame(varx = c(0, 0.2, 0.3, 0.6),
                      vary = c(1.01, 1.44, 1.55, 2.1))
reg.lin <- lm(vary~varx, data = donnees)
a <- reg.lin$coefficients[1]
b <- reg.lin$coefficients[2]

# Q2
plot(donnees)
abline(a = a, b = b, col="blue")

# Q3
summary_reg.lin <- summary(reg.lin)
sum(summary_reg.lin$residuals)

mean(donnees$vary) == (a+mean(donnees$varx)*b)


# Q4
n <- length(donnees$vary)

# variance totale
var_tot <- mean((donnees$vary-mean(donnees$vary))^2)
var_tot

# variance expliquée par la régression
var_reg <- mean((reg.lin$fitted.values-mean(donnees$vary))^2)
var_reg

# variance residuelle
var_res <- (1/n)*sum((donnees$vary - reg.lin$fitted.values)^2)
var_res

# var_tot = var_res + var_reg
var_res + var_reg
var_tot

# R**2
var_reg/var_tot
cor(donnees$varx, donnees$vary)^2

# Partie 1.2
attach(anscombe)
anscombe$x1
lm(y1 ~ x1)

# Q5
par(mfrow=c(2, 2))
plot(x1, y1)
plot(x2, y2)
plot(x3, y3)
plot(x4, y4)

# Q6
par(mfrow=c(1, 1))
reg.lin_1 <- lm(y1~x1, data = anscombe)
hist(reg.lin_1$residuals, freq = FALSE)
curve(dnorm(x, mean = mean(reg.lin_1$residuals), sd = sd(reg.lin_1$residuals)), add= TRUE, col ='RED')

reg.lin_2 <- lm(y2~x2, data = anscombe)
hist(reg.lin_2$residuals, freq = FALSE)
curve(dnorm(x, mean = mean(reg.lin_2$residuals), sd = sd(reg.lin_2$residuals)), add= TRUE, col ='RED')

reg.lin_3 <- lm(y3~x3, data = anscombe)
hist(reg.lin_3$residuals, freq = FALSE)
curve(dnorm(x, mean = mean(reg.lin_3$residuals), sd = sd(reg.lin_3$residuals)), add= TRUE, col ='RED')

reg.lin_4 <- lm(y4~x4, data = anscombe)
hist(reg.lin_4$residuals, freq = FALSE)
curve(dnorm(x, mean = mean(reg.lin_4$residuals), sd = sd(reg.lin_4$residuals)), add= TRUE, col ='RED')


qqnorm(reg.lin_1$residuals)
qqline(reg.lin_1$residuals)

qqnorm(reg.lin_2$residuals)
qqline(reg.lin_2$residuals)

qqnorm(reg.lin_3$residuals)
qqline(reg.lin_3$residuals)

qqnorm(reg.lin_4$residuals)
qqline(reg.lin_4$residuals)

plot(reg.lin_1$fitted.values, rstandard(reg.lin_1))
plot(reg.lin_2$fitted.values, rstandard(reg.lin_2))
plot(reg.lin_3$fitted.values, rstandard(reg.lin_3))
plot(reg.lin_4$fitted.values, rstandard(reg.lin_4))


# Partie 2
# Q7
reg_test <- lm(Pression~Temp, data=hooker.data)
a <- reg_test$coefficients[1]
b <- reg_test$coefficients[2]

plot(reg_test)

qqpnorm(reg_test$residuals)
qqline(reg_test$residuals)


plot(reg_test$fitted.values, rstandard(reg_test))
