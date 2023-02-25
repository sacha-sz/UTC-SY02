install.packages("MASS")
library("MASS")

install.packages("isdals")
library("isdals")


# /*************************** Tests de conformité ***************************/ 
# Q1 : test sur l'espérance 

data <- read.csv("bottles.data")

t.test(data, mu = 500, alternative = "less")
# p.value = 0.07 > conf.level = 0.05
# pas de rejet de H0

t.test(data, mu = 500, alternative = "less", conf.level = 0.9)
# p.value = 0.07 < conf.level = 0.1
# rejet de H0


# Q2 : Test sur une proportion

data <- read.csv("MM.data")

prop.test(data[1, 1], 1713, 1/6)
# p.value = 0.89 => 

prop.test(data[1, 2], 1713, 1/6)
# => rejet quasi certain de H0

prop.test(data[1, 3], 1713, 1/6)
# => rejet

prop.test(data[1, 4], 1713, 1/6)
# => rejet

prop.test(data[1, 5], 1713, 1/6)
# => rejet

prop.test(data[1, 6], 1713, 1/6)
# => rejet


# Q3 : Régression linéaire
data("bodyfat")

plot(Fat~Triceps, data = bodyfat)
plot(Fat~Thigh, data = bodyfat)
plot(Fat~Midarm, data = bodyfat)


reg <- lm(Fat~Tricpes, data=bodyfat)
summary(reg)


reg <- lm(Fat~Thigh, data=bodyfat)
summary(reg)


reg <- lm(Fat~Midarm, data=bodyfat)
summary(reg)


reg <- lm(Fat~0+Midarm, data=bodyfat)
summary(reg)



# /************** Niveau de signification et fonction puissance ***************/

# Q4
# W = {X_barre < |(u_alpha*/sqrt(n) + 1)/(theta_0)}

# Q5
delai <- read.csv("delai-data.data")$delai
theta0 <- 1/151
n <- length(delai)
w <- (mean(delai) < ((qnorm(0.05)/sqrt(n) + 1) * (1 / theta0)))

pnorm(mean(delai) - (1/theta0) / (1/(theta0 * sqrt(n))))


puiss_emp <- function(theta, theta0, n) {
  x <- rexp(n, theta)
  return(mean(x) < (qnorm(0.05)/sqrt(n) + 1) * (1 / theta0))
}

mean(replicate(100, puiss_emp(theta0, theta0, 100)))
