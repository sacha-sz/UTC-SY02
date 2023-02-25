install.packages("MASS")
library(MASS)

# Test d'homogénéité

# Q1
im <- immer
t.test(im$Y1, im$Y2, paired = TRUE)
# p-value faible => rejette H0 => moyennes différentes

# Q2
z <- im$Y1 - im$Y2
shapiro.test(z)

sign <- im$Y1 < im$Y2
(nsuccess <- length(sign[sign]))
(nsuccess <- sum(sign))
n <- length(sign)
prop.test(nsuccess, n, p=0.5)


# Q3
chauss <- shoes
var.test(chauss$A, chauss$B)

# Q4
t.test(chauss$A, chauss$B, var.equal = TRUE)
# Il n'y a pas de différence significative d'usure

# Test d'adéquation

# Q5
gal <- galaxies
shapiro.test(gal)

# Q6
delai <- read.csv("delai-data.data")
lambda <- 1/mean(delai$delai)
ks.test(delai$delai, "pexp", rate=lambda)

# Q7
(breaks <- quantile(delai$delai, seq(0, 1, 0.1)))

# Q8
(x <- table(cut(delai$delai, breaks = breaks, include.lowest = TRUE)))

# Q9
(p <- diff(c(0, pexp(breaks, lambda)[3 : length(breaks) - 1], 1)))

# Q10
chisq.test(x, p=p)

# Q11
stat <- chisq.test(x, p=p)$statistic
1 - pchisq(stat, df = length(x) -1 -1)

# Tests d’indépendance
# Q12
glace <- data.frame(chocolat = c(100, 350),
                    vanille = c(120, 200), 
                    fraise = c(60, 90))
# Q13 + Q14
ct <- chisq.test(glace)
# rejette l'indépendance 

# Q15
ct$observed
ct$expected

# Q16
sum((ct$expected - glace)^2/ct$expected)

# Cas d'études
# Q17
cold <- read.csv("cold.data", row.names = 1)
chisq.test(cold)
