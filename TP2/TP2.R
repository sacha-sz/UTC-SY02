#     P1

library(MASS)
head(painters)


# Q1
histo<-hist(painters$Composition, xlab="Composition", main="Répartition note : composition")
histo1<-hist(painters$Drawing, xlab="Drawing", main="Répartition note : dessin")
histo2<-hist(painters$Colour, xlab="Colour", main="Répartition note : couleur")
histo3<-hist(painters$Expression, xlab="Expression", main="Répartition note : expression")


# Q2
painters_2 <- painters
moyenne <- (painters$Composition + painters$Drawing + painters$Colour + painters$Expression) / 4


painters_2$Moyenne <- moyenne
painters_2 <- subset(painters_2, select = -c(Composition:School))

head(painters_2)


# Q3
moyenne_emp <- sum(painters_2$Moyenne) / nrow(painters_2)
moyenne_emp



variance_tab <- (painters_2$Moyenne - moyenne_emp)**2

variance_emp <- sum(variance_tab) / nrow(painters_2)
variance_emp

variance_emp_c <- sum(variance_tab) / (nrow(painters_2) - 1)
variance_emp_c


ecart_type_emp <- sqrt(variance_emp)
ecart_type_emp

ecart_type_emp_c <- sqrt(variance_emp_c)
ecart_type_emp_c


# Q4
moyenne_emp_v <- mean(painters_2$Moyenne)
moyenne_emp_v

variance_emp_v <- var(painters_2$Moyenne) * ((nrow(painters_2) - 1) / nrow(painters_2))
variance_emp_v

variance_emp_c_v <- var(painters_2$Moyenne)
variance_emp_c_v


ecart_type_emp_v <- sd(painters_2$Moyenne) * sqrt(((nrow(painters_2) - 1) / nrow(painters_2)))
ecart_type_emp_v

ecart_type_emp_c_v <- sd(painters_2$Moyenne)
ecart_type_emp_c_v


# Q5
histo4<-hist(painters_2$Moyenne, xlab="Moyenne", main="Répartition moyenne")
# Ressemble à une loi normale
#   symétrie
#   les queues sont ni trop grandes ni trop petites
#   le mode est environ égal à la moyenne et à la médiane



# P2

unif # loi uniforme
pois # loi de Poisson
exp # loi exponentielle
binom # loi binomiale
norm # loi normale
t # loi de Student
chisq # loi du X2
f # loi de Fisher

dunif(1, min = 2, max = 5)
dunif(2, min = 2, max = 5)
dunif(4, min = 2, max = 5)
dunif(6, min = 2, max = 5)

punif(4, min = 2, max = 5)

qunif(0.25, min = 2, max = 5) # fractile d'ordre 0.25, premier quartile

runif(1, min = 2, max = 5)

# Ajout de
# d : fonction de densité
# p : fonction de répartition
# q : fractile
# r : réalisation d'une va

# Q6.1
1 - pnorm(3)
pnorm(3, lower.tail=FALSE) # par défaut loi normale centréee réduite

# Q6.2
pnorm(42, mean = 35, sd = 6)

# Q6.3
pnorm(50, mean = 35, sd = 6) - pnorm(40, mean = 35, sd = 6)

# Q6.4
n <- 5
dbinom(n-1, n, 0.5)

n <- 10
dbinom(n-1, n, 0.5)

n <- 30
dbinom(n-1, n, 0.5)

# Q6.5
sum(dbinom(15:20, 20, 0.5))
1 - pbinom(14, 20, 0.5)

# Q6.6
sum(dbinom(10:15, 20, 0.5))
pbinom(15, 20, 0.5) - pbinom(9, 20, 0.5)



# Q7
for (n in c(0.05, 0.1, 0.9))
{
  print(qnorm(n, mean=0, sd=1)) # Q7.1

  print(qchisq(n, 10)) # Q7.2

  print(qt(n, 5)) # Q7.3

  print(qf(n, 2, 5)) # Q7.4
}

# qnorm(c(0.05, 0.1, 0.9), mean=0, sd=1)

# P3

carre <- function(x) {
  y <- x * x
  return(y)
}

carre(2)



puissance <- function(a, b) {
  return(a^b)
}

puissance(2, 3)


# Q8

dloi <- function (x, b) {
  if (b <= 0)
  {
    stop("B ne peut pas être inférieur à 0")
  }
  a <- (2/(b**2))
  f <- a * x
  f[(x<0 | x > b)] <- 0
  return (f)
}

dloi(5, 5)
dloi(5:8, 18)

curve(dloi(x, 3), from = -5, to = 5)

# Q9

dloi(c(-1, 0, 1, 2, 3, 4, 5), 3)

# Q10
ploi <- function (x, b) {
  if (b <= 0)
  {
    stop("B ne peut pas être inférieur à 0")
  }
  a_prim <- (1 / (b**2))
  f <- a_prim * (x**2)
  f[x <= 0] <- 0
  f[x >= b] <- 1
  return (f)
}

# Q11
curve(ploi(x, 3), from = -5, to = 5)

# Q12
qloi <- function(alpha, b)
{
  if (b <= 0)
  {
    stop("B ne peut pas être inférieur à 0")
  }

  if (alpha < 0 || alpha > 1)
  {
    stop("Alpha doit être compris entre 0 et 1")
  }

  f <- b * sqrt(alpha)
  f[alpha == 0] <- 0
  f[alpha == 1] <- b
  f[alpha < 0 | alpha > 1] <- NULL
  return(f)
}

curve(qloi(x, 3), from =0, to = 1)

# Q13
rloi <- function(n, b)
{
  if (b <= 0)
  {
    stop("B ne peut pas être inférieur à 0")
  }

  if (n <= 0)
  {
    stop("On ne peut pas générer un échantillon de taille nulle ou négative")
  }

  real_dloi <- qloi(runif(n, min=0, max=1), b)
  return (real_dloi)
}

# Q14
res1 <- rloi(1000, 3)
histo <- hist(res1, freq = FALSE)
curve(dloi(x, 3), from = -1, to = 5, add = TRUE)
