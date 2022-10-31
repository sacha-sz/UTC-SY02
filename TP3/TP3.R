#     Partie 1

runifa <- function(n) {
  if(!exists("param")) param <<- sample(10:20, 1)
  runif(n, min = 0, max = param)
}

# Q1
estim <- function (echantillon)
{
  return (2 * mean(echantillon))
}

n <- 1000
n1 <- 100


# Q2
a <- estim(runifa(n1))
a

b <- estim(runifa(n1))
b

c <- estim(runifa(n1))
c

# a <- replicate(1000, estim(runifa(n))) on remplace n
a_r <- replicate(n, estim(runifa(n1)))
a_r

# Q3
boxplot(a_r)
median(a_r)
param

# Q4
estim_k <- function (echantillon, k)
{
  if (k <= 0)
  {
    stop("k doit être supérieur ou égal à 1")
  }
  return (((k+1) * mean(echantillon^k))^(1/k))
}

b_r <- replicate(n, estim_k(runifa(n1), 2))
b_r

boxplot(b_r)
median(b_r)
param



#     Partie 2

runknown <- function(n) {
  bn <- rbinom(n, 1, 0.2)
  bn * rnorm(n, mean=-4, sd=1) + (1 - bn) * rnorm(n, mean=10, sd=1)
}

# Q5
loi_inc <- runknown(n)

u <- mean(loi_inc)
u

sd <- sd(loi_inc)
sd

# Q6
histo<-hist(runknown(n), xlab="X", main="Répartition loi inconnue")

# Q7
plot(ecdf(runknown(n)))

# Q8
u <- 7.2
o <- sqrt(32.36)

T <- (mean(loi_inc) - u)/(o / sqrt(n))

# Q9
T <- (mean(loi_inc) - u)/(o / sqrt(n))


# Q10
random.T <- function(n) {
  # Générer le vecteur x de taille n
  # Calculer une réalisation de la loi T


  return((mean(runknown(n)) - u)/(o / sqrt(n)))
}

t.10 <- replicate(10, random.T(n))
t.1000 <- replicate(n, random.T(n))

mean(t.1000)
sd(t.1000)

# Q11
plot(ecdf(runknown(n)))
plot(ecdf(t.1000), add=TRUE)

# Q12
curve(pnorm, add=TRUE)

# Q13


#     Partie 3
# Q14
f <- function(x, lambda)
{
  if (x < 0) {
    return (0)
  }
  else
  {
    return (dexp(x, rate=lambda))
  }
}

# Q15
L <- function(x, lambda)
{
  return (prod(dexp(x, rate=lambda)))
}

# Q16
logL <- function(x, lambda)
{
  return (sum(log(dexp(x, rate=lambda))))
}

# Q17
x <- rexp(n1, rate=3)

L(x, 3)
logL(x, 3)


L(x, 2.8)
logL(x, 2.8)


L(x, 3.1)
logL(x, 3.1)

# Q18
lambdas <- seq(0, 6, 0.01)
logL.lambdas <- sapply(lambdas, function(lambda) logL(x, lambda))
plot(lambdas, logL.lambdas, type = "l")

g <- function(x) {
  -(x - pi)^2
}

(opt <- optimize(g, lower = -10, upper = 10, maximum = TRUE))
opt$maximum
opt$objective

# Q19
lambda_r <- optimize(logL, lower=0, upper=6, maximum=TRUE, x=x)
lambda_r

# Q20
sim.EMV <- function ()
{
  x <- rexp(n, rate=3)
  return (optimize(logL, lower=0, upper=6, maximum=TRUE, x=x)$maximum)
}

# Q21
lambda_r_m <- replicate(10000, sim.EMV())
lambda_r_m

mean(lambda_r_m)

sd(lambda_r_m)

(n/(n-1))*3 - 3


#     Partie 4

# Q22
install.packages("pracma")
library(pracma)
grad(g, 0)

lambda <- 3

# Q23
sim.Fisher <- function ()
{
  x <- rexp(n, rate=3)
  
  logLx <- function(lambda) logL(x, lambda)
  (grad(logLx, 3))^2
}

sim.Fisher()

# Q24

inf.Fisher <- mean(replicate(10000, sim.Fisher()))
inf.Fisher
n/(lambda ^ 2)


# Q25
variance_emp <- 1 / inf.Fisher
variance_emp

var_theorique <- 1 / (n/(lambda ^ 2))
var_theorique


# Q26
grad2 <- function(f, x) {
  df <- function(x) {
    grad(f, x)
  }
  grad(df, x)
}


sim.Fisher <- function ()
{
  x <- rexp(n, rate=3)
  
  logLx <- function(lambda) logL(x, lambda)
  (grad2(logLx, 3))
}


(-mean(replicate(1000, sim.Fisher())))

