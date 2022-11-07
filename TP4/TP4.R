par(mfrow=c(1, 1))

#     Partie 1 - Fonctions pivotales

# Q1
mu <- 3
sigma <- 2
n <- 1000

chisq1 <- function()
{
  x <- rnorm(n, mean = mu, sd = sigma)
  (n - 1) * (sd(x)^2) / (sigma ^ 2)
}

chisq1000 <- replicate(n, chisq1())
hist(chisq1000, freq = FALSE)

curve(dchisq(x, df = n - 1), add = TRUE)


# Q2
stu1 <- function()
{
  x_b <- rnorm(n, mean = mu, sd = sigma)
  (mean(x_b) - mu) / (sd(x_b) / sqrt(n))
}

stu1000 <- replicate(n, stu1())
hist(stu1000, freq = FALSE)

x <- seq(0,10,length.out=200)
curve(dt(x, df = n - 1), add = TRUE)

#     Partie 2 - Intervalles de confiance

# Q3
# IC sur moyenne avec variance connue
# IC 1−α(μ) = [mean(X) − u(1− α/2)√(σ2/n), mean(X) + u(1− α/2)√(σ2/n)] 

n <- 100
mu <- 42
sd <- pi
alpha <- 0.05

x <- rnorm(n, mean = mu,sd = sd)
mean(x) + c(-1, 1) * qnorm(1 - (alpha / 2)) * pi/ sqrt(n)
 

# Q4
# IC sur moyenne avec varaince inconnue
# IC 1−α(μ) = [mean(X) − t(n-1, 1− α/2)√(S*2/n), mean(X) + t(n-1, 1− α/2)√(S*2/n)] 

mean(x) + c(-1, 1) * qt(1 - alpha/2, n - 1) * sd(x) / sqrt(n)
  
t.test(x, conf.level = 1-alpha)$conf.int

# Q5
gen_IC <- function(x, alpha) {
  n <- length(x)
  mean(x) + c(-1, 1) * qt(1 - alpha / 2, n - 1) * sd(x) / sqrt(n)
} 

# Q6
mu <- 3

ICs <- replicate(100, gen_IC(rnorm(n, mean = mu), alpha))


# Q7
source("utils.R")

plot_ICs(ICs, mu)
# Nous allons obtenir en moyenne nb_vert/(nb_vert+nb_rouge) est environ égal à 95%
# Soit nb_rouge environ égale à 5%


# Q8
xlim <- c(1.5, 4.5)
n_10 <- 10
n_100 <- 100
n_1000 <- 1000

ICs_10 <- replicate(100, gen_IC(rnorm(n_10, mean = mu,sd = sd), alpha))
ICs_100 <- replicate(100, gen_IC(rnorm(n_100, mean = mu,sd = sd), alpha))
ICs_1000 <- replicate(100, gen_IC(rnorm(n_1000, mean = mu,sd = sd), alpha))

par(mfrow = c(1, 3))
plot_ICs(ICs_10, mu, xlim=xlim, main="n = 10")
plot_ICs(ICs_100, mu, xlim=xlim, main="n = 100")
plot_ICs(ICs_1000, mu, xlim=xlim, main="n = 1000")

# On remarque que la largeur moyenne de l'intervalle de confiance, diminue lorsque n augmente

# Q9
sd_1 <- sd * 0.1
sd_2 <- sd * 1.1
sd_3 <- sd * 1.9

ICs_n1 <- replicate(100, gen_IC(rnorm(n_100, mean = mu,sd = sd_1), alpha))
ICs_n2 <- replicate(100, gen_IC(rnorm(n_100, mean = mu,sd = sd_2), alpha))
ICs_n3 <- replicate(100, gen_IC(rnorm(n_100, mean = mu,sd = sd_3), alpha))

par(mfrow = c(1, 3))

plot_ICs(ICs_n1, mu, xlim=xlim, main="sigma = 0.1")
plot_ICs(ICs_n2, mu, xlim=xlim, main="sigma = 1.1")
plot_ICs(ICs_n3, mu, xlim=xlim, main="sigma = 1.9")

# On remarque que la largeur moyenne de l'intervalle de confiance, diminue lorsque l'écart-type diminue

# Q10
hit  <- function (n, param, alpha) {
  x <- rnorm(n, mean = param)
  IC <- gen_IC(x, alpha)
  return (IC[1] <= param & param <= IC[2])
}

recouvrement <- mean(replicate(1000, hit (n, mu, alpha)))

# Très proche de l'intervalle requis avec le paramètre alpha

#     Partie 3 - Lemme de Slutsky

# Q11
slutsky <- function (p, n, k, alpha) {
  hit_b <- function () {
    x <- rbinom(n, 1 ,p)
    IC <- mean(x) + c(-1, 1) * qnorm(1 - alpha/2) * sqrt(mean(x) * (1 - mean(x)) / n)
    return (IC[1] <= p & p <= IC[2])
  }
  
  return(mean(replicate(k, hit_b())))
}


# Q12
p <- 0.02
k <- 10000
ns <- floor(10^seq(1,4, length.out=40))
res <- sapply(ns, function(n) slutsky(p, n, k, alpha))

par(mfrow=c(1, 1))
plot(x = log10(ns), y = res, type = "l", col = "red")


# Q13
appartenance_binom_r <- function (n, param, alpha) {
  x <- rbinom(n, 1 ,param)
  u2 <- (qnorm(1-alpha/2)^2)
  IC <-  (2*n*mean(x) + u2 + c(-1, 1)*qnorm(1-alpha/2) * sqrt(u2 + 4 * n * mean(x) * (1-mean(x)))) / (2*n + 2*u2) 
  return (IC[1] <= param & param <= IC[2])
}


real <- function (p, n, k, alpha) {
  return(mean(replicate(k, appartenance_binom_r(n, p, alpha))))
}

res_2 <- sapply(ns, function(n) real(p, n, k, alpha))


lines(x = log10(ns), y = res_2, col = "green")

# Le bon IC a une convergence beaucoup plus rapide

