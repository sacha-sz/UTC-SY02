# Q1

# 6*7
# 2**16
# 1+2+3+4+5
# 5*6/2
# 
# sqrt(36)
# sin(pi/15)**2 + cos(pi/15)**2
# log2(2**42)
# log(exp(1.6))
# 
# a <- (1+sqrt(5))/2
# a**2
# a+1

a <- log(640320**3 + 744)/sqrt(163)
print(a==pi)


# Q2

# c(1, c(2, 3), c(4, 5))
notes <- c(18, 1.5, 9.5, 15.5, 15, 15.5, 0.5, 14.5, 10.)


# Q3

# v <- c(1, 2, 3, 4, 5)
# u <- c(5, 4, 3, 2, 1)
# v + 1
# 2*v
# v/3
# u * v
# u == 2
# v > pi

notes_update <- c(notes, 4)

v <- c(1, 2, 3, 4, 5)
u <- c(5, 4, 3, 2, 1)
v + 1
2*v
v/3
u * v
u == 2
v > pi


# Q4

# v[1]
# v[2] + v[4]
# v[2:4] # On extrait le vecteur formé par les cases 2, 3 et 4
# v[c(1, 3, 5)] # On extrait le vecteur formé par les cases 1, 3 et 5


notes10 <- notes_update/2
notes10
length(notes10[notes10 > 6])


# Q5

# choix <- c(FALSE, FALSE, TRUE, FALSE, TRUE)
# v[choix]
# choix <- v > pi
# v[choix]

mean(c(notes10[1], notes10[length(notes10)], notes10[3]))


# Q6

choix <- notes_update > 10.
res <- notes_update[choix]
length(res)


# Q7

# v[2] <- 0
# v[2:4] <- 1
# v[v == 1] <- 2

non_fract <- notes_update == round(notes_update)
res <- notes_update[non_fract]
min(res)


# Q8

notes2 <- notes_update - 2
notes2


# Q9

# (collection <- c("R", "V", "B", "V"))
# [1] "R" "V" "B" "V"
# (f <- factor(collection))
# [1] R V B V
# Levels: B R V

# 2 * f
# Warning in Ops.factor(2, f): '*' not meaningful for factors
# [1] NA NA NA NA
# f > "V"
# Warning in Ops.factor(f, "V"): '>' not meaningful for factors
# [1] NA NA NA NA Batman

# (f <- ordered(collection))
# [1] R V B V
# Levels: B < R < V
# f > "B"
# [1] TRUE TRUE FALSE TRUE
# f <- factor(collection, ordered = TRUE)
# f < "R"
# [1] FALSE FALSE TRUE FALSE

# (f <- factor(collection, ordered = TRUE, levels = c("R", "V", "B")))
# 1] R V B V
# Levels: R < V < B
# f < "R"
# [1] FALSE FALSE FALSE FALSE


choix <- notes2 < 0.
res <- notes2[choix]
length(res)

notes2[choix] <- 0
notes2


# Q10

ADN <- factor(c("A", "C", "A", "A", "G", "A", "T", "G", "C", "C", "A", "T", "T", "G", "T", "C"))
levels(ADN)
nlevels(ADN)


# Q11

# v <- 5:10
# f <- factor(c("R", "V", "B", "R", "V", "B"))
# X <- data.frame(v, f, v > 7)
# X
# v f v...7
# 1 5 R FALSE
# 2 6 V FALSE
# 3 7 B FALSE
# 4 8 R TRUE
# 5 9 V TRUE
# 6 10 B TRUE

A_nb <- length(ADN[c(ADN =="A")])
C_nb <- length(ADN[c(ADN =="C")])
G_nb <- length(ADN[c(ADN =="G")])
T_nb <- length(ADN[c(ADN =="T")])


# Q12

X <- read.csv("sy02.data")
length(X)
ncol(X)
nrow(X)
names(X)


# Q13

head(X)
summary(X)


# Q14

X[,c(2, ncol(X))]


# Q15

mean(X[X$correcteur.median == "EG", 'median'])
mean(X[X$correcteur.median == "EG", 2])


# Q16

notes_median <- X$median
notes_final <- X$final
progression <- notes_final - notes_median
length(progression[progression > 0]) / length(progression) * 100


# Q17

mean(X$median)
sd(X$median)
var(X$median)
median(X$median)
max(X$median)
min(X$median)


# Q18 

summary(X$median)
quantile(X$median)


# Q19

IQR(X$median)


# Q20

SY02_sorted <- sort(X$median)
taille <- length(SY02_sorted) - 10
SY02_sorted <- SY02_sorted[11:taille]
mean(SY02_sorted)


# Q21

t <- table(X$correcteur.median)
barplot(t)


# Q22

boxplot(X$final)
summary(X$final)


# Q23

val <- quantile(X$final, 0.25) - 1.5 * IQR(X$final)
length(X$final[X$final < val])


# Q24

stem(X$moyenne)


# Q25

hist(X$final)


# Q26

h <- hist(X$final, breaks=c(0, 15, 20))


# Q27

h$density


# Q28

sum(diff(h$breaks) * h$density)


# Q29

plot(X$median, X$final)
plot(X$final ~ X$median, data=X)


# Q30

boxplot(X$final ~ X$correcteur.final, data=X)


# Q31

final <- X$final[X$correcteur.final == "DH"]
boxplot(final)


# Q32

stripchart(X$final ~ X$correcteur.final, data=X)


# Q33

stripchart(X$final ~ X$correcteur.final, data=X, method="jitter")

