###########################################
###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 7            ###
###########################################



# Cel: weryfikacja hipotez ------------------------------------------------


# Zadanie 8 ---------------------------------------------------------------



# Zadanie 9 ---------------------------------------------------------------



# Zadanie 10 --------------------------------------------------------------



# Zadanie 11 --------------------------------------------------------------

# a)

# b)


# ZESTAW 5 ----------------------------------------------------------------


# Zadanie 1 ---------------------------------------------------------------

n <- rnorm(200)
c <- rcauchy(200)
u <- runif(200)
w <- rexp(200)
aw <- w*(-1)

par(mfrow = c(3,2))
qqnorm(n, main = "N(0,1)")
qqline(n)
qqnorm(c, main = "C(0,1)")
qqline(c)
qqnorm(u, main = "U([0,1])")
qqline(u)
qqnorm(w, main = "Exp(1)")
qqline(w)
qqnorm(aw, main = "NegExp(1)")
qqline(aw)



# Zadanie 2 ---------------------------------------------------------------

par(mfrow = c(1, 2))
N <- 200
u <- runif(N)
qu <- qunif(ppoints(N))
qqplot(qu, u)

u <- rexp(N)
qu <- qexp(ppoints(N))
qqplot(qu, u)

ppoints(5, a = 1 / 2)
? ppoints

