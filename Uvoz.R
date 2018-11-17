library(knitr)
library(dplyr)
library(readr)
library(rvest)
library(gsubfn)
library(ggplot2)
library(reshape2)
library(shiny)
library(tidyr)
library(rmarkdown)
library(actuar)
library(discretization)

# Uvozimo podatke in narišemo histogram

vzorec <- scan("vzorec1.txt")
h <- hist(vzorec, col = "blue", xlab = "visina odskodnine")


visina <- mde(vzorec,pexp,start = list(rate = 1),measure = "CvM")
#mde vrne oceno in razdaljo 
ocena <- visina$estimate[1]
lambda <-ocena
razdalja <- visina$distance[1]
#c)#density exp - dexp
hist(vzorec, probability = TRUE, xlab = "Višina odškodnine")
curve(dexp(x, ocena, razdalja), add = TRUE, From = 0, to = 9, 
      col = 'red')
legend("topright", "Eksponentna porazdelitev", fill="red")

#veliki N je bnomsko porazdeljen  E(N)
#n = 25 - dej eno drugo oznako za n
n_2 = 25
p = 0.5

#N <- binom(x, size = n, prob = p, log = FALSE)

EN <- n_2*p #UPANJE N
EY<- 1/lambda  #Upanje Y
ES <- EN*EY
varN = n_2*p*(1-p)
varY = 1/(lambda)^2
EY2 <- varY + (EY)^2
varS <- varY * EN + EY2 * var(N)
EY
ES
#2 diskretizacija
n = 25
h = 0.25 

diskretna_y <- discretize(pexp(x, lambda),from = 0, to= h*n , step = h ,method = "rounding")


#2b)
#vektor ki izračuna vrednosti od 0 do n*h s korakom h
vektor_2 <- seq(0,(n-1)*h,h)
#vektor x-ov pri katerih pride do skokov
#vzamemo enega manj

#višine stopnic, dobimo iz diskretna y z funkcijo dif
visina_2 <- diffinv(diskretna_y)

plot(stepfun(vektor_2,visina_2), do.points = FALSE,ylab = "porazdelitvena funkcija")
curve(pexp(x,lambda),add = TRUE, col = "blue")
#dodaš v pexp svoje parametre,
# add = True doda na isti graf

#2c
#treba je bilo povečat n 
porazdelitvena <- aggregateDist(method = "recursive",
              model.freq = "binom",
              model.sev = diskretna_y,
              x.scale = h,
              size = n,
              prob = 1/2,
              maxit = 100000
              )
#Uporabiš knots -vstaviš  za povprečje in varianco
plot(porazdelitvena,ylab = "Porazdelitvena funkcija")

#2d upanje in disperzija komulativne škode dobljene funkcije "porazdelitvena"

vrednosti <- knots(porazdelitvena)
verjetnosti <- diff(porazdelitvena)




Upanje_S_diskretno <- sum(vrednosti * verjetnosti)
upanje_kvadrat <- sum(vrednosti * vrednosti* verjetnosti)
Var_S <- upanje_kvadrat - (Upanje_S_diskretno)*(Upanje_S_diskretno)
# Var(S) = E[S^2] - E[S]^2

#3.a) 
#simulacija N
N_simulacija <- bin(10000, 25, 1/2)

# simulacija S
S_simulacija <- c()
for (n in N_sumulacija){
S_simulacija <-c(simulacija_S, sum(rexp(n,shape,min)))

}
#3.b) 
upanje_S_simulacije <- mean(S_simulacija)
varianca_S_simulacije <- var(S_simulacija)

graf_simulacija<- plot(porazdelitvena)
plot(ecdf(S_simulacija),add = TRUE, col = "blue")
legend(500, 0.5, legend = c("Exponentni algoritem,", "Monte Carlo simulacija"), box.lty = 0, 
       col = c("black","blue"), lty = 1:1)
  
