#Homework 8
#setwd
library(rethinking)
library(rstan)
library(xlsx)
library(reshape2)
#bringing in the mongoose data and cleaning
occ<-read.csv("H:/2017Data/daily_occupancy.csv")
colnames(occ)[2] <- "StudySite"
occ$btm<- ifelse(occ$bushy_tailed_mongoose > 0, "1","0")
ALLcovs<-read.xlsx("H:/2017Data/Data2.xlsx", sheetName = "PsiCov")
BTM<-merge(occ, ALLcovs, by="StudySite")
BTM<-cbind(BTM[1],BTM[,43],BTM[,44],BTM[,45],BTM[,46],BTM[47],BTM[48])
colnames(BTM)[2]<-"btm"
colnames(BTM)[3]<-"nights"
colnames(BTM)[4]<-"settle"
colnames(BTM)[5]<-"river"
colnames(BTM)[6]<-"NDVI"
#convert camera site names to integer index variable for proportional data setup
BTM$site <- coerce_index(BTM$StudySite)
#change presence/absence btm column from factor to numeric
BTM$btm<-as.numeric(BTM$btm)
BTM$btm<-(BTM$btm-1)

#-------------Q1------------------

#COMBO ONE
btm2<-alist(
  btm~dbinom(1,p),
  logit(p)<-a+b1*NDVI+b2*river,
  a ~ dnorm(0,10),
  b1 ~ dnorm(0,1),
  b2 ~ dnorm(0,1)
)

btm_stan1 <- map2stan(btm2 , data=BTM ,
                      iter=2000 , warmup=1000 , chains=4 ,
                      refresh=500)
summary(btm_stan1)
WAIC(btm_stan1) # 105.5443

#COMBO TWO
btm3<-alist(
  btm~dbinom(1,p),
  logit(p)<-a+b1*NDVI+b2*river+b3*settle,
  a ~ dnorm(0,10),
  b1 ~ dnorm(0,1),
  b2 ~ dnorm(0,1),
  b3 ~ dnorm(0,1)
)

btm_stan2 <- map2stan(btm3 , data=BTM ,
                      iter=2000 , warmup=1000 , chains=4 ,
                      refresh=500)

summary(btm_stan2)
WAIC(btm_stan2) # 97.31256

#COMBO THREE
btm4<-alist(
  btm~dbinom(1,p),
  logit(p)<-a+b1*NDVI+b3*settle,
  a ~ dnorm(0,10),
  b1 ~ dnorm(0,1),
  b3 ~ dnorm(0,1)
)

btm_stan3 <- map2stan(btm4 , data=BTM ,
                      iter=2000 , warmup=1000 , chains=4 ,
                      refresh=500)
summary(btm_stan3)
WAIC(btm_stan3) # 95.48336

# COMBO FOUR
btm5<-alist(
  btm~dbinom(1,p),
  logit(p)<-a+b3*settle,
  a ~ dnorm(0,10),
  b3 ~ dnorm(0,1)
)

btm_stan4 <- map2stan(btm5 , data=BTM ,
                      iter=2000 , warmup=1000 , chains=4 ,
                      refresh=500)
summary(btm_stan4)
WAIC(btm_stan4) # 95.52503

###ACCORDING TO WAIC SCORES: My third model that included only NDVI and distance from settlement was the "best".
# This is what I would expect given my larger model results. 

#------------DATADRIVEN COMP-------------------
#Blood donations
#I'm nowhere close to being able to predict anything. 
training<-read.csv("training.csv")
test<-read.csv("test.csv")

blood1<-alist(
  donated_march~dbinom(1,p),
  logit(p)<-a+b1*volume,
  a ~ dnorm(0,10),
  b1 ~ dnorm(0,1)
)

stan_blood1 <- map2stan(blood1 , data=training ,
                      iter=2000 , warmup=1000 , chains=4 ,
                      refresh=500)

plot(stan_blood1) #check convergence
WAIC(stan_blood1) # 613.5372
summary(stan_blood1) #estimate of b1 is just zero, so...
plot(training$donated_march~training$volume)
curve(plogis(-1.65+0*x),add=T)

#changing the priors
blood2<-alist(
  donated_march~dbinom(1,p),
  logit(p)<-a+b1*volume,
  a ~ dnorm(0,5),
  b1 ~ dnorm(0.1,10)
)

stan_blood2 <- map2stan(blood2 , data=training ,
                        iter=2000 , warmup=1000 , chains=4 ,
                        refresh=500)

plot(stan_blood2) #check convergence
WAIC(stan_blood2) # 613.7447
summary(stan_blood2) #still 0, so... and a= -1.66 now

#try another variable to see if b1 still 0
blood3<-alist(
  donated_march~dbinom(1,p),
  logit(p)<-a+b1*months_first,
  a ~ dnorm(0,10),
  b1 ~ dnorm(0,5)
)

stan_blood3 <- map2stan(blood3 , data=training ,
                        iter=2000 , warmup=1000 , chains=3 ,
                        refresh=500)

summary(stan_blood3) #also b1=0
plot(stan_blood3)
WAIC(stan_blood3) #638

#-----------McElreath Qs------------------------

library(rethinking)
data(Howell1)
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
set.seed(1000)
i <- sample(1:nrow(d),size=nrow(d)/2)
d1 <- d[i,]
d2 <- d[-i,]

#I wrote these out the long way and played with the priors, but couldn't get the map function to work
#code from git user William Wolf
f1 <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- alpha + beta.1*age,
  c(alpha, beta.1) ~ dnorm(mean = 0, sd = 100),
  sigma ~ dunif(min = 0, max = 50)
)

f2 <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- alpha + beta.1*age + beta.2*age^2,
  c(alpha, beta.1, beta.2) ~ dnorm(mean = 0, sd = 100),
  sigma ~ dunif(min = 0, max = 50)
)

f3 <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3,
  c(alpha, beta.1, beta.2, beta.3) ~ dnorm(mean = 0, sd = 100),
  sigma ~ dunif(min = 0, max = 50)
)

f4 <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3 + beta.4*age^4,
  c(alpha, beta.1, beta.2, beta.3, beta.4) ~ dnorm(mean = 0, sd = 100),
  sigma ~ dunif(min = 0, max = 50)
)

f5 <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3 + beta.4*age^4 + beta.5*age^5,
  c(alpha, beta.1, beta.2, beta.3, beta.4, beta.5) ~ dnorm(mean = 0, sd = 100),
  sigma ~ dunif(min = 0, max = 50)
)

f6 <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3 + beta.4*age^4 + beta.5*age^5 + beta.6*age^6,
  c(alpha, beta.1, beta.2, beta.3, beta.4, beta.5, beta.6) ~ dnorm(mean = 0, sd = 100),
  sigma ~ dunif(min = 0, max = 50)
)


#starting values
alpha.start <- mean(d$height)
sigma.start <- sd(d$height)

m1 <- map(flist = f1, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0))
m2 <- map(flist = f2, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0, beta.2 = 0))
m3 <- map(flist = f3, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0, beta.2 = 0, beta.3 = 0))
m4 <- map(flist = f4, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0, beta.2 = 0, beta.3 = 0, beta.4 = 0))
m5 <- map(flist = f5, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0, beta.2 = 0, beta.3 = 0, beta.4 = 0, beta.5 = 0))
m6 <- map(flist = f6, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0, beta.2 = 0, beta.3 = 0, beta.4 = 0, beta.5 = 0, beta.6 = 0))

#6H1. Compare the models above, using WAIC. Compare the model rankings, as well as the WAIC
#weights.

compare(m1,m2,m3,m4,m5,m6) 
#ERROR: All inputs should have class 'loo'

WAIC(m1) # 2395
WAIC(m2) # 2149
WAIC(m3) # 1952
WAIC(m4) # 1926
WAIC(m5) # 1928.18
WAIC(m6) # 1928.20

#6H2. For each model, produce a plot with model averaged mean and 97% confidence interval of the
#mean, superimposed on the raw data. How do predictions differ across models?

#I know this isn't right, but I also don't know how to pull out the mu estimates using the map function
#So I just plotted the mus with the full equations. 
plot(height~age, data=d2)
curve((138.43 + 18.65*x), add=T)
curve((152.37+25.79*x+-14.10*x^2),add=T,col="red")
curve((158.11+11.36*x+-24.1*x^2+8.08*x^3),add=T,col="blue")
curve((156.69+5.96*x+-19.28*x^2+12.35*x^3+-2.31*x^4),add=T,col="purple")
curve((156.98+5.67*x+-20.55*x^2+12.87*x^3+-1.68*x^4+-0.26*x^5),add=T,col="orange")
curve((156.97+3.44*x+-20.22*x^2+17.09*x^3+-2.61*x^4+-1.83*x^5+0.52*x^6),add=T,col="yellow")
#according to WAIC and the plots, m4 fits the data the best, though certainly overfits.
#But, the standard deviations get wider for beta estimates with each more-complex model.

#6H3. Now also plot the model averaged predictions, across all models. In what ways do the averaged
#predictions differ from the predictions of the model with the lowest WAIC value?

#Code by Jay
# ensemble from R code 6.30
m_ensemble <- ensemble(m1, m2, m3, m4, m5, m6, data = d2)
mu <- apply(m_ensemble$link, 2, mean)
mu.PI <- apply(m_ensemble$link, 2, PI)
height_ci <- apply(m_ensemble$sim, 2, PI)

# create the plot
age.seq2 <- seq(from=min(d2$age), to=max(d2$age), length.out = length(d2$age))
plot(height ~ age, data = d2, col=col.alpha(rangi2,0.5))
line(age.seq2, mu)
shade(mu.PI , age.seq2)
shade(height_ci, age.seq2)


