#Task 2
#Creating 100 bins between 0 and 3:
Nseq <- seq(0, 3, length=100)

Nprior <- dgamma(Nseq, 1, 1)

likelihood <- function(lamb){
  dpois(3, lamb*6.56) + dpois(4, lamb*6.56) + dpois(5, lamb*6.56) + dpois(6, lamb*6.56)
}

Nposterior <- Nprior * likelihood(Nseq)

Nposterior <- Nposterior/sum(Nposterior)

#The posterior density at the 100 bins:
plot(Nseq,Nposterior, main="Posterior", xlab="N", pch=16)

#Task 3
NposteriorCDF <- cumsum(Nposterior)

#The posterior cumulative distribution
plot(Nseq,NposteriorCDF, main="posterior CDF", xlab="N", pch=16)

#probability for over 1
1- NposteriorCDF[34]

#Task 4
likelihood2 <- function(lamb){
  dpois(6, lamb*6.56)
}

Nposterior2 <- Nprior * likelihood2(Nseq)
Nposterior2 <- Nposterior2/sum(Nposterior2)
Nposterior2
plot(Nseq,Nposterior2, main="Posterior with 6 besettings", xlab="N", pch=16)
#Answer: the probability is centered around a higher value of lambda

#Task 5

y_tildes <- c(0,0,0,0,0)

for (y in 0:4){
  sum <- 0
  for (i in 1:length(Nseq)){
    sum <- sum + dpois(y, Nseq[i]*0.5)*Nposterior[i]

  }
  y_tildes[y+1] <- sum
  print(y)
  print(sum)
}
print(y_tildes)

#A plot of the density
plot(c(0,1,2,3,4),y_tildes,type='b', main="Y_tildes in posterior prediction", xlab="N", pch=16)
