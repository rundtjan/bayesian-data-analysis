M=25
abs_min <- M # the number of balls cannot be negative
abs_max <- 1000 # No way that bag can contain more than 1000 balls (a subjective assumption)
Nseq <- seq(abs_min, abs_max, length=abs_max-abs_min+1)
par(mfrow=c(2,3)) # Open figure for plotting the examples
# uniform prior
Nprior <- rep(1,length(Nseq))/length(Nseq)
sum(Nprior) # check that prior probabilities sum up to to one
plot(Nseq,Nprior, main="Uniform prior", xlab="N", pch=16)
# "Gaussian" prior
Nprior <- dnorm(Nseq, mean=50, sd=20)
Nprior <- Nprior/sum(Nprior) # Normalize the prior probabilities to sum to one
sum(Nprior) # check that prior probabilities sum up to to one
plot(Nseq,Nprior, main="Gaussian prior", xlab="N", pch=16)
# log-Gaussian prior
Nprior <- dlnorm(Nseq, mean=5, sd=1)
Nprior <- Nprior/sum(Nprior) # Normalize the prior probabilities to sum to one
sum(Nprior) # check that prior probabilities sum up to to one
plot(Nseq,Nprior, main="log-Gaussian prior", xlab="N", pch=16)
# Step wise prior by giving different relative weights for different values
Nprior <- rep(1,length(Nseq))
Nprior[Nseq>50 & Nseq<600] <- 2
Nprior[Nseq>70 & Nseq<400] <- 4
Nprior[Nseq>200 & Nseq<300] <- 6
Nprior <- Nprior/sum(Nprior) # Normalize the prior probabilities to sum to one
sum(Nprior) # check that prior probabilities sum up to to one
plot(Nseq,Nprior, main="Step-wise prior", xlab="N", pch=16)
# --- Here we will fill in the prior defined during the lecture ---
Nprior <- dlnorm(Nseq, mean=5.05, sd=1.1)
Nprior <- Nprior/sum(Nprior) # Normalize to sum to one
sum(Nprior) # check that prior probabilities sum up to to one
plot(Nseq,Nprior, main="My own prior", xlab="N", pch=16)
# The result from the other sampling time
C=25
R=9
Nposterior <- Nprior*dbinom(R,C,M/Nseq) # numerator of Bayes theorem
Nposterior <- Nposterior/sum(Nposterior) # divide by marginal likelihood
plot(Nseq,Nposterior, main="The posterior distribution", xlab="N", pch=16)

posteriorMean = sum(Nposterior*Nseq)
print(posteriorMean)

NposteriorCDF <- cumsum(Nposterior)
# Plot CDF
plot(Nseq,NposteriorCDF, main="posterior CDF", xlab="N", pch=16)


#Median
Nseq[which(NposteriorCDF>=0.5)[1]]

#Mean for calculating variance
posteriorMean
#E(X^2) for the variance formula:
EX2 <- sum(Nposterior*Nseq^2)
variance <- EX2 - posteriorMean^2
variance

#standard deviation is the square root
standd <- sqrt(variance)
standd
sum(Nprior*(Nseq-posteriorMean)) #can this be the variance?!

#80% confidence interval, start and end:
Nseq[which(NposteriorCDF>=0.1)[1]]
Nseq[which(NposteriorCDF>=0.9)[1]]

#sampling:
Nsamp = Nseq[sapply(runif(1000,min=0,max=1),function(temp){ which(NposteriorCDF>=temp)[1] })]
#80% central posterior interval
quantile(Nsamp,probs=c(0.1,0.9))
#variance
variance2 <- var(Nsamp)
variance2
standd2 <- sd(Nsamp)
standd2
median2 <- median(Nsamp)
median2

#the sample statistics will converge towards the true values as n -> eternal, but here we still have a quite limited 1000n sample.
