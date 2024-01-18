data = read.csv("white_fishes_data.csv")
y = table(data$WHIBIN, data$BOTTOMCOV)
colnames(y) <- c("COV0","COV1")
rownames(y) <- c("y0","y1")

#this is similar to the example with female birth, so following the example.

y
n0 <- y[[1]] + y[[2]]
succ0 <- y[[2]]

#naive approach:
succ0/n0

x <- seq(0.65, 0.85, length=101)

p0 <- dbeta(x, succ0+1, n0-succ0+1)

plot(x,p0, main="Posterior for theta=0, Uniform prior", xlab="th", ylab="p(th|N,y)", type="l")

n1 <- y[[3]] + y[[4]]
succ1 <- y[[4]]

#naive approach:
succ1/n1

x1 <- seq(0.43, 0.63, length=101)

p1 <- dbeta(x1, succ1+1, n1-succ1+1)

plot(x1,p1, main="Posterior for theta=1, Uniform prior", xlab="th", ylab="p(th|N,y)", type="l")

s_th0 <- rbeta(10000, succ0+1, n0-succ0+1)
s_th1 <- rbeta(10000, succ1+1, n1-succ1+1)

succ0
n0
succ1
n1

#posterior mean theta 0
mean(s_th0)

#standard deviation theta 0
sd(s_th0)

#posterior mean theta 1
mean(s_th1)

#standard deviation theta 1
sd(s_th1)

hist(s_th0, main="Samples for posterior theta=0")
hist(s_th1, main="Samples for posterior theta=1")

diff <- s_th0 - s_th1
hist(diff, main="Distribution of difference between theta=0 and theta=1")


sum <- 0
for (i in diff){
  if (i <= 0){
    sum <- sum +1
  }
}
print(sum)

#In this case, all elements in the sample for theta=1 are smaller thank theta=0, so the probability
#is 1. In practice, the probability should be given as 0.9999 (extremely small).