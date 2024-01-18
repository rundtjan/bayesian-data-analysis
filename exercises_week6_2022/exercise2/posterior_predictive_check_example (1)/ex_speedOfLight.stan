// Stan model file for Exercise 4.1
data{
    int N; //the number of observations
    vector[N] y; //data recorded as deviations from 24800 nanoseconds
}
parameters {
  real mu; //mean 
  real<lower=0> tau; //precision
}
model{
  mu~normal(0,sqrt(10^6));
  tau~gamma(2,2);
  y ~ normal(mu,sqrt(1/tau));//or with for loop
  
}
generated quantities{
      real<lower=0> sigma2;   
      vector[N] y_rep; //replicates
      
      sigma2=1/tau;
      for (i in 1:N){
        y_rep[i]=normal_rng(mu,sqrt(1/tau));
      }
}
