    library("testthat")

points_for_all_tests(character(0))


own_expect_equal_vec <- function(current, target, tolerance = 1e-6) {
  current_name <- deparse(substitute(current))
  target_name <- deparse(substitute(target))
  test_current <- abs(current - target) < tolerance
  target_correct <- rep(TRUE, length(target))
  assign(current_name, test_current)
  assign(target_name, target_correct)
  do.call("expect_equal", list(as.name(current_name),
                               as.name(target_name)))
}



set.seed(123)

# 1)
setwd("/home/piailari/Documents/BayesianDataAnalysis/exercises/week7/exercise1")
post_samples_correct1 = read.csv("param1.txt", sep = "")
a1_correct = post_samples_correct1[,1]
b1_correct = post_samples_correct1[,2]
sigma21_correct = post_samples_correct1[,3]

test("Hidden exercise 1.1 is solved correctly", c("W7.E1.1"), {
  own_expect_equal_vec(a1_dot, a1_correct)
  own_expect_equal_vec(b1_dot, b1_correct)
  own_expect_equal_vec(sigma21_dot, sigma21_correct)
  
})


# 2)
setwd("/home/piailari/Documents/BayesianDataAnalysis/exercises/week7/exercise1")
post_samples_correct = read.csv("param2.txt", sep = "")
a_correct = post_samples_correct[,1]
b_correct = post_samples_correct[,2]
c_correct = post_samples_correct[,3]
sigma2_correct = post_samples_correct[,4]


x.pred= seq(1,70*12,length=70*12)

mu_correct = matrix(NA,length(x.pred),length(b_correct))      # matrix of posterior samples of mu
y_correct = matrix(NA,length(x.pred),length(b_correct)) # matrix of posterior samples of y.tilde

mean.mu_correct=rep(NA, length(x.pred))              # posterior mean of mu
CI.mu_correct = matrix(NA,length(x.pred),2)         # posterior 95% interval of mu

mean.y_correct=rep(NA, length(x.pred))              # posterior mean of y.tilde
CI.y_correct = matrix(NA,length(x.pred),2)         # posterior 95% interval of y.tilde

for (i in 1:length(x.pred)) {
  mu_correct[i,] = a_correct + b_correct*x.pred[i] + c_correct*x.pred[i]*x.pred[i]
  mean.mu_correct[i] = mean(mu_correct[i,])
  CI.mu_correct[i,] = quantile(mu_correct[i], c(0.025,0.975))
  y_correct[i,] =  mu_correct[i,] + rnorm(length(mu_correct[i,]), 0, sqrt(sigma2_correct))
  mean.y_correct[i] = mean(y_correct[i,])
  CI.y_correct[i,] = quantile(y_correct[i,], c(0.025,0.975))
}


test("Hidden exercise 1.2 is solved correctly", c("W7.E1.2"), {
  own_expect_equal_vec(a_dot, a_correct)
  own_expect_equal_vec(b_dot, b_correct)
  own_expect_equal_vec(c_dot, c_correct)
  own_expect_equal_vec(sigma2_dot, sigma2_correct)
  
  own_expect_equal_vec(mean_mu , mean.mu_correct)
  own_expect_equal_vec(as.vector(int_mu) , as.vector(CI.mu_correct))
  own_expect_equal_vec(mean_y , mean.y_correct)
  own_expect_equal_vec(as.vector(int_y) , as.vector(CI.y_correct))
  
})


# 3)
test("Hidden exercise 1.3 is solved correctly", c("W7.E1.3"), {
  expect_equal(y.tilde, y_correct)

})


# 4)

lpd1_correct = 144.2783 
rmse1_correct = 0.1503274
lpd2_correct = 255.9146 
rmse2_correct = 0.1038790

test("Hidden exercise 1.4 is solved correctly", c("W7.E1.4"), {
  own_expect_equal_vec(lpd1, lpd1_correct,0.1)
  own_expect_equal_vec(rmse1, rmse1_correct,0.001)
  own_expect_equal_vec(lpd2, lpd2_correct, 0.1)
  own_expect_equal_vec(rmse2, rmse2_correct, 0.001)
  
})

