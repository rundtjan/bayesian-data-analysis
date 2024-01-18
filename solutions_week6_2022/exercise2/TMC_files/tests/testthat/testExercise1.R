library("testthat")

points_for_all_tests(character(0))

test("exercise 1.1 is solved correctly", character(0), {
  expect_equal(c(a1_dot[3005], b1_dot[2341],  sigma21_dot[5211]), c(0.01278223, 0.99470559 , 0.02186823), tolerance=1e-6, scale=1)
})

test("exercise 1.2 is solved correctly", character(0), {
  expect_equal(c(a_dot[3005], b_dot[2341], c_dot[321], sigma2_dot[5211]), c(-0.10966659,  0.98294125,  0.13198381,  0.01022179), tolerance=1e-6, scale=1)
  expect_equal(as.vector(c(mean_mu[840], int_mu[421,1], int_mu[421,2], mean_y[1], int_y[328,1], int_y[328,2])),  c( 8.662984e+04, 2.205398e+04 ,2.205398e+04 ,9.903181e-01, 1.239679e+04 ,1.443683e+04), tolerance=1e-1, scale=1)
})

test("exercise 1.3 is solved correctly", character(0), {
  expect_equal(y.tilde[264,3113],  8534.384, tolerance=1e-2, scale=1)
})

test("exercise 1.4 is solved correctly", character(0), {
  expect_equal(c(lpd1, rmse1, lpd2, rmse2),  c(144.2783, 0.1503274, 255.91146, 0.1038790), tolerance=1e-2, scale=1)
})
