context("add_data")

my_pseudo_experiment <- list(lower=0,upper=50,batch=4,explore_budget=c(20,2),num_parms=1,next_batch=c(35,47,2,40))
my_next_batch        <- add_data(my_pseudo_experiment)

test_that("add_data produces the correct batch dimensions", {
  expect_equal(dim(my_next_batch)[1],4)
  expect_equal(dim(my_next_batch)[2],1)
  expect_equivalent(add_data(my_pseudo_experiment), matrix(c(35,47,2,40),ncol=1,nrow=4)) 
}
)

test_that("add_data accpets correct sized data", {
  expect_error(add_data(my_pseudo_experiment,c(1,2,3,4), confirm = FALSE))
  expect_output(add_data(my_pseudo_experiment,matrix(c(1,2,3,4),ncol = 1, nrow = 4), confirm = FALSE))
}
)