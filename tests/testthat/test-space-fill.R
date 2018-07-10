context("space_fill")

my_pseudo_experiment <- list(lower=0,upper=50,batch=4,explore_budget=c(20,2),num_parms=1)
my_lhs               <- GADGET:::space_fill(my_pseudo_experiment)

test_that("space_fill returns correct size design", {
  expect_equal(dim(my_lhs)[1],20)
  expect_equal(dim(my_lhs)[2],4)
}
)

test_that("space_fill returns points within design range", {
  expect_lte(max(my_lhs),50)
  expect_gte(min(my_lhs),0)
}
)