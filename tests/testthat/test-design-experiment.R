context("design_experiment")

dc <- function(x){x}

test_that("input checks", {
  expect_error(design_experiment(dc,stochastic = TRUE,lower_bound = c(1,1), upper_bound = c(0,0)))
  expect_error(design_experiment(dc,stochastic = TRUE,lower_bound = c(0,0), upper_bound = c(1)))
}
)

# test_that("space_eval multidimensional output", {
#   result <- space_eval(my_lhs2,foo3)
#   p_lhs  <- my_lhs2[,perm]
#   expect_equal(prod(p_lhs == t(result)),1)
# }
# )