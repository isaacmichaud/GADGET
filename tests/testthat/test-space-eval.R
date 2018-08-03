context("space_eval")

my_lhs1  <- space_fill(0,50,10)
foo1     <- function(x){x}
my_lhs2  <- space_fill(rep(0,5),rep(50,5),10)
foo2     <- function(x){sum(x)}
perm     <- c(2,1,3,5,4)
foo3     <- function(x){x[perm]}


test_that("space_eval univariate output", {
  expect_equal(prod(space_eval(my_lhs1,foo1) == my_lhs1),1)
  expect_equal(prod(space_eval(my_lhs2,foo2) == rowSums(my_lhs2)),1)
}
)

test_that("space_eval multidimensional output", {
  result <- space_eval(my_lhs2,foo3)
  p_lhs  <- my_lhs2[,perm]
  expect_equal(prod(p_lhs == t(result)),1)
  }
)