context("space_fill")

my_lhs1               <- space_fill(0,50,10)
my_lhs2               <- space_fill(rep(0,5),rep(50,5),10)

test_that("space_fill returns correct size design", {
  expect_equal(dim(my_lhs1)[1],10)
  expect_equal(dim(my_lhs1)[2],1)
  expect_equal(dim(my_lhs2)[1],10)
  expect_equal(dim(my_lhs2)[2],5)
}
)

test_that("space_fill returns points within design range", {
  expect_lte(max(my_lhs1),50)
  expect_gte(min(my_lhs1),0)
  expect_lte(max(my_lhs2),50)
  expect_gte(min(my_lhs2),0)
}
)