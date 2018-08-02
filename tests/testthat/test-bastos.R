context("bastos_ohangan_09")

test_that("Two-Input Toy Example", {
  expect_lt(abs(bo09_toy(c(1,1)) - 4.005316),1e-5)
  expect_equal(bo09_toy(c(0,0)),3)
  expect_lt(abs(bo09_toy(c(0,1)) -  1.180408),1e-5)
  expect_lt(abs(bo09_toy(c(0.5,0.5)) -  7.405124),1e-5)
  expect_lt(abs(bo09_toy(c(1,0)) - 10.17949),1e-5)
}
)
