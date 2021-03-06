context('whole number')
test_that('is.wholenumber returns correct values for correct input', {
  expect_true(trlboku:::is.wholenumber(1))
  expect_false(trlboku:::is.wholenumber(1.5))
})

test_that('is.wholenumber works for vectors', {
  expect_true(all(trlboku:::is.wholenumber(c(1, 2, 3))))
  expect_that(sum(trlboku:::is.wholenumber(c(1, 2.5, 3))), equals(2))
})

test_that('is.wholenumber works with negative numbers', {
  expect_true(trlboku:::is.wholenumber(-1))
  expect_that(sum(trlboku:::is.wholenumber(c(1, 2.5, -3))), equals(2))
})

test_that('is.wholenumber handles incorrect input', {
  expect_that(trlboku:::is.wholenumber('a'), throws_error('non-numeric argument to binary operator'))
  expect_true(is.na(trlboku:::is.wholenumber(NA)))
  expect_that(length(trlboku:::is.wholenumber(NULL)), equals(0))
  expect_that(trlboku:::is.wholenumber(as.factor(1)), throws_error('x needs to be numeric'))
})
