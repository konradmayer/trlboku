context('round up for year numbers')
test_that('round_up manages numbers one to four digit numbers', {
  expect_that(round_up(1, 10), equals(10))
  expect_that(round_up(11, 10), equals(20))
  expect_that(round_up(101, 10), equals(110))
})


test_that('round_up manages negative numbers', {
  expect_that(round_up(-1, 10), equals(0))
  expect_that(round_up(-11, 10), equals(-10))
  expect_that(round_up(-101, 10), equals(-100))
})

test_that('round_up gives correct output with wrong input', {
  expect_that(round_up('a', 10), throws_error('non-numeric argument to binary operator'))
  expect_that(round_up(NULL, 10), equals(numeric(0)))
  expect_true(is.na(round_up(NA, 10)))
  expect_that(round_up(as.factor(1), 10), throws_error('input must be numeric'))
  })

test_that('round_up works for vectors', {
  expect_that(round_up(c(1, 11, 111), 10), equals(c(10, 20, 120)))
})


test_that('round_up handles wrong input for to', {
  expect_true(is.na(round_up(1, NA)))
  expect_that(round_up(1, 'a'), throws_error('non-numeric argument to binary operator'))
  expect_that(round_up(1, NULL), throws_error('input must be numeric'))
  expect_that(round_up(1, as.factor(1)), throws_error('input must be numeric'))
})
