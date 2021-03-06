context('round down for year numbers')

test_that('round_down manages numbers one to four digit numbers', {
  expect_that(round_down(1, 10), equals(0))
  expect_that(round_down(11, 10), equals(10))
  expect_that(round_down(101, 10), equals(100))
})

test_that('round_down manages negative numbers', {
  expect_that(round_down(-1, 10), equals(-10))
  expect_that(round_down(-11, 10), equals(-20))
  expect_that(round_down(-101, 10), equals(-110))
})

test_that('round_down gives correct output with wrong input', {
  expect_that(round_down('a', 10), throws_error('non-numeric argument to binary operator'))
  expect_that(round_down(NULL, 10), equals(numeric(0)))
  expect_true(is.na(round_down(NA, 10)))
  expect_that(round_down(as.factor(1), 10), throws_error('input must be numeric'))
})

test_that('round_down works for vectors', {
  expect_that(round_down(c(1, 11, 111), 10), equals(c(0, 10, 110)))
})


test_that('round_down handles wrong input for to', {
  expect_true(is.na(round_down(1, NA)))
  expect_that(round_down(1, 'a'), throws_error('non-numeric argument to binary operator'))
  expect_that(round_down(1, NULL), throws_error('input must be numeric'))
  expect_that(round_down(1, as.factor(1)), throws_error('input must be numeric'))
})
