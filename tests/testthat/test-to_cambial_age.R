context('align series by cambial age')

n <- 10
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {AbcAA01a = c(rep(NA, 3), seq_len(n-3)); AbcAA01b =seq_len(n);
               AbcAA02a = seq_len(n); AbcAA02b = c(rep(NA, 2), seq_len(n-2))})
po <- data.frame(series = names(dat), po = 1:4, stringsAsFactors = FALSE)

test_that('to_cambial_age works with and without supplying po', {
  expect_that(first_last(to_cambial_age(dat))$first, equals(rep(1,4)))
  expect_that(first_last(to_cambial_age(dat, po))$first, equals(1:4))
})


test_that('to_cambial_age handles incorrect input', {
  expect_that(to_cambial_age(1), throws_error('rwl must be of class data.frame'))
  expect_that(to_cambial_age(as.factor(1)), throws_error('rwl must be of class data.frame'))
  expect_that(to_cambial_age('1'), throws_error('rwl must be of class data.frame'))
  expect_that(to_cambial_age(NA), throws_error('rwl must be of class data.frame'))
  expect_that(to_cambial_age(NULL), throws_error('rwl must be of class data.frame'))

  expect_that(to_cambial_age(dat, NA), throws_error('po must be of class data.frame or NULL'))
  expect_that(to_cambial_age(dat, 1:4), throws_error('po must be of class data.frame or NULL'))
})

test_that('to_cambial_age handles wrong series names in po', {
  po[1, 1] <- 'test'
  expect_that(to_cambial_age(dat, po), throws_error('series names in po are not the same as provided in rwl'))
})

test_that('to_cambial_age handles wrong values in po', {
  po[1, 2] <- 'test'
  expect_that(to_cambial_age(dat, po), throws_error())
})
