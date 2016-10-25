context('new_date_end')

n <- 10
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {AbcAA01a = c(rep(NA, 3), seq_len(n-3)); AbcAA01b =seq_len(n);
               AbcAA02a = seq_len(n); AbcAA02b = c(rep(NA, 2), seq_len(n-2))})
date.end <- data.frame(series = names(dat), de = c(1600, 1700, 1800, 1900), stringsAsFactors = FALSE)


test_that('new_date_end returns correct values', {
  expect_that(first_last(new_date_end(dat, date.end))$last, equals(c(1600, 1700, 1800, 1900)))
  date.end[ , 2] <- -date.end[ , 2]
  expect_that(first_last(new_date_end(dat, date.end))$last, equals(-c(1600, 1700, 1800, 1900)))
})


test_that('new_date_end handles wrong input', {
  expect_that(new_date_end(1:4, date.end), throws_error('please provide input data with correct class'))
  expect_that(new_date_end(NA, date.end), throws_error('please provide input data with correct class'))
  expect_that(new_date_end(dat, NA), throws_error('please provide input data with correct class'))
  expect_that(new_date_end(c('a', 'b'), date.end), throws_error('please provide input data with correct class'))
  expect_that(new_date_end(as.matrix(dat), date.end), throws_error('please provide input data with correct class'))
  expect_that(new_date_end(dat,as.matrix(date.end)), throws_error('please provide input data with correct class'))
  })
