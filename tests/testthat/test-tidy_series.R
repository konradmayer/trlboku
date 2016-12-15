context('tidy_series')

n <- 5
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {AbcAA01a = seq_len(n)+3*n; AbcAA01b =seq_len(n)+2*n;
               AbcAA02a = seq_len(n)+n; AbcAA02b =seq_len(n)})
out <- tibble::tibble(year=as.integer(rep(seq.int(1601, length.out = n), 4)),
              series = rep(c('AbcAA02b', 'AbcAA02a', 'AbcAA01b', 'AbcAA01a'),
                           each = n), value = as.double(1:(n*4)))

test_that('tidy_series works as expected', {
  expect_that(tidy_series(dat), equals(out))
})


test_that('tidy_series handles incorrect input', {
  expect_that(tidy_series(1:3), throws_error('rwl must be of class data.frame or rwl'))
  expect_that(tidy_series('a'), throws_error('rwl must be of class data.frame or rwl'))
  expect_that(tidy_series(as.factor(1)), throws_error('rwl must be of class data.frame or rwl'))
  })
