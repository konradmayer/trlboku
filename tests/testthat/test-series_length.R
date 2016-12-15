context('series_length')

n <- 10
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {x = runif(n); y = x + runif(n)})

out <- c(y = 10, x = 10)


test_that('series_length works with valid input without NA', {
  expect_that(series_length(dat), equals(out))
})


test_that('series_length works with NA', {
  dat[1:3,'x'] <- NA
  dat[8:10, 'y'] <- NA
  out <- c(y = 7, x = 7)
  expect_that(series_length(dat), equals(out))
})


