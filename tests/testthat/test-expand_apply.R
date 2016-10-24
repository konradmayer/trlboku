context('apply on expanding window')

#expand_apply.default
test_that('expand_apply.default works as expected', {
  expect_that(expand_apply(seq(1, 5, 0.5), 'sum'),
              equals(cumsum(seq(1, 5, 0.5))))
  expect_that(expand_apply(c(NA, NA, 1, 3, 2, 3, NA), 'sum'),
              equals(c(NA, NA, 1, 4, 6, 9, NA)))
})

test_that('expand_apply.default handles wrong input classes', {
  expect_that(expand_apply(c('a', 'b'), 'sum'),
              throws_error('x must be numeric'))
  expect_that(expand_apply(as.factor(c('a', 'b')), 'sum'),
              throws_error('x must be numeric'))
  expect_that(expand_apply(NA, 'sum'),
              throws_error('x must be numeric'))
  expect_that(expand_apply(NULL, 'sum'),
              throws_error('x must be numeric'))
})


#expand_apply.data.frame
n <- 10
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {x = runif(n); y = x + runif(n)})
out <- within(data.frame(row.names = seq.int(1601, length.out = n)),
                      {x = seq_len(n); y = seq_len(n)})
out2 <- within(data.frame(row.names = seq.int(1601, length.out = n)),
              {x = seq_len(n); y = c(NA, seq_len(n-1))})

test_that('expand_apply.data.frame works as expected', {
  expect_that(expand_apply(dat, 'length'), equals(out))
  dat[1, 1] <- NA
  expect_that(expand_apply(dat, 'length'), equals(out2))
})
