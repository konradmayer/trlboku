context('age_rwl')

test_that('age_rwl handles wrong input classes', {
  expect_that(age_rwl(c('a', 'b')),
              throws_error('please provide input of class rwl or data.frame'))
  expect_that(age_rwl(as.factor(c('a', 'b'))),
              throws_error('please provide input of class rwl or data.frame'))
  expect_that(age_rwl(NA),
              throws_error('please provide input of class rwl or data.frame'))
  expect_that(age_rwl(NULL),
              throws_error('please provide input of class rwl or data.frame'))
})



n <- 10
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {x = runif(n); y = x + runif(n)})
out <- within(data.frame(row.names = seq.int(1601, length.out = n)),
              {x = seq_len(n); y = seq_len(n)})
out2 <- within(data.frame(row.names = seq.int(1601, length.out = n)),
               {x = seq_len(n); y = c(NA, seq_len(n-1))})

test_that('age_rwl works as expected', {
  expect_that(age_rwl(dat), equals(out))
  dat[1, 1] <- NA
  expect_that(age_rwl(dat), equals(out2))
})
