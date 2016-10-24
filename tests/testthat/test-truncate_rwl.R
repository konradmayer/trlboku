context('truncate rwl')

n <- 15
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {x = c(rep(NA, 3), runif(n - 5), rep(NA, 2));
               y = c(rep(NA, 2), runif(n - 5), rep(NA, 3))})

out <- dat[3:13, ]


test_that('truncate_rwl works as expected', {
  expect_that(truncate_rwl(dat), equals(out))
})

test_that('truncate_rwl handles input of wrong class', {
  expect_that(truncate_rwl(as.factor(1)),
              throws_error('x must be of class data.frame'))
  expect_that(truncate_rwl(c(1:3)),
              throws_error('x must be of class data.frame'))
  expect_that(truncate_rwl('a'),
              throws_error('x must be of class data.frame'))
  expect_that(truncate_rwl(NA),
              throws_error('x must be of class data.frame'))
  expect_that(truncate_rwl(NULL),
              throws_error('x must be of class data.frame'))
})