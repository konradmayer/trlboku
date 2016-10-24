context('radius_rwl')

n <- 5
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {x = 1:n; y =1:n})


test_that('radius_rwl works with valid input', {
  expect_that(radius_rwl(dat), equals(cumsum(dat)))

  dat[1,'x'] <- NA
  dat[n, 'y'] <- NA
  out <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
                 {x = c(NA, 2, 5, 9, 14); y =c(1, 3, 6, 10, NA)})
  expect_that(radius_rwl(dat), equals(out))
})


test_that('radius_rwl handles input of wrong class', {
  expect_that(radius_rwl(as.factor(1)),
              throws_error('please provide input of class rwl or data.frame'))
  expect_that(radius_rwl(c(1:3)),
              throws_error('please provide input of class rwl or data.frame'))
  expect_that(radius_rwl('a'),
              throws_error('please provide input of class rwl or data.frame'))
  expect_that(radius_rwl(NA),
              throws_error('please provide input of class rwl or data.frame'))
  expect_that(radius_rwl(NULL),
              throws_error('please provide input of class rwl or data.frame'))
})
