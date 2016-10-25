context('stand depth')

n <- 10 # must be >3
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {AbcAA01a = seq_len(n);
               BcdAA01b = c(seq_len(n - 1), NA);
               CdeAA02a = c(seq_len(n - 3), rep(NA, 3));
               DefAA02b = c(rep(NA, 2), seq_len(n - 2))
               })


test_that('stand_depth gives correct output for valid input', {
  expect_that(stand_depth(dplR::chron(dat), dat)[ , 3],
  equals(c(3, 3, 4, 4, 4, 4, 4, 3, 3, 2)))
})

test_that('stand_depth handles incorrect input crn', {
  expect_that(stand_depth(NA, dat), throws_error("'crn' must be a data.frame"))
  expect_that(stand_depth(c(1,2,3), dat), throws_error("'crn' must be a data.frame"))
  expect_that(stand_depth(as.list(dplR::chron(dat)), dat), throws_error("'crn' must be a data.frame"))
  expect_that(stand_depth(factor(1), dat), throws_error("'crn' must be a data.frame"))
  expect_that(stand_depth(c('a', 'b'), dat), throws_error("'crn' must be a data.frame"))
  })

test_that('stand_depth handles incorrect input rwl', {
  expect_that(stand_depth(dplR::chron(dat), NA), throws_error("'rwl' must be a data.frame"))
  expect_that(stand_depth(dplR::chron(dat), c(1,2,3)), throws_error("'rwl' must be a data.frame"))
  expect_that(stand_depth(dplR::chron(dat), as.list(dat)), throws_error("'rwl' must be a data.frame"))
  expect_that(stand_depth(dplR::chron(dat), factor(1)), throws_error("'rwl' must be a data.frame"))
  expect_that(stand_depth(dplR::chron(dat), c('a', 'b')), throws_error("'rwl' must be a data.frame"))
})

test_that('stand_depth handles incorrect input stand', {
  expect_that(stand_depth(dplR::chron(dat), dat, stand = c(1)), throws_error("stand needs to be a numeric vector of length 2"))
  expect_that(stand_depth(dplR::chron(dat), dat, stand = c('a', 'b')), throws_error("stand needs to be a numeric vector of length 2"))
  expect_that(stand_depth(dplR::chron(dat), dat, stand = factor(c(1, 2))), throws_error("stand needs to be a numeric vector of length 2"))

  })
