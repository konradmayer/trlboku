context('first_last')

n <- 10
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {x = runif(n); y = x + runif(n)})

out <- data.frame(series = c('y', 'x'), first = c(1601, 1601),
                  last = c(1610, 1610), stringsAsFactors = FALSE)


test_that('first_last works with valid input', {
  expect_that(first_last(dat), equals(out))
})

test_that('first_last handles wrong rownames', {
  rownames(dat)[5] <- 'test'
  expect_that(first_last(dat),
              throws_error('please provide an input object with correct rownames'))
})

test_that('first_last works with NA', {
  dat[1:3,'x'] <- NA
  dat[8:10, 'y'] <- NA
  out <- data.frame(series = c('y', 'x'), first = c(1601, 1604),
                    last = c(1607, 1610), stringsAsFactors = FALSE)
    expect_that(first_last(dat), equals(out))
})


test_that('first_last handles input of wrong class', {
  expect_that(first_last(as.factor(1)),
              throws_error('please provide a data.frame/rwl object'))
  expect_that(first_last(c(1:3)),
              throws_error('please provide a data.frame/rwl object'))
  expect_that(first_last('a'),
              throws_error('please provide a data.frame/rwl object'))
  expect_that(first_last(NA),
              throws_error('please provide a data.frame/rwl object'))
  expect_that(first_last(NULL),
              throws_error('please provide a data.frame/rwl object'))
})
