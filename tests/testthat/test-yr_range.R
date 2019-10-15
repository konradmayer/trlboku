context('year ranges')

test_that('yr_range handles input of wrong class', {
  expect_that(yr_range(as.factor(1)),
              equals(as.double(c(NA, NA))))
  expect_that(yr_range(c(1:3)),
              equals(as.double(c(NA, NA))))
  expect_that(yr_range('a'),
              equals(as.double(c(NA, NA))))
  expect_that(yr_range(NA),
              equals(as.double(c(NA, NA))))
  expect_that(yr_range(NULL),
              equals(as.double(c(NA, NA))))

})


test_that('yr_range works as expected', {
  expect_that(yr_range(structure(1:10+0.5, names = as.character(1601:1610))),
              equals(c(1601, 1610)))
})

test_that('yr_range works also with NA values', {
  expect_that(yr_range(structure(c(NA, NA, 3:8+0.5, NA, NA),
                                 names = as.character(1601:1610))),
              equals(c(1603, 1608)))
})

test_that('yr_range works with negative years', {
  expect_that(yr_range(structure(1:10+0.5, names = as.character(-4:5))),
              equals(c(-4, 5)))
})

test_that('yr_range handles wrong names', {
  expect_that(yr_range(structure(1:10+0.5, names = c('a', as.character(1602:1610)))),
              gives_warning('NAs introduced by coercion'))
})
