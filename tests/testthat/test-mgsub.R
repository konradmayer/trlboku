context('generalization of gsub')

test_that('mgsub returns correct values for correct input', {
  expect_that(mgsub(list(c('e', 'ff')), 'test'), equals('tffst'))
  expect_that(mgsub(list(c('e', 'ff'), c('s', 'r')), 'test'), equals('tffrt'))
  #expect_that(mgsub(list(), 'test'), equals('test'))
})

test_that('mgsub handles wrong input for myrepl', {
  expect_that(mgsub(list(), 'test'), equals('test'))
  expect_that(mgsub(list(c('e', 'ff', 'a')), 'test'), throws_error())
  expect_that(mgsub(list('e', 1), 'test'), throws_error())
})

test_that('mgsub handles wrong input for mystring', {
  expect_that(mgsub(list(c('e', 'ff')), 1), throws_error())
  expect_that(mgsub(list(c('e', 'ff')), NA), throws_error())
  expect_that(mgsub(list(c('e', 'ff')), NULL), throws_error())
  expect_that(mgsub(list(c('e', 'ff')), as.factor('test')), throws_error())
})
