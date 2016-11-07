context('combine_fragments')

rwl <- data.frame('AbcAA01f' = c(1, 1, NA, NA, NA),
                  'AbcAA01g' = c(NA, NA, 1, 1, NA),
                  'AbcAA01h' = c(NA, NA, NA, NA, 1),
                  'AbcAA01m' = c(2, 2, 2, NA, NA),
                  'AbcAA01n' = c(NA, NA, NA, 2, 2),
                  'XyzPA01a' = c(1, 1, 1, 1, 1),
                  'XyzPA01m' = c(1, 1, 1, 1, 1),
                  'XyzPA01n' = c(2, 2, 2, 2, 2))

out <- data.frame('AbcAA01a' = rep(1, 5),
                  'AbcAA01b' = rep(2, 5),
                  'XyzPA01a' = rep(1, 5),
                  'XyzPA01b' = rep(1.5, 5))


test_that('combine_fragments averages correctly incl. NA handling', {
  expect_that(combine_fragments(rwl), equals(out))
})

test_that('handles wrong input for rwl', {
  expect_that(combine_fragments(as.matrix(rwl)),
              throws_error('rwl must be a data.frame'))
  expect_that(combine_fragments(c(1,2,3)),
              throws_error('rwl must be a data.frame'))
  expect_that(combine_fragments(as.factor(c(1,2,3))),
              throws_error('rwl must be a data.frame'))
  expect_that(combine_fragments(NULL),
              throws_error('rwl must be a data.frame'))
  expect_that(combine_fragments(NA),
              throws_error('rwl must be a data.frame'))
})

test_that('handles wrong input for stc', {
  expect_that(combine_fragments(rwl, stc = NA),
              throws_error('stc must be a numeric vector of length 3'))
  expect_that(combine_fragments(rwl, stc = NULL),
              throws_error('stc must be a numeric vector of length 3'))
  expect_that(combine_fragments(rwl, stc = c('a', 'b', 'c')),
              throws_error('stc must be a numeric vector of length 3'))
  expect_that(combine_fragments(rwl, stc = factor(c(1, 2, 3))),
              throws_error('stc must be a numeric vector of length 3'))
  expect_that(combine_fragments(rwl, stc = c(2, 4, 1)),
              throws_error('number of characters in series names must equal the sum of stc'))
})

test_that('handles different rules', {
  expect_that(combine_fragments(rwl, rules = c('a' = '[af-l]$',
                                               'b' = '[bm-w]$',
                                               'c' = '[c]$',
                                               'd' = '[dx-z]$',
                                               'x' = 'ab')),
              equals(out))
})
