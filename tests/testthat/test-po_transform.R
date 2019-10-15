context('transform pith offset')

n <- 10
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {AbcAA01a = c(rep(NA, 3), rep(5, n-3)); AbcAA01b =rep(3, n);
               AbcAA02a = rep(7, n); AbcAA02b = c(rep(NA, 2), rep(8, n-2))})
po <- data.frame(series = names(dat), po = c(8*3, 7*3, 3*3, 5*3), stringsAsFactors = FALSE)

test_that('po_transform gives correct output values', {
  expect_that(po_transform(po, dat)$po.new, equals(rep(3, 4)))
})

test_that('po_transform handles wrong input classes', {
  expect_that(po_transform(as.matrix(po), dat), throws_error('po and rwl must be data frames'))
  expect_that(po_transform(1, dat), throws_error('po and rwl must be data frames'))
  expect_that(po_transform('a', dat), throws_error('po and rwl must be data frames'))
  expect_that(po_transform(factor(1), dat), throws_error('po and rwl must be data frames'))
  expect_that(po_transform(NA, dat), throws_error('po and rwl must be data frames'))
  expect_that(po_transform(NULL, dat), throws_error('po and rwl must be data frames'))
  expect_that(po_transform(po, as.matrix(dat)), throws_error('po and rwl must be data frames'))
  expect_that(po_transform(po, 1), throws_error('po and rwl must be data frames'))
  expect_that(po_transform(po, 'a'), throws_error('po and rwl must be data frames'))
  expect_that(po_transform(po, factor(1)), throws_error('po and rwl must be data frames'))
  expect_that(po_transform(po, NA), throws_error('po and rwl must be data frames'))
  expect_that(po_transform(po, NULL), throws_error('po and rwl must be data frames'))
  })
