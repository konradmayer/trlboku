context('po_find')

rc <- exp(-seq(1,10,0.1))

series1 <- data.frame(series1 = rc[5:20], row.names = 1600 + 5:20)
series2 <- data.frame(series2 = rc[15:35], row.names = 1600 + 15:35)

dat <- dplR::combine.rwl(series1, series2)

test_that('po_finds position of cut out fragments of neg. exp. curve', {
  expect_that(po_find(dat, rc, make.plot = FALSE)$po, equals(c(5, 15)))
  expect_that(po_find(dat[1], rc, make.plot = FALSE)$po, equals(c(5)))
  expect_that(po_find(dat[1], as.matrix(rc), make.plot = FALSE)$po, equals(c(5)))

  })


test_that('po_find handles wrong input classes', {
  expect_that(po_find(as.matrix(dat), rc, make.plot = FALSE), throws_error('rwl must be a data.frame'))
  expect_that(po_find(1, rc, make.plot = FALSE), throws_error('rwl must be a data.frame'))
  expect_that(po_find('a', rc, make.plot = FALSE), throws_error('rwl must be a data.frame'))
  expect_that(po_find(factor(1), rc, make.plot = FALSE), throws_error('rwl must be a data.frame'))
  expect_that(po_find(NA, rc, make.plot = FALSE), throws_error('rwl must be a data.frame'))
  expect_that(po_find(NULL, rc, make.plot = FALSE), throws_error('rwl must be a data.frame'))
  expect_that(po_find(dat, 1, make.plot = FALSE), throws_error('rc needs to be a numeric vector'))
  expect_that(po_find(dat, 'a', make.plot = FALSE), throws_error('rc needs to be a numeric vector'))
  expect_that(po_find(dat, factor(1), make.plot = FALSE), throws_error('rc needs to be a numeric vector'))
  expect_that(po_find(dat, NA, make.plot = FALSE), throws_error('rc needs to be a numeric vector'))
  expect_that(po_find(dat, NULL, make.plot = FALSE), throws_error('rc needs to be a numeric vector'))
})
