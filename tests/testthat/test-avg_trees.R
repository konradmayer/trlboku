context('avg_trees')

n <- 10
dat <-  within(data.frame(row.names = seq.int(1601, length.out = n)),
               {AbcAA01a = seq_len(n); AbcAA01b =seq_len(n);
               AbcAA02a = seq_len(n)*2; AbcAA02b =seq_len(n)*3})
out <- as.data.frame(cbind(apply(dat[1:2], 1, mean), apply(dat[3:4], 1, mean)))
names(out) <- c('AbcAA02', 'AbcAA01')
class(out) <- c('rwl', 'data.frame')

test_that('avg_trees works as expected', {
  expect_that(avg_trees(dat, stc = c(3,4,1)), equals(out))
  dat[1:2, 4] <- NA
  expect_that(avg_trees(dat, stc = c(3,4,1)), equals(out))
})

test_that('avg_trees handles wrong input classes', {
  expect_that(avg_trees(c('a', 'b'), stc = c(3,4,1)),
              throws_error('please provide an object of class rwl or data.frame'))
  expect_that(avg_trees(as.factor(c('a', 'b')), stc = c(3,4,1)),
              throws_error('please provide an object of class rwl or data.frame'))
  expect_that(avg_trees(NA, stc = c(3,4,1)),
              throws_error('please provide an object of class rwl or data.frame'))
  expect_that(avg_trees(NULL, stc = c(3,4,1)),
              throws_error('please provide an object of class rwl or data.frame'))
})

