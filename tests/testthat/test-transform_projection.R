context('transform projection')

init_dataframe <- data.frame(Y = c(48.6476, 48.6447, 48.6709),
                             X = c(16.7958, 6.7962, 16.5370),
                             meta = c('a', 'b', 'c'))

spatpoints <- sp::SpatialPointsDataFrame(init_dataframe[c('X', 'Y')], init_dataframe['meta'])

#function
spatpoints_lambert <- transform_projection(spatpoints, '31287',
                                               epsg_old = '4326')

#manual
sp::proj4string(spatpoints) <- sp::CRS("+init=epsg:4326")
CRS.new <- sp::CRS("+init=epsg:31287")
spatpoints_lambert2 <- sp::spTransform(spatpoints, CRS.new)

test_that('transform_projection has same output as manual approach', {
  expect_true(identical(spatpoints_lambert, spatpoints_lambert2))
})

