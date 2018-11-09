context("Market value capping")

test_that("3 names, no capping",{
  expect_equal(market_capping(c(1:3), rep(100,3), step=1)[,"capped_mv_wts"], c(1/6, 1/3, 1/2) * 100)
})


test_that("3 names, 1 capped",{
  expect_equal(market_capping(c(1:3), c(2, 100, 100), step=1)[,"capped_mv_wts"], c(0, 40, 60))
})

test_that("3 names, 2 capped",{
  expect_equal(market_capping(c(1:3), c(2, 2, 100), step=1)[,"capped_mv_wts"], c(0, 0, 10))
})

#market_capping(1:10, rep(100, 10))
# market_capping(1:3, c(100, 100, 100), debug=TRUE, step=1)
# market_capping(rep(25, 4), c(2, 100, 100, 100), debug=TRUE)

mvs<- c(500407906.00,
        581272702.00,
        483637513.00,
        2470721801.00,
        1635571759.00,
        2154268200.00,
        10256050102.00,
        490988281.00,
        765738897.00,
        1401132226.00,
        2274879731.00,
        10552812685.00
)

caps <- c(1,
          1,
          1,
          1,
          1,
          0.05,
          0.15,
          1,
          1,
          0.15,
          1,
          1
)

#market_capping(mvs, caps, debug=TRUE)
