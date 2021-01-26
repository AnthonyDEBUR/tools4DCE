test_that("legend_LQ returns a cowplot object", {
  res <- legend_LQ()
  expect_type(res, "list")
  expect_that(res, is_a("gtable") , label = "legend_LQ should return a gtable")
})
