test_that("plotting works", {
  input_spg = wrangleSpgTable(
    makeSpgTable(
      readPocSampleData(
        googledrive::as_id("12vNFDYa-EMSPjDRGAjOTniF54kC_nEgV"))))
  
  pl_res = plotSpgComparison(input_spg, input_spg)
  
  expect_type(pl_res, "list")
  expect_length(pl_res, 3)
  expect_all_equal(sapply(pl_res, function(x) paste0(names(x), collapse = "-")),
                   paste0(c("data","layers","scales","guides","mapping","theme",
                     "coordinates","facet", "plot_env","layout","labels"),
                     collapse = "-"))
})
