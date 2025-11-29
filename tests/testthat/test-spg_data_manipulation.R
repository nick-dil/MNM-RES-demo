test_that("makeSpgTable works", {
  poc_table = readPocSampleData(googledrive::as_id("12vNFDYa-EMSPjDRGAjOTniF54kC_nEgV"))
  expect_equal(colnames(makeSpgTable(poc_table)), c("meetnet","type","hydr_class","steekproefgrootte","id"))
  
})

test_that("makeSpgTable aggregates correctly", {
  poc_table = readPocSampleData(googledrive::as_id("12vNFDYa-EMSPjDRGAjOTniF54kC_nEgV"))

  expect_all_equal(duplicated(makeSpgTable(poc_table)[,c(1,2,3)]), FALSE)
})

test_that("wrangle does its job", {
  poc_table = readPocSampleData(googledrive::as_id("12vNFDYa-EMSPjDRGAjOTniF54kC_nEgV"))
  
  spg_table = makeSpgTable(poc_table)
  
  wr_table = wrangleSpgTable(spg_table)
  
  expect_lte(sum(wr_table$steekproefgrootte), sum(spg_table$steekproefgrootte))
  expect_all_equal(duplicated(wr_table[,c(1,2,3)]), FALSE)
  expect_equal(colnames(wr_table), c("meetnet","hydr_class","id","steekproefgrootte"))
  
})