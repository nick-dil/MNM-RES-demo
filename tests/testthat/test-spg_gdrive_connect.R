test_that("Gdrive connection works", {
  expect_type(getPocIdList(), "list")
})

test_that("Reading data from GDrive works", {
  id_hash = googledrive::as_id("12vNFDYa-EMSPjDRGAjOTniF54kC_nEgV")
  expect_gt(nrow(readPocSampleData(id_hash, addVersion = F, isDemo = F)), 0)
})

test_that("DEMO data retrieval", {
  id_hash = googledrive::as_id("12vNFDYa-EMSPjDRGAjOTniF54kC_nEgV")
  expect_equal(colnames(readPocSampleData(id_hash)),
                   c("type", "meetnet", "locatie", "hydr_class", "id"))
})
