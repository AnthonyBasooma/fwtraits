

test_that(desc = 'dataframe of species traits',{
  skip_on_cran()
  cache_dir <- fw_path('cache')
  if (dir.exists(cache_dir)) {
    unlink(list.files(cache_dir, full.names = TRUE), recursive = TRUE)
  }

  vcr::use_cassette("get-all-db-data",{

    db <- fw_paramlist(cachefolder = "cache")

  })
  # Basic checks
  expect_type(db, "list")
  expect_true(length(db) > 0)
})

#vcr::vcr_configuration()$dir
