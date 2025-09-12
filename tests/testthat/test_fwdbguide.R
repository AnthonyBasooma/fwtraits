
test_that(desc = 'dataframe of species traits',{
  skip_on_cran()

  db <- fw_dbguide()

  expect_s3_class(db, 'data.frame')

  expect_equal(length(colnames(db)), 9)

  expect_error(fw_dbguide("mm")) #wrong organism group
})


