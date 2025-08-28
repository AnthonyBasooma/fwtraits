
test_that(desc = 'before u start',
          code = {
            expect_error(fw_be4ustart())#rstudio not running
          })

test_that("fw_be4ustart tries to open viewer", {
  called_url <- NULL

  with_mocked_bindings(
    viewer = function(url, ...) { called_url <<- url },
    {
      fw_be4ustart()
    }
  )

  expect_match(called_url, "\\.html$")
})
