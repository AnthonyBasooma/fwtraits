
test_that(desc = 'register',

          code = {
            skip_on_cran()
            skip_if_offline()

            called_url <- NULL
            with_mocked_bindings(
              browseURL = function(url, ...) { called_url <<- url },
              {
                fw_register()
              }
            )
            expect_equal(called_url, "https://www.freshwaterecology.info/register/index.php")

          })

