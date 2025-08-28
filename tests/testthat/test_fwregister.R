
test_that(desc = 'register',

          code = {
            called_url <- NULL
            with_mocked_bindings(
              browseURL = function(url, ...) { called_url <<- url },
              {
                fw_register()
              }
            )

            expect_equal(called_url, "https://www.freshwaterecology.info/register/index.php")

          })

