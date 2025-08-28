
test_that(desc = "fwpath",
          code = {
            expect_error(fw_path(dir = NULL))
          })
