
test_that(desc = "get msg",
          code = {
            skip_on_cran()
            expect_message(fw_cite())
          })
