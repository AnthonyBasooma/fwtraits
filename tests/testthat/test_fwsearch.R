
test_that(desc = "tchecks",
          code = {
            expect_equal(length(tcheck(c("mi", "fi", 'pp','pb','di','mp'))), 6)
          })
