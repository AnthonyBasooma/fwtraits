
test_that(desc = "tchecks",
          code = {
            expect_equal(length(tcheck(c("mi", "fi", 'pp','pb','di','mp'))), 6)
            expect_equal(length(tcheck(c("mi", "fi", 'pp','pb','di','mp'), TRUE)), 6)
            expect_error(tcheck('mm'))#wrong taxa
          })

