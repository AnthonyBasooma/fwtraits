
test_that(desc = 'dataframe of species traits',
          code = {
            expect_s3_class(fw_dbguide(), 'data.frame')

            expect_equal(length(colnames(fw_dbguide())), 9)

            expect_error(fw_dbguide("mm")) #wrong organism group
          })

