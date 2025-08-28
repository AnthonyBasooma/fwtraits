test_that(desc = 'check pkg instals',
          code = {
            expect_error(check_packages("gfdd")) #wrong pkg
            expect_match(check_packages("ggplot2"), "ggplot2") #pkg is there
          })
