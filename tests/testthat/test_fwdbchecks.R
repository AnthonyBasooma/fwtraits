

test_that("str_sentence checks",
          code = {
            expect_error(str_sentence())

            expect_equal(length(str_sentence("salmo trutta")), 1)

            expect_equal(length(str_sentence("salmo trutta fario")), 1)
          })

