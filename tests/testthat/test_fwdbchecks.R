

test_that("str_sentence checks",
          code = {
            expect_error(str_sentence())

            expect_equal(length(str_sentence(x = "salmo trutta")), 1)

            expect_equal(length(str_sentence(x= c("salmo trutta fario", "fario"))), 1)
          })

