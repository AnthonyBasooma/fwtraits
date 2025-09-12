
test_that(desc = 'token checks', {
            skip_on_cran()
            skip_if_offline()
            expect_error(fw_token(cachefolder = 'cache')) #no seed set

            expect_error(fw_token(apikey = 'dkdjfhfjd', seed = 2234))

            expect_error(fw_token(seed = 1135, cachefolder = 'cachee', secure = FALSE))
          })

