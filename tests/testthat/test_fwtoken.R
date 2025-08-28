
test_that(desc = 'token checks',
          code = {
            expect_error(fw_token(cachefolder = 'cache')) #no seed set

            expect_error(fw_token(apikey = 'dkdjfhfjd', seed = 2234))
          })
