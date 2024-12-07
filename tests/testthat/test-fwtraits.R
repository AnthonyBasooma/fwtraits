#
# #encrypted token for my api key

searchdata <- fw_searchdata(organismgroup = 'fi', ecoparams = 'migration', cachefolder = 'cache')

splitdata <- fw_split(data = "Abramis brama", organismgroup = 'fi',
                      ecoparams = 'migration', cachefolder = 'cache')

fetchdata <- fw_fetchdata(data = "Abramis brama", organismgroup = 'fi',
                      ecoparams = 'migration', cachefolder = 'cache',
                      sanitize = TRUE)

test_that(desc = 'List is generated species with migration types',
          code = {expect_type(searchdata, 'list')
            })

test_that(desc = 'Dataframe with a particular species ',
          code = {
            expect_s3_class (splitdata, 'data.frame')

            expect_equal(nrow(splitdata), 1)
            })

test_that(desc = 'Dataframe with sanitized species data ',
          code = {
            expect_s3_class (fetchdata, 'data.frame')

            expect_equal(nrow(fetchdata), 1)
          })

