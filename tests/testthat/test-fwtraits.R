
#encrypted token for my api key
encryted_api <- "p6-9gAvYXveyC_B_0hTzyYl5FwLRwZEPD-ZE9Y4KrIBstgNc8K2Mr4u6t39LGZ2E1m9SOw"

apikey <- httr2::secret_decrypt(encrypted = encryted_api, key = 'FWTRAITS_KEY')

apikeydecrypted <- fw_loadapikey(test = TRUE, encrytedkey = encryted_api, fwtraitskey =  'FWTRAITS_KEY')

tokendata <- fw_token(key= apikeydecrypted, seed = 1234)

expect_gt(nchar(tokendata), 500)

test_that(desc = 'List of data generated for fish and other groups',
          code = {
            dfpull <- fw_searchdata(organismgroup = 'fi', ecoparams = 'migration', token = tokendata)
            expect_type(dfpull, 'list')
          })




