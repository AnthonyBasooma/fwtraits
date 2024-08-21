#
#encypted token for my api key
enc_api <- "p6-9gAvYXveyC_B_0hTzyYl5FwLRwZEPD-ZE9Y4KrIBstgNc8K2Mr4u6t39LGZ2E1m9SOw"

#the FWTRAITS_KEY is the unlock key saved in my local environment
#check https://httr2.r-lib.org/articles/wrapping-apis.html for more information

apikey <- httr2::secret_decrypt(encrypted = enc_api, key = 'FWTRAITS_KEY')

#download fish catchment region data

apikeydecrypted <- loadapikey(test = TRUE, encrytedkey = enc_api,
                              fwtraitskey =  'FWTRAITS_KEY')

tokendata <- fip_token(key= apikeydecrypted, seed = 1234)

fishdata <- collatedata(taxa = 'fi', ecotraits = 'catchment region',
                        token = tokendata)

x <- "ABramis brama"

test_that(desc = 'Species in catchment region and name is also cleaned',
          code = {
            expect_type(clean_names(sp= x, grouplists = fishdata, group = 'fishes'), 'character')
          })




