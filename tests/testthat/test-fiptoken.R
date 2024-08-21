#
# enc_api <- "p6-9gAvYXveyC_B_0hTzyYl5FwLRwZEPD-ZE9Y4KrIBstgNc8K2Mr4u6t39LGZ2E1m9SOw"
#
# tokendata <- httr2::secret_decrypt(encrypted = enc_api, key = 'FWTRAITS_KEY')
#
#
# test_that(desc = 'token generated',
#           code = {
#             expect_type(fip_token(seed = 1234), 'character')
#           })
