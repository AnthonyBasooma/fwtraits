#
# #encrypted token for my api key

searchdata <- fw_searchdata(organismgroup = 'fi', ecoparams = 'migration', cachefolder = 'cache')

#fish
fetchdata <- fw_fetchdata(data = "Abramis brama", organismgroup = 'fi',
                      ecoparams = 'migration', cachefolder = 'cache')

#macroinvertebrates and phytobenthos

mipb <- fw_fetchdata(data = list(mi=c("Congeria kusceri",
                                      "Congeria leucophaeata",
                                      "Dreissena polymorpha",
                                      "Dreissena rostriformis bugensis"),
                                 pb= c("Gongrosira debaryana",
                                       "Gongrosira fluminensis",
                                       "Hydrodictyon reticulatum",
                                       "Sphaerobotrys fluviatilis",
                                       "Stigeoclonium farctum",
                                       "Stigeoclonium tenue",
                                       "Tetraspora gelatinosa",
                                       "Thorea hispida")),
                     organismgroup = c('mi','pb'),
                     ecoparams = list(mi = c('stream zonation preference'),
                                      pb = c('substrate preference')),
                     cachefolder = 'cache')

test_that(desc = 'List is generated species with migration types',
          code = {expect_type(searchdata, 'list')
            })


test_that(desc = 'Dataframe with sanitized species data ',
          code = {
            expect_s3_class (fetchdata$ecodata, 'data.frame')

            expect_equal(nrow(fetchdata$ecodata), 1)

            expect_contains(colnames(fetchdata$ecodata), 'CategoryName') #sanitized column
          })
test_that(desc = 'dataframe of species traits',
          code = {
            expect_s3_class(fw_dbguide(), 'data.frame')
            expect_equal(length(colnames(fw_dbguide())), 9)
          })
