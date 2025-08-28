#
data("speciesdata")


test_that(desc = 'checks for fetch data',
          code = {
            #dont provide the taxa group column name when the data is a dataframe

            expect_error(fw_fetchdata(data = speciesdata, organismgroup = 'fi', ecoparams = 'migration'))

            out <- utils::capture.output()
          })


searchdata <- fw_searchdata(organismgroup = 'fi', ecoparams = 'migration', cachefolder = 'cache')

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
            #fish
            fetchdata <- fw_fetchdata(data = "Abramis brama", organismgroup = 'fi',
                                      ecoparams = 'migration', cachefolder = 'cache')

            expect_s3_class (fetchdata$ecodata, 'data.frame')

            expect_equal(nrow(fetchdata$ecodata), 1)

            expect_contains(colnames(fetchdata$ecodata), 'CategoryName') #sanitized column

            #check for details parameter

            fetchdata <- fw_fetchdata(data = "Abramis brama", organismgroup = 'fi',
                                      ecoparams = 'migration', cachefolder = 'cache',
                                      details = TRUE)

            out <- utils::capture.output(fetchdata)[31]

            expect_equal(out, "$funcall_$details")

          })

#test different taxonomic groups
#macroinvertebrates
test_that(desc = "test for macroinvertebrates",
          code = {

            expect_s3_class(fw_fetchdata(data = 'Amphichaeta leydigii',
                                         ecoparams = 'stream zonation preference',
                         organismgroup = 'mi')$ecodata, 'data.frame')

            expect_message(fw_fetchdata(data = 'Amphichaeta leydigii',
                                         ecoparams = 'stream zonation preference',
                                         organismgroup = 'mi', inform = TRUE)$ecodata)

            expect_s3_class(fw_fetchdata(data = 'Amphichaeta',
                                         ecoparams = 'stream zonation preference',
                                         organismgroup = 'mi',
                                         taxalevel = 'genus')$ecodata, 'data.frame')

            expect_s3_class(fw_fetchdata(data = 'NAIDIDAE',
                                         ecoparams = 'stream zonation preference',
                                         organismgroup = 'mi',
                                         taxalevel = 'family')$ecodata, 'data.frame')

          })
#test for phyto benthos
test_that(desc = "test for phytobentos",
          code = {

            expect_s3_class(fw_fetchdata(data = c("Gongrosira debaryana"),
                                         organismgroup = 'pb',
                                         ecoparams = c('substrate preference'),
                                         cachefolder = 'cache')$ecodata, 'data.frame')
            expect_s3_class(fw_fetchdata(data = c("Gongrosira"),
                                         organismgroup = 'pb',
                                         ecoparams = c('substrate preference'),
                                         cachefolder = 'cache',
                                         taxalevel = 'genus')$ecodata, 'data.frame')
          })

#combined macroinvertebrates and phytobenthso
test_that(desc = "macro+phytbentos",
          code = {

            expect_s3_class(fw_fetchdata(data = list(mi=c("Amphichaeta leydigii"),
                                                     pb= c("Gongrosira debaryana")),
                                         organismgroup = c('mi','pb'),
                                         ecoparams = list(mi = c('stream zonation preference'),
                                                          pb = c('substrate preference')),
                                         cachefolder = 'cache')$ecodata, 'data.frame')
          })


# ppp <- fw_fetchdata(data = c("Anathece clathrata"),
#                     organismgroup = 'pp',
#                     ecoparams = c('stenoecy factor', 'aquaticity','life form'),
#                     cachefolder = 'cache')
#
# ppp2 <- fw_fetchdata(data = c("Anathece minutissima"),
#                      organismgroup = 'pp',
#                      ecoparams = c('stenoecy factor', 'aquaticity','life form'),
#                      cachefolder = 'cache')










