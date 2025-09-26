#
data("speciesdata")


test_that(desc = 'checks for fetch data',

          code = {
            skip_on_cran()
            skip_if_offline()
            #dont provide the taxa group column name when the data is a dataframe

            expect_error(fw_fetchdata(data = speciesdata, organismgroup = 'fi',
                                      ecoparams = 'migration'))

            expect_error(fw_fetchdata(data = speciesdata, organismgroup = 'fi',
                                      ecoparams = 'migration', taxonomic_column = 'spp'))
          })

test_that(desc = 'List is generated species with migration types',
          code = {
            skip_on_cran()
            skip_if_offline()
            expect_type(fw_searchdata(organismgroup = 'fi',
                                            ecoparams = 'migration',
                                            cachefolder = 'cache'), 'list')
            })

test_that(desc = 'Dataframe with sanitized species data ',
          code = {
            skip_on_cran()
            skip_if_offline()
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
            skip_on_cran()
            skip_if_offline()
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
            skip_on_cran()
            skip_if_offline()
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
            skip_on_cran()
            skip_if_offline()
            expect_s3_class(fw_fetchdata(data = list(mi=c("Amphichaeta leydigii"),
                                                     pb= c("Gongrosira debaryana")),
                                         organismgroup = c('mi','pb'),
                                         ecoparams = list(mi = c('stream zonation preference'),
                                                          pb = c('substrate preference')),
                                         cachefolder = 'cache')$ecodata, 'data.frame')
          })


test_that(desc = "expect error, wrong sp names",
          code = {
            expect_error(fw_fetchdata(data = 'absnd brama',
                                      organismgroup = 'fi',ecoparams = 'migration'))
            expect_error(fw_fetchdata(data = 'abramis bramaf', organismgroup = 'fi',
                                      ecoparams = 'migration'))
          })

#macrophtyes
test_that("macrophyte",
          code = {
            skip_if_offline()
            skip_on_cran()

            expect_s3_class(fw_fetchdata(data = c("Amblystegium fluviatile",
                                         "Amblystegium humile",
                                         "Amblystegium riparium",
                                         "Amblystegium serpens",
                                         "Amblystegium tenax",
                                         "Amblystegium varium"),
                                organismgroup = 'mp',
                                ecoparams = c('zone - systema'))$ecodata, 'data.frame')
          })

dfmult <- data.frame(spp = c('Abramis barma', 'Salmo trutta fario',
                             'Bangia atropurpurea', 'Aphanocapsa fonticola'),
                     orggroup = rep(c('fi', 'pb'), each = 2))

test_that("multiple datasets",
          code = {
            skip_on_cran()
            skip_if_offline()

            expect_s3_class(fw_fetchdata(data =  dfmult,
                                         organismgroup = list('fi', 'pb'),
                                         taxonomic_column = 'spp',
                                         organismgroup_column = 'orggroup',
                                         ecoparams = list(pb = c('life form', 'water type',
                                                                 "substrate preference"),
                                                          fi= c('migration', 'threat austria')),
                                         cachefolder = 'cache',
                                         seed = 123,
                                         inform = FALSE,
                                         details = FALSE)$ecodata, "data.frame")
            expect_error(fw_fetchdata(data =  dfmult,
                                      organismgroup = list('fi', 'pb'),
                                      taxonomic_column = 'spp',
                                      organismgroup_column = 'orggroupww',#wrong group column
                                      ecoparams = list(pb = c('life form', 'water type',
                                                              "substrate preference"),
                                                       fi= c('migration', 'threat austria')),
                                      cachefolder = 'cache',
                                      seed = 123,
                                      inform = FALSE,
                                      details = FALSE))
          })

