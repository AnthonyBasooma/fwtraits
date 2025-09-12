

set.seed(1135)
speciesdata$abundance <- rnorm(n = nrow(speciesdata), 4.3, 1.2)

spgeo1 <- speciesdata |>
  sf::st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = sf::st_crs(4326))

test_that(desc = "Test for diversity indices",
          code = {
            skip_on_cran()
            skip_if_offline()

            sptraits <- fw_fetchdata(data = speciesdata,
                                     ecoparams = c('rheophily habitat', 'spawning habitat',
                                                   'feeding diet adult'),
                                     taxonomic_column = 'scientificName',
                                     organismgroup = 'fi')

            expect_s3_class(fw_fdcompute(fwdata = sptraits,
                                      sitesdata = speciesdata,
                                      sites = 'waterBody',
                                      species = 'scientificName',
                                      FD = TRUE), 'data.frame')
            #not sites
            expect_s3_class(fw_fdcompute(fwdata = sptraits,
                                      sitesdata = spgeo1,
                                      species = 'scientificName',
                                      FD = TRUE), 'data.frame')

            #provide abundance data to calcualte other FD indicies
            expect_s3_class(fw_fdcompute(fwdata = sptraits,
                                      sitesdata = speciesdata,
                                      abund = 'abundance',
                                      sites = 'waterBody',
                                      species = 'scientificName',
                                      FD = TRUE), 'data.frame')
            #provide abundance data
            expect_s3_class(fw_fdcompute(fwdata = sptraits,
                                      sitesdata = spgeo1,
                                      abund = 'abundance',
                                      species = 'scientificName',
                                      FD = TRUE), 'data.frame')

            expect_s3_class(fw_fdcompute(fwdata = sptraits, sitesdata = speciesdata,
                                     species = 'scientificName',
                                     sites = 'waterBody',
                                     abund = 'abundance',
                                     FD = FALSE, dummy = TRUE), 'data.frame')


            expect_s3_class(fw_fdcompute(fwdata = sptraits,
                                       sitesdata = speciesdata,
                                       sites = 'waterBody',
                                       species = 'scientificName',
                                       abund = 'abundance',
                                       FD = FALSE, dummy = FALSE), 'data.frame')

            #with no sites
            expect_s3_class(fw_fdcompute(fwdata = sptraits, sitesdata = spgeo1,
                                      species = 'scientificName',
                                      abund = 'abundance',
                                      FD = FALSE, dummy = TRUE), 'data.frame')


            expect_s3_class(fw_fdcompute(fwdata = sptraits,
                                      sitesdata = spgeo1,
                                      species = 'scientificName',
                                      abund = 'abundance',
                                      FD = FALSE, dummy = FALSE), 'data.frame')
            #Functional dendogram
            expect_s3_class(fw_fdendro(fwdata = sptraits, plot = TRUE), 'data.frame')
          })
