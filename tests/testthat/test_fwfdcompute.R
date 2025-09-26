

set.seed(1135)
speciesdata$abundance <- rnorm(n = nrow(speciesdata), 4.3, 1.2)

spgeo1 <- speciesdata |>
  sf::st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = sf::st_crs(4326))

#test for errors when the traits are not not dismialr
set.seed(1135)
sites1 <- rep(c("A","B", "C", "D", "E"), each= 8)
pb <- c("Gongrosira debaryana","Gongrosira fluminensis", "Hydrodictyon reticulatum",
        "Sphaerobotrys fluviatilis","Stigeoclonium farctum","Stigeoclonium tenue",
        "Tetraspora gelatinosa","Thorea hispida")
sp <- c(sample(pb, 8), sample(pb, 8),sample(pb, 8),sample(pb, 8),sample(pb, 8))
abun <- rnorm(length(sp), 5.3, 0.5)

pbdata <- data.frame(sites = sites1, species = sp, abund = abun)

test_that(desc = "Test for diversity indices",
          code = {
            skip_on_cran()
            skip_if_offline()

            sptraits <- fw_fetchdata(data = speciesdata,
                                     ecoparams = c('rheophily habitat', 'spawning habitat',
                                                   'feeding diet adult'),
                                     taxonomic_column = 'scientificName',
                                     organismgroup = 'fi')

            ppdatatraits <- fw_fetchdata(data = pbdata,
                                         organismgroup = 'pb',
                                         taxonomic_column = 'species',
                                         ecoparams = c('substrate preference','life form'),
                                         cachefolder = 'cache',
                                         inform = FALSE)

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
            #expect errors

            expect_error(fw_fdcompute(fwdata = sptraits,
                                      sitesdata = speciesdata,
                                      species = 'scientificme',#not in sites data
                                      FD = TRUE, dummy = FALSE))

            expect_error(fw_fdcompute(fwdata = speciesdata,#wrong data, fetchdata expected
                                         sitesdata = spgeo1,
                                         species = 'scientificName',
                                         abund = 'abundance',
                                         FD = FALSE, dummy = FALSE))
            expect_error(fw_fdcompute(fwdata = sptraits, #not sites
                                         sitesdata = speciesdata,
                                         species = 'scientificName',
                                         abund = 'abundance',
                                         FD = TRUE, dummy = FALSE))
            expect_error(fw_fdcompute(fwdata = sptraits, #not sites
                                      sitesdata = speciesdata,
                                      species = 'scientificName',
                                      sites = 'waterB', #wrong site column
                                      abund = 'abundance',
                                      FD = TRUE, dummy = FALSE))
            expect_s3_class(fw_fdcompute(fwdata = sptraits,
                                         sitesdata = spgeo1,
                                         species = 'scientificName',
                                         abund = 'abundance',
                                         sites = 'waterBody',
                                         FD = TRUE, dummy = FALSE), 'data.frame')

            expect_error( fw_fdcompute(fwdata = ppdatatraits,
                                       sitesdata = pbdata,
                                       species = 'species',
                                       abund = 'abund',
                                       sites = 'sites',
                                       FD = FALSE, dummy = TRUE))


            #Functional dendogram
            expect_s3_class(fw_fdendro(fwdata = sptraits, plot = TRUE), 'data.frame')
          })
