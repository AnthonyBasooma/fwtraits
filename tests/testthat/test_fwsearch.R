
test_that(desc = "tchecks",
          code = {
            expect_equal(length(tcheck(c("mi", "fi", 'pp','pb','di','mp'))), 6)
            expect_equal(length(tcheck(c("mi", "fi", 'pp','pb','di','mp'), TRUE)), 6)
            expect_error(tcheck('mm'))#wrong taxa

            #test for migration ecoparam retruns warning
            expect_warning(fw_searchdata(organismgroup = 'fi', taxa_searched = 'Abramis brama', ecoparams = 'migte'))

            expect_warning(fw_searchdata(organismgroup = 'pb', taxa_searched = 'Achnantheffs danica',
                                         ecoparams = 'life form', taxalevel = 'species', seed = 1233, warn = TRUE))
          })

