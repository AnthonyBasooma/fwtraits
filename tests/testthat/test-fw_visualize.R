
test_that(desc = "ggplot2",
          code = {

            skip_on_cran()

            skip_if_not_installed("ggplot2")

            fetchdata2 <- fw_fetchdata(data = c("Abramis brama",'Salmo trutta fario'),
                                       organismgroup = 'fi',
                                       ecoparams = 'migration', cachefolder = 'cache')
            fetchdata <- fw_fetchdata(data = c("Abramis brama"),
                                      organismgroup = 'fi',
                                      ecoparams = 'migration', cachefolder = 'cache')

            p <- fw_visualize(fetchdata2)

            expect_true(ggplot2::is_ggplot(p))

            expect_error(fw_visualize(fetchdata2$ecodata))
          })

