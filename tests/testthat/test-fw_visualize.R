
library(fwtraits)

fetchdata2 <- fw_fetchdata(data = c("Abramis brama",'Salmo trutta fario'),
                           organismgroup = 'fi',
                           ecoparams = 'migration', cachefolder = 'cache')

test_that(desc = "ggplot2",
          code = {
            skip_if_not_installed("ggplot2")

            p <- fw_visualize(fetchdata2)

            expect_true(ggplot2::is.ggplot(p))
          })

