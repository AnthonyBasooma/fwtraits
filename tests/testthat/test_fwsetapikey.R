
test_that("fw_setapikey tries to open .Renviron", {
  called <- FALSE
  with_mocked_bindings(
    file.edit = function(path) {
      called <<- TRUE
      expect_match(path, "\\.Renviron$")
    },
    {
      fw_setapikey()
    }
  )
  expect_true(called)
})

#set key

test_that("The key is properly set",
         code =  {
            expect_error(fw_keyload("api_key1")) #wrong api key variable name

           expect_equal(nchar(fw_keyload()), 36)

           expect_error(fw_keyload("GITHUB_PAT"))

          })






