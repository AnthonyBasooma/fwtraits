
library(vcr)

vcr::vcr_configure(
  dir = "../fixtures/vcr_cassettes",
  record = "once",
  match_requests_on = c("method", "uri"),
  preserve_exact_body_bytes = FALSE,
  filter_sensitive_data = list("<<api_key>>" = Sys.getenv("API_KEY"))
)

