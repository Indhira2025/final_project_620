library(httr2)
get_cdc_data <- function(api) {
  request(api) |> 
    req_url_query("$limit" = 10000000) |> 
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
}