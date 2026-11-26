# Shared by the payload and county-request tests.

payload_ids <- function(payload) {
  as.integer(strsplit(payload$VariableIDs, ",")[[1]])
}

county_ids <- lookup[["counties"]]$geo_id
county_states <- substr(lookup[["counties"]]$fips, 1, 2)
