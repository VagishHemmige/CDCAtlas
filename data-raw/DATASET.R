# Builds R/sysdata.rda: CDC's AtlasPlus code table (`varvals`) and the
# geography lookups (`lookup`) that get_atlas() builds its requests from.
#
# Run from the package root, in a fresh R session:
#
#   source("data-raw/DATASET.R")
#
# It downloads CDC's AtlasPlus set-up data (about 2.6 MB, no key needed) and
# overwrites R/sysdata.rda. Check the summary it prints, run the tests, and
# commit the new R/sysdata.rda with this script's version.
#
# The live tests ("stored codes still match CDC's code table") say when CDC has
# changed a code the package uses, which is when to rerun this.

devtools::load_all()

init <- .atlas_init_data()
varvals <- tibble::as_tibble(init$varvals)

# AtlasPlus geography ids, with their names and FIPS codes from `varvals`.
geo_lookup <- function(geo_id) {
  tibble::tibble(geo_id = geo_id) |>
    dplyr::left_join(varvals, by = c("geo_id" = "id")) |>
    dplyr::select("geo_id", "name", "fips")
}

lookup <- list(
  # The 50 states and DC (5001-5051), the five island territories including
  # Puerto Rico (5052-5056) and Palau (8451).
  state = geo_lookup(c(5001:5056, 8451)),

  # The 50 Ending the HIV Epidemic (EHE) counties. Not used by the package yet.
  ehe = geo_lookup(c(
    5165, 5248, 5266, 5277, 5280, 5281, 5283, 5284, 5285, 5381, 5388, 5397,
    5410, 5425, 5430, 5432, 5434, 5482, 5493, 5509, 5516, 5673, 5808, 6192,
    6211, 6270, 6271, 6279, 6292, 6375, 6811, 6843, 6845, 6893, 6914, 6921,
    6931, 7012, 7123, 7130, 7136, 7357, 7569, 7600, 7642, 7686, 7805, 7812,
    8033, 8274
  )),

  # Metropolitan statistical areas, in AtlasPlus's order. Leaves out its three
  # summary rows (8436-8438).
  msa = geo_lookup(c(
    8294, 8295, 8296, 8297, 8357, 8469, 8298, 8459, 8299, 8300, 8301, 8302,
    8303, 8304, 8305, 8306, 8433, 8309, 8310, 8307, 8386, 8311, 8312, 8313,
    8314, 8434, 8315, 8319, 8470, 8320, 8321, 8322, 8323, 8324, 8325, 8327,
    8328, 8329, 8330, 8332, 8331, 8334, 8335, 8316, 8465, 8336, 8337, 8364,
    8326, 8427, 8338, 8317, 8339, 8340, 8341, 8342, 8343, 8344, 8440, 8345,
    8346, 8347, 8348, 8462, 8463, 8349, 8318, 8461, 8350, 8464, 8351, 8352,
    8353, 8354, 8355, 8356, 8358, 8359, 8360, 8460, 8361, 8362, 8363, 8365,
    8367, 8368, 8369, 8387, 8439, 8370, 8374, 8435, 8371, 8372, 8376, 8373,
    8375, 8377, 8407, 8378, 8379, 8380, 8381, 8382, 8383, 8384, 8388, 8385,
    8390, 8391, 8441, 8392, 8393, 8394, 8395, 8396, 8397, 8452, 8398, 8399,
    8400, 8308, 8401, 8403, 8404, 8405, 8406, 8408, 8410, 8411, 8409, 8412,
    8414, 8413, 8416, 8417, 8402, 8466, 8418, 8419, 8415, 8467, 8420, 8421,
    8422, 8423, 8424, 8425, 8333, 8468, 8426, 8428, 8366, 8429, 8389, 8430,
    8431, 8432
  )),

  # Counties. Leaves out the pre-change Alaska, South Dakota and Virginia
  # county codes and the four island territories that `varvals` also lists.
  counties = geo_lookup(
    scan("data-raw/counties.txt", sep = ",", quiet = TRUE)
  )
)

# Summary to check before committing.
no_fips <- sum(vapply(lookup, \(x) sum(is.na(x$fips)), integer(1)))
cli::cli_inform(c(
  "{nrow(varvals)} codes in CDC's code table.",
  "*" = "{nrow(lookup$state)} states and territories, {nrow(lookup$ehe)} EHE
    counties, {nrow(lookup$msa)} MSAs, {nrow(lookup$counties)} counties.",
  "*" = "Lookup rows with no FIPS code: {no_fips}."
))

usethis::use_data(varvals, lookup, internal = TRUE, overwrite = TRUE)
