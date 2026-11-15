# Web sources and papers

## Retrieved and read

### CDC, NCHHSTP AtlasPlus overview

- **URL:** https://www.cdc.gov/nchhstp/about/atlasplus.html
- **Accessed:** 2026-11-15.
- **Content:** AtlasPlus gives about 20 years of CDC surveillance data on HIV,
  viral hepatitis, STDs and TB, plus social determinants of health, as charts,
  maps and tables by year, geography and demographics. Data from before
  20 January 2025 that are no longer in AtlasPlus are offered as a historical
  extract, which is not updated.
- **Documentation:** the page says that for "all other supporting
  documentation, please access the technical notes, glossary, and FAQs
  available in AtlasPlus". Those are inside the application, not on a public
  page. The page links only the historical-extract ReadMe (next entry).

### CDC, ReadMe for the AtlasPlus historical data extract (30 April 2025)

- **URL:** https://www.cdc.gov/nchhstp/media/pdfs/2025/05/ReadMe_AtlasPlus_historical-data-extract_20250430.pdf
- **Accessed:** 2026-11-15, read through a web fetch that returned text without
  page numbers.
- **Scope:** documents the historical CSV of data that were on AtlasPlus on
  20 January 2025: 13 indicators from the National HIV Surveillance System
  (NHSS) and the Medical Monitoring Project (MMP). It is not documentation for
  current AtlasPlus data.
- **Key points:**
  - "Numbers less than 12, and rates and percentages based on these numbers,
    should be interpreted with caution."
  - Suppression is required at the state and county levels under data
    re-release agreements. No single numeric suppression threshold is given.
  - NHSS data are preliminary until a 12-month reporting delay (18 months for
    deaths) and should not be used for trends; the extract marks 2023 and 2024
    preliminary, and 2022 deaths and prevalence.
  - MMP samples adults aged 18 and older, but its estimates are labelled "13+"
    for comparability; they do not represent ages 13 to 17.
- **Not found in this text:** the labels "Data suppressed" and "Data not
  available", "provisional", the NHSS age floor, and any COVID-19 caution for
  2020–2021. Check the in-application Technical Notes for these.

### Census Bureau, substantial changes to counties, 2020s

- **URL:** https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html
- **Accessed:** 2026-11-15.
- **Content:** one entry: Connecticut (09) "finalized adoption of the State's
  nine Councils of Governments (COGs) as the county-equivalent geographic unit"
  (listed under January 2020 on the page). Links a change notice, a crosswalk
  from counties to county subdivisions, and a planning-region map. The page
  itself gives no FIPS codes or effective date for the change.

### Census Bureau, substantial changes to counties, 2010s

- **URL:** https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2010.html
- **Accessed:** 2026-11-15.
- **Changes, quoted from the page:**
  - Bedford city, VA (51515): "Changed to town status and added to Bedford
    County (51-019) effective July 1, 2013."
  - Shannon County, SD (46113): "Changed name and code to Oglala Lakota County
    (46-102) effective May 1, 2015."
  - Wade Hampton Census Area, AK (02270): "Changed name and code to Kusilvak
    Census Area (02-158) effective July 1, 2015."
  - Valdez-Cordova Census Area, AK (02261): "Split to form Chugach Census Area
    (02-063) and Copper River Census Area (02-066) effective January 02, 2019."
  - Petersburg Borough, AK (02195) created 3 January 2013 from part of the
    former Petersburg Census Area (same code) and part of Hoonah-Angoon (02105);
    Prince of Wales-Hyder (02198) gained part of the former census area.

### Walker, *Analyzing US Census Data*, section 7.3.2

- **URL:** https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html
- **Accessed:** 2026-11-15.
- **Content:** "Population-weighted areal interpolation". Moves values between
  geographies that do not nest by weighting their intersections with 2020
  Census block populations (`weight_column = "POP20"`; housing units are an
  alternative), with `tidycensus::interpolate_pw()`.

### AIDSVu, data methods: city and ZIP-level data

- **URL:** https://aidsvu.org/data-methods/data-methods-city-level/
- **Accessed:** 2026-11-15, first 100,000 of about 113,000 characters read.
- **Key points:**
  - ZIP-level values are suppressed when the numerator is under 5 and/or the
    ZIP's population is under 500. Philadelphia suppresses numerators under 10,
    including zero; Phoenix under 6, including zero; Seattle under 6 but above
    zero.
  - Rates with numerators of 5 to 11 are flagged as unstable.
  - Denominators are 2020 Census ZCTA populations, ages 13 and older for
    prevalence and new diagnoses.
  - For the 2024 release, prevalence, care and viral suppression are one-year
    values; new and late diagnoses and linkage to care are five-year
    (2020–2024).
  - Use of data from cities without downloadable datasets "is only permitted
    through exclusive permission from the State or Local Health Department";
    Philadelphia data "is only permitted through exclusive permission from the
    Philadelphia Department of Public Health (PDPH)".
- **Not yet read:** the remainder of the page, including the downloadable
  datasets section, which may hold citation or reuse terms.

## Cited but not yet verified

- **AtlasPlus Technical Notes, Glossary and FAQs:** inside the AtlasPlus
  application, reached from the overview page above. Expected to define
  suppression, data status values and indicator definitions. *Not yet
  verified.*
- **Census tract relationship files (2020 to 2010):** Census Geography,
  Relationship Files. Needed for Connecticut and Alaska tract vintages.
  *Not yet verified.*
- **ACS geography boundaries by year:**
  https://www.census.gov/programs-surveys/acs/geography-acs/geography-boundaries-by-year.html.
  *Not yet verified.*
- **ACS tables B01001 (sex by age), B01003 (total population), B03002
  (Hispanic or Latino origin by race):** table universes and variable labels
  for the requested ACS year. *Not yet verified.*
- **Sullivan PS et al.,** *JMIR* 2020;22(10):e23173, the citation AIDSVu asks
  for. Title and journal not checked. *Not yet verified.*
- **San Francisco Department of Public Health, HIV Epidemiology Annual Report
  2025,** Map 17.1, people living with HIV by 2020 census tract, in count
  bands. Recorded from a local copy, not from a public URL. *Not yet verified.*
