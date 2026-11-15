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
- **Accessed:** 2026-11-15, whole page read in two parts.
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
- **Dataset codes:** "-5: City did not authorize sharing this specific type of
  data." City profile data come under "a data release agreement with health
  departments". Questions go to info@aidsvu.org.
- **Citation:** AIDSVu's main methods page (https://aidsvu.org/data-methods/,
  accessed 2026-11-15) gives, under "Cite AIDSVU", the Sullivan et al. 2020
  paper below. It states no terms of use or redistribution limits.
- **Not found:** reuse terms for the downloadable datasets beyond the
  permissions quoted above.

### AtlasPlus application pages

- **URL:** https://gis.cdc.gov/grasp/nchhstpatlas/main.html (the application)
  and https://www.cdc.gov/nchhstp/atlas/ (its "About AtlasPlus" page).
- **Accessed:** 2026-11-15, through a web fetch, which does not run the
  application's scripts.
- **Content:** the application says "Refer to technical notes for each
  indicator" and links its Technical Notes to the NHSS page (next entry). It
  says "CDC has postponed release of estimates for HIV incidence, HIV
  prevalence, and PrEP coverage." "Glossary" appears only as a menu item. The
  "About" page describes county-level maps, charts and tables and links only the
  historical-extract ReadMe; neither page states a suppression threshold.

### CDC, National HIV Surveillance System (NHSS) and its technical notes

- **URL:** https://www.cdc.gov/hiv-data/nhss/
- **Accessed:** 2026-11-15.
- **Key points:**
  - Data re-release agreements "require specific levels of cell suppression at
    the state and county level"; no numeric threshold is given on the page.
  - HIV case definitions classify infection "among persons aged ≥ 13 years";
    transmission categories are imputed for ages 13 and older.
  - Data are preliminary before a 12-month reporting delay; death data "are
    considered preliminary unless based on a ≥ 18-month reporting delay."
  - "HIV diagnosis data for the year 2020 should be interpreted with caution due
    to the onset of the COVID-19 pandemic." Nothing specific on 2021.
  - "Data for Guam are not included due to small case numbers."
  - Links the Federal Register notice on Connecticut's county-equivalents:
    https://www.federalregister.gov/documents/2022/06/06/2022-12063/change-to-county-equivalents-in-the-state-of-connecticut
    (not read).
- **Not found:** cases with an unknown county of residence; the labels "Data
  suppressed" and "Data not available".

### Census Bureau, ACS geography boundaries by year

- **URLs:** https://www.census.gov/programs-surveys/acs/geography-acs/geography-boundaries-by-year.html
  and the yearly pages `geography-boundaries-by-year.2019.html`, `.2020.html`
  and `.2021.html`.
- **Accessed:** 2026-11-15.
- **Census tract vintage, by ACS year:** 2019 "2010 Census"; 2020 "2020
  Census"; 2021 "2020 Census"; 2025 "2020 Census" (Statistical Areas table,
  row "140 | Census Tract"). For 5-year estimates, "use the last year of the
  estimate period to determine the vintage", so 2016–2020 is the first 5-year
  release on 2020 tracts.

### Census Bureau, 2020 relationship files

- **URL:** https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2020.html
- **Accessed:** 2026-11-15.
- **Content:** "2020 Census Tract to 2010 Census Tract Relationship File", a
  national file and state-level files, which "provide simple relationships
  between 2020 census tract entities and 2010 census tract entities", with an
  explanation page and "2020 Comparability Relationship File Record Layouts".
  The fetch did not return the download URLs. The main relationship-files page
  also offers special 2024 files "for the state of Connecticut only" for its
  new county-equivalents.

### Census Bureau, ACS tables B01001, B01003 and B03002 (2022 5-year)

- **URLs:** https://api.census.gov/data/2022/acs/acs5/groups/B01001.html,
  `.../B01003.html`, `.../B03002.html`.
- **Accessed:** 2026-11-15; each page read to 100,000 characters.
- **B01001 "Sex by Age":** `B01001_001E` "Estimate!!Total: Sex by Age";
  `B01001_005E` "...Male:!!10 to 14 years"; `B01001_006E` "...Male:!!15 to
  17 years"; `B01001_029E` "...Female:!!10 to 14 years"; `B01001_030E`
  "...Female:!!15 to 17 years". The bands do not start at 13.
- **B01003 "Total Population":** `B01003_001E` "Estimate!!Total".
- **B03002 "Hispanic or Latino Origin by Race":** `_001E` total; `_002E` not
  Hispanic or Latino; `_003E` to `_009E` not Hispanic or Latino by race alone
  (White, Black or African American, American Indian and Alaska Native, Asian,
  Native Hawaiian and Other Pacific Islander, Some other race) and Two or more
  races; `_012E` Hispanic or Latino.
- Labels were checked for 2022 only; other ACS years may differ.

### Sullivan et al. 2020, the AIDSVu data resource

- **URL:** https://pmc.ncbi.nlm.nih.gov/articles/PMC7654504/
- **Accessed:** 2026-11-15, citation details only; the paper was not read.
- **Citation:** Sullivan PS, Woodyatt C, Koski C, et al. A Data Visualization
  and Dissemination Resource to Support HIV Prevention and Care at the Local
  Level: Analysis and Uses of the AIDSVu Public Data Resource. *J Med Internet
  Res* 2020;22(10):e23173. doi:10.2196/23173.

### San Francisco Department of Public Health, HIV Epidemiology Annual Report 2025

- **Source:** local copy of the PDF ("2025 San Francisco HIV Epidemiology
  Annual Report", 117 pages); public URL not recorded.
- **Read:** 2026-11-15, Map 17.1 (printed page 76).
- **Content:** "Geographic distribution of people living with HIV who resided in
  San Francisco as of December 2025", by census tract, in bands 1–29, 30–54,
  55–99, 100–199 and 200–243. "Data not shown (Census tracts with population
  totals fewer than 500)". The most recent address as of 31 December 2025 was
  geocoded to tract; tract boundaries are from the 2020 Census. The map is an
  image; the counts behind it are not in the text.

## Not yet verified

- **AtlasPlus Technical Notes and Glossary inside the application.** The
  application needs a browser to run; a web fetch reaches only its static text.
  The numeric suppression rule for current county data, and the meaning of
  "Data suppressed" and "Data not available", remain unconfirmed.
- **AIDSVu reuse terms for the downloadable datasets.** Neither methods page
  states them; ask AIDSVu (info@aidsvu.org) before published use.
