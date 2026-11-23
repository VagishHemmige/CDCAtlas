# CDCAtlas (development version)

* `extrapolate_from_county()` warns once (class `"cdcatlas_duplicated_geoid"`) when an area is split between counties and so has more than one row with the same `geoid`: with ACS 2010 to 2019, one Connecticut tract and its block groups. Its help and the "Extrapolation to tracts" article show how to sum the rows before joining to boundaries (#152).
* `extrapolate_from_county()` warns once (class `"cdcatlas_state_unplaced_cases"`) when a state has more than 25% of its published state total in no county row of `x`, naming the states and their percentages: county and area estimates there are too low by about that share, or with `no_county = "spread"` any of those cases that reach areas are placed by assumption. No estimate changes (#164).
* `extrapolate_from_county(no_county = "spread")` doesn't spread a remainder `no_county_cases()` withholds, and the message says how many state strata that applies to (#173).
* `impute_suppressed()` says in a message how many suppressed rows `method = "state_rate"` filled in and how many of them had their value set by `cap` and by `floor`, as counts and percentages. No estimate changes (#164).
* `impute_suppressed()` returns a new column, `cases_uncapped`: for rows filled in by `method = "state_rate"`, the estimate before `cap` and `floor` (state rate times county population), and `NA` otherwise. `cases_uncapped > cap` marks the rows whose value the cap set. `cases` is unchanged (#151).
* `no_county_cases()` doesn't report the remainder of a state stratum with exactly one county row without a published count (counted in CDC's county table as well as in `x`), since it could be that county's count, nor where CDC's county table has no rows for the stratum: `published_cases`, `unpublished_cases` and `no_county_cases` are `NA`, and a new column, `no_county_withheld`, says why. CDC's documentation doesn't say that state totals include cases with no county (#173).
* `no_county_cases()` warns once (class `"cdcatlas_state_unplaced_cases"`) when a state has more than 25% of its published state total in no county row of `x` after imputation, naming the states and their percentages; this happens when a state's county counts are largely suppressed. The report is unchanged (#164).

# CDCAtlas 0.1.0

First release. CDCAtlas downloads surveillance data from CDC AtlasPlus and estimates counts below the county level. Earlier development versions had only `get_atlas()`.

## Breaking changes

* `extrapolate_from_county()` no longer returns `area_noncases`: population minus cases means people without the condition only for prevalence. Its help shows how to compute it for HIV prevalence (#117).
* `get_atlas()` returns a tibble (#17).
* `get_atlas()` stops before downloading when CDC doesn't offer the request: congenital syphilis with any `stratify_by`, county data for hepatitis or congenital syphilis, MSAs for anything but `"hiv"`, and regions for anything but `"estimate"` (#79, #87, #88).
* `get_atlas(extrapolate_to_tract)` is defunct: use `impute_suppressed()` then `extrapolate_from_county()` (#29).
* `impute_suppressed()` keeps rows marked "Data not available" by default (`not_available = "keep"`), so a state whose only rows are unavailable keeps its remainder; `"drop"` warns when it removes a whole state group (#110).

## New features

* Saved downloads are kept for 30 days, then downloaded again so CDC's revisions are picked up. They are stored in `tools::R_user_dir("CDCAtlas", "cache")`, and nothing is written when the package loads (#14, #56).
* Requests time out after 120 seconds. A CDC database timeout or network timeout is retried twice (after 10 and 30 seconds), and other failures give a plain explanation (#13).
* `clear_atlas_cache()` deletes every saved download.
* `extrapolate_from_county()` shares county counts across census tracts or block groups in proportion to ACS 5-year populations in the same stratum, with options for cases with no county (`no_county`, `spread_by`; #27, #54), empty strata (`empty_strata`), unknown race or age (`unknown_race`, `unknown_age`; #10), the race table (`race_table`), ages (`ages`; #53), missing ACS populations (`missing_population`; #52) and suppressed rows that couldn't be estimated (`unestimated`; #36) (#29).
* `extrapolate_from_county()` and `impute_suppressed(population = "acs")` work for age groups beyond HIV, for the diseases they support (not congenital syphilis). Each CDC age group counts the share of each Census age band its ages cover (#10).
* `extrapolate_from_county()` assigns 2010 tracts in Connecticut and Alaska to CDC's current counties (Connecticut's planning regions; Chugach and Copper River), so ACS releases from 2010 to 2019 cover them (#30).
* `extrapolate_from_county()` says why a county has no tracts when the ACS release predates CDC's current county codes: Connecticut with ACS 2009, 2020 and 2021, and Chugach and Copper River with ACS 2009 (#112).
* `extrapolate_from_county()` stops if the Census returns an ACS variable twice for one area (#111).
* `extrapolate_from_county(no_county = "spread")` is refused for CDC's HIV estimates, whose county data cover only Ending the HIV Epidemic counties; `no_county_cases()` says their remainder can include people in other counties and isn't necessarily cases with no county (#109).
* `extrapolate_from_county(no_county = "spread")` reports as unspread any share of the remainder that reached no tract (#114).
* `get_atlas()` labels a code it doesn't know as `"CDC code <id>"`, with a warning, rather than `NA`, and decodes CDC's "No reported cases" status (#106).
* `get_atlas()` requests each disease's own age groups, as AtlasPlus offers them. `"estimate"` now uses HIV's age groups (#11).
* `get_atlas()` requests each disease's own race groups: hepatitis now returns Asian/Pacific Islander and Other (#105).
* `get_atlas()` requests one disease, year and (for counties) state at a time, so large requests are less likely to time out (#77).
* `get_atlas()` warns when CDC returns some of a disease's indicators but not others, naming the missing indicator and year (#116).
* `get_atlas(geography = "region")` returns the four Census regions for `disease = "estimate"` (#32, #79).
* `get_atlas(refresh = TRUE)` downloads again instead of using a saved copy. For county data it also refreshes the state totals the later steps use (#14).
* `get_atlas()` accepts years up to the latest in the package's copy of CDC's code table (2025) (#16).
* `impute_suppressed()` estimates suppressed county counts from the state rate in the same stratum, capped at 4 by default, or sets them to zero with `method = "zero"`. With `population = "acs"`, it uses ACS county populations (#29).
* `impute_suppressed()` fills in only rows marked "Data suppressed". A missing count with any status other than "Data suppressed" or "Data not available" stays `NA`, is marked `"not estimated"` and is named in a warning (#107).
* `impute_suppressed(population = "acs")` refuses Knowledge of Status, whose denominator is people living with HIV, not residents (#108).
* `impute_suppressed()`, `no_county_cases()` and `extrapolate_from_county()` stop on negative, infinite or non-numeric `cases` or `population` (#115).
* `no_county_cases()` reports, for each state, the difference between CDC's state total and the county counts supplied (#27).

## Documentation

* New article, "About CDC AtlasPlus data". It covers what `data_status` means, rates and denominators, what CDC offers for each disease, CDC's county map and the Census denominators (#45).
* `?get_atlas` says what each disease returns, the rate units, and the Knowledge of Status columns (#18).
* The README and the "Getting started" and "Extrapolation to tracts" articles are rewritten, and the articles build on the website rather than as vignettes (#19, #20).
