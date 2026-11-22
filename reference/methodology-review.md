# Methodology background

Topic-by-topic background for the methods the package's estimates rely on.
Each topic states what the sources say and which points are assumptions or open
questions. Entries describe methods, not CDCAtlas code.

## Small counts and suppression

- HIV county counts are suppressed when the case count is 1–4 in counties
  under 500,000 people, when the population is under 100, under re-release
  agreements, and when a suppressed cell could be back-calculated
  ("CDC, AtlasPlus Technical Notes" in [web-sources.md](web-sources.md)). STI
  and TB follow different rules. The historical-extract ReadMe advises caution
  for numbers under 12.
- So a suppressed HIV county in a county under 500,000 is usually 1–4, but a
  cell suppressed to prevent back-calculation, or under a re-release agreement,
  can be larger. Any filled value is an assumption. Possible approaches include a fixed value,
  a state rate applied to the county population and capped at the range, and
  models that treat the value as censored. *Not yet sourced:* methods papers
  that use or compare these for surveillance counts.
- **Measured: a state rate capped at 4 loses cases once data are
  stratified.** HIV prevalence, county, ages 13 and older, 2022; each
  suppressed cell estimated as its state's rate times the county population,
  capped at 4. From the runs described under "County suppression in HIV
  prevalence, 2017, 2022 and 2024" in
  [atlasplus-notes.md](atlasplus-notes.md); no independent check.
  [observed 2026-11-21 and 2026-11-22]

  | 2022 | Unstratified | By sex | By age | By race |
  |---|---|---|---|---|
  | Estimates above 4 before capping | 77% | 71% | 50% | 15% |
  | Cases removed by the cap | 3,807 | 56,020 | 58,857 | 30,927 |
  | Counties below 90% of their own count | | 717 | 388 | 273 |
  | Counties above 110% of their own count | | 112 | 815 | 386 |
  | Cases short of county counts | | 22,763 | 21,241 | 17,590 |
  | Cases over county counts | | 313 | 5,528 | 2,479 |

  The last four rows sum each county's stratum values, published and
  estimated, and compare them with the county's own published unstratified
  count. The shortfalls imply that suppressed stratum cells in counties with a
  published count are often larger than 4. By age the estimates run over as
  well as under.

## Cases with no county

- A state's total can exceed the sum of its published counties by more than its
  suppressed counties would hold if each held 1–4 cases ("Responses" in
  [atlasplus-notes.md](atlasplus-notes.md)). One explanation is cases whose
  county of residence is unknown or not released, but CDC's documentation read
  so far does not say, and complementary suppression means some suppressed
  counties hold more than 4. Treating the gap as cases to allocate is an
  assumption that needs that evidence first.
- If the gap is cases with no county, spreading them across a state's counties
  assumes they resemble cases with a county. Which counties and in what proportion (population or cases)
  are assumptions, not documented by CDC. *Not yet sourced.*
- **Measured: the gap after capped estimates.** State total, minus the sum of
  published county counts, minus estimates for suppressed counties (state rate
  times county population, capped at 4), as a signed sum over states. HIV
  prevalence, ages 13 and older; same runs and caveats as the measured entry
  under "Small counts and suppression". [observed 2026-11-21]

  | Stratification | 2017 | 2022 | 2024 |
  |---|---|---|---|
  | None | 33,025 | 20,028 | 17,530 |
  | Sex | 51,988 | 41,696 | 40,514 |
  | Age group | 39,541 | 33,238 | 34,698 |
  | Race/ethnicity | 44,429 | 33,659 | 32,382 |

  In 2022 the gap was a large share of some states' totals. Unstratified:
  Alaska 87%, Rhode Island 73%, Nebraska 60%, Idaho 47%, Vermont 19%, Wyoming
  14%. By sex: New Hampshire 94%, Maine 93%, Idaho 81%, Alaska 81%, Rhode
  Island 74%. That is consistent with those states' county counts being
  suppressed wholesale, in which case the gap is mostly cases in suppressed
  counties, not cases with no county. *Cause not verified.*

## Geography vintages

- Counties change: Connecticut adopted planning regions as county-equivalents
  in the 2020s, and several Alaska, South Dakota and Virginia areas changed in
  the 2010s ("Census county changes" entries in [web-sources.md](web-sources.md)).
- An ACS release uses one tract vintage: 2010 tracts through the 2019 ACS
  (2015–2019 5-year), 2020 tracts from the 2020 ACS (2016–2020 5-year) on
  ("ACS geography boundaries by year" in [web-sources.md](web-sources.md)).
  Counts on a newer county map need a relationship file to reach older tracts
  ("2020 relationship files").

## Allocation to small areas

- Sharing a county count across its tracts in proportion to population assumes
  every tract has the county rate. It conserves county totals by construction,
  so totals adding up does not show the spatial pattern is right.
- For target areas that cross county lines, population-weighted areal
  interpolation uses block populations as weights ("Walker, section 7.3.2" in
  [web-sources.md](web-sources.md)).
- Sub-county data (such as ZIP-level prevalence from AIDSVu) can supply
  relative weights within a county; their own suppression and denominators
  apply ("AIDSVu, data methods" in [web-sources.md](web-sources.md)).

## Age universes and partial bands

- "AtlasPlus includes NHSS data for persons aged 13 years and older", and its
  HIV rates are per 100,000 persons aged 13 and older ("CDC, AtlasPlus Technical
  Notes" in [web-sources.md](web-sources.md)). NHSS itself also covers children
  under 13. AIDSVu's denominators are the population aged 13 and older. AtlasPlus's
  code table has "Ages 13 years and older" (650) beside "All age groups" (651)
  ([atlasplus-notes.md](atlasplus-notes.md)). The age universe of each
  indicator other than HIV is a separate question, to check per indicator.
- ACS sex-by-age bands do not start at 13: the 10–14 band must be split. Counting
  2/5 of it assumes ages are spread evenly within the band; that is an
  assumption, not a Census figure.
- STD age groups differ from HIV's (for example 15–19, 20–24), so one set of
  population bands does not fit every disease.
