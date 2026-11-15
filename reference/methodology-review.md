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
