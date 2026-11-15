# Methodology background

Topic-by-topic background for the methods the package's estimates rely on.
Each topic states what the sources say and which points are assumptions or open
questions. Entries describe methods, not CDCAtlas code.

## Small counts and suppression

- State and county values are suppressed under data re-release agreements
  ("CDC, National HIV Surveillance System" in [web-sources.md](web-sources.md)),
  and the historical-extract ReadMe advises caution for numbers under 12.
  Neither states the numeric threshold for current county suppression.
- A suppressed county is known only to lie in a range (for example 1 to 4), so
  any filled value is an assumption. Possible approaches include a fixed value,
  a state rate applied to the county population and capped at the range, and
  models that treat the value as censored. *Not yet sourced:* methods papers
  that use or compare these for surveillance counts.

## Cases with no county

- A state's total can exceed the sum of its published and suppressed counties
  by more than suppression allows ("Responses" in
  [atlasplus-notes.md](atlasplus-notes.md)). The excess is most likely cases
  whose county of residence is unknown or not released; CDC's documentation
  read so far does not say.
- Spreading those cases across a state's counties assumes they resemble cases
  with a county. Which counties and in what proportion (population or cases)
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

- HIV surveillance case definitions apply to persons aged 13 and older ("CDC,
  National HIV Surveillance System" in [web-sources.md](web-sources.md)), and
  AIDSVu's denominators are the population aged 13 and older. AtlasPlus's
  code table has "Ages 13 years and older" (650) beside "All age groups" (651)
  ([atlasplus-notes.md](atlasplus-notes.md)); which one each indicator uses is
  set per request.
- ACS sex-by-age bands do not start at 13: the 10–14 band must be split. Counting
  2/5 of it assumes ages are spread evenly within the band; that is an
  assumption, not a Census figure.
- STD age groups differ from HIV's (for example 15–19, 20–24), so one set of
  population bands does not fit every disease.
