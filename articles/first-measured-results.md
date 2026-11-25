# First measured results (provisional)

> **Provisional.** These are first results from two automated runs of
> the package’s pipeline (21 November 2026 and 22 November 2026 UTC) and
> one live test run ([22 November 2026
> UTC](https://github.com/VagishHemmige/CDCAtlas/actions/runs/45017608026)),
> on CDC AtlasPlus data as returned on those dates. Unless a section
> says otherwise, they cover HIV prevalence (people living with
> diagnosed HIV), ages 13 and older, for 2017, 2022 and 2024, with
> default settings. They have not been independently checked. The
> defaults they describe may change. Fuller articles with figures will
> replace this page.

This page reports what the package’s steps did to real data: how much
CDC suppresses, what
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
fills in, and what is left over. It has no code that downloads data. For
what CDC’s numbers mean, see [About CDC AtlasPlus
data](https://vagishhemmige.github.io/CDCAtlas/articles/about-atlasplus-data.md);
for the tract estimates, see [Extrapolation to
tracts](https://vagishhemmige.github.io/CDCAtlas/articles/extrapolation.md).

[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
keeps CDC’s published counts as they are and fills in suppressed ones.
The counts it fills in are modeled estimates, not counts CDC withheld;
its `cases_source` column says which is which (`"published"`, or the
method that filled the count in). Nothing here says how large any
suppressed count is.

## How much CDC suppresses at county level

The more finely the data are divided, the more county rows are
suppressed.

| Share of county rows suppressed | 2017 | 2022 | 2024 |
|---------------------------------|------|------|------|
| Not stratified                  | 15%  | 14%  | 13%  |
| By sex                          | 44%  | 43%  | 42%  |
| By age                          | 50%  | 49%  | 48%  |
| By race                         | 60%  | 58%  | 57%  |

## What the default imputation does

By default,
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
estimates a suppressed county row as the state’s rate times the county’s
population, and caps the estimate at 4. In 2022 the cap, not the state
rate, set most of the estimates unstratified and by sex, about half by
age, and about one in seven by race.

| 2022 | Not stratified | By sex | By age | By race |
|----|----|----|----|----|
| Estimates above 4 before the cap | 77% | 71% | 50% | 15% |
| Cases the cap takes off the estimated total | 3,807 | 56,020 | 58,857 | 30,927 |

These are facts about the estimates. An estimate above 4 does not show
that the hidden count is above 4. The column `cases_uncapped` lets you
count the same thing for your own request.

Changing the settings moved the 2022 national total of published and
estimated county cases by this much:

| Setting              | Not stratified | By sex      |
|----------------------|----------------|-------------|
| `method = "zero"`    | −0.15%         | −0.90%      |
| `cap = Inf`          | +0.35%         | +5.28%      |
| `floor = 1`          | under 0.01%    | under 0.01% |
| `population = "acs"` | under 0.01%    | under 0.01% |

## Stratified estimates against the county’s own total

A county’s count is often published while some of its counts by sex, age
or race are suppressed. Adding up such a county’s strata, published and
estimated, should give about the county’s published count. Under the
default it often does not, in either direction.

| 2022 | By sex | By age | By race |
|----|----|----|----|
| Counties with a published count above 0 (base for the percentages) | 2,644 | 2,644 | 2,624 |
| Strata sum to under 90% of the county’s count | 717 (27%) | 388 (15%) | 273 (10%) |
| Strata sum to over 110% of the county’s count | 112 (4%) | 815 (31%) | 386 (15%) |
| Cases short, added over all counties compared, misses of any size | 22,763 | 21,241 | 17,590 |
| Cases over, added over all counties compared, misses of any size | 313 | 5,528 | 2,479 |

So stratified estimates under the default do not add up to the
unstratified ones. By sex, 27% of the counties compared fell short by
more than 10% and 4% ran over by more than 10%. By age, 15% fell short
by more than 10% and 31% ran over by more than 10%. By race, 10% and
15%. Only the two percentage rows use the 10% line; the case totals
below them include misses of every size. A net shortfall nationally does
not mean every county is underestimated. Every county beyond the 90% or
110% line had two or more suppressed strata.

The comparison uses counties whose unstratified count is published. It
leaves out counties with a stratum row absent, a stratum with no count
(20 counties by race), or exactly one suppressed stratum (none in 2022).
The two percentage rows also leave out the 132 counties whose published
count is 0; the case totals include them. By race, a county can fall
short with no suppressed stratum at all, because CDC puts cases of
unknown race in no row.

## Cases in state totals not accounted for by county rows

[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
reports each state’s total minus its published county counts minus the
estimates for its suppressed counties. Added over states (shortfalls and
excesses offset each other), after the default imputation and with the
default `no_county = "report"`:

|                           | 2017      | 2022      | 2024      |
|---------------------------|-----------|-----------|-----------|
| Sum of state totals       | 1,002,796 | 1,102,798 | 1,157,995 |
| Remainder, not stratified | 33,025    | 20,028    | 17,530    |
| Remainder, by sex         | 51,988    | 41,696    | 40,514    |
| Remainder, by age         | 39,541    | 33,238    | 34,698    |
| Remainder, by race        | 44,429    | 33,659    | 32,382    |

In a few states the remainder, the shortfall of the county rows against
the published state total, was a large share of that total in 2022, not
stratified, under the same defaults:

| State        | Share of the state’s total, 2022 |
|--------------|----------------------------------|
| Alaska       | 87%                              |
| Rhode Island | 73%                              |
| Nebraska     | 60%                              |
| Idaho        | 47%                              |
| Vermont      | 19%                              |
| Wyoming      | 14%                              |

Shares by sex, age or race are not shown by state: for some states they
could come close to a single county’s count.

This is consistent with those states’ county counts being largely
suppressed. The cause has not been verified. The remainder measures how
far the county rows fall short of the state’s published total; it does
not say where those cases are. County or tract estimates for such a
state leave out about the share shown: most of the state’s total in
Alaska, Rhode Island and Nebraska, and smaller shares in Idaho, Vermont
and Wyoming.
[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
and
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
now warn when more than 25% of a state’s cases are in no county row, and
name the states with their shares.

## What CDC’s data look like inside a county

These come from the [live test
run](https://github.com/VagishHemmige/CDCAtlas/actions/runs/45017608026),
for HIV diagnoses and HIV prevalence. They are counts of counties that
the run printed to its log. The run failed on a different check, so they
are observations, not results of tests that passed. For the sums, it
added a county’s published stratum counts and compared the sum with the
county’s published count.

Where every stratum of a county is published, the strata sum exactly to
the county’s count by sex and by age, in every year checked: diagnoses
2017, 2022, 2024 and 2025, and prevalence 2017, 2022 and 2024. By race,
diagnoses also sum exactly, but prevalence sums to less in about a fifth
of such counties and never to more. That is consistent with unknown race
being in no row.

| Prevalence by race, counties with every stratum published | 2017 | 2022 | 2024 |
|-----------------------------------------------------------|------|------|------|
| Counties                                                  | 233  | 244  | 244  |
| Strata sum to less than the county’s count                | 47   | 51   | 54   |

Counties with a published count and exactly one suppressed stratum cell
are rare. There were none by sex, age or race for diagnoses in 2022,
2024 and 2025, or for prevalence in 2022 and 2024. In 2017 there were a
few: 1 for diagnoses by age, 2 for diagnoses by race and 5 for
prevalence by race.

## What this means for users now

- **Unstratified estimates.** The national total of county cases changed
  little with the imputation settings. That was measured nationally
  only: it says nothing about how much any one state’s, county’s or
  tract’s estimates change. In the states listed above, a large share of
  each state’s total is not accounted for by county rows under the
  default, and their county and tract estimates leave that share out.
- **Estimates by sex, age or race.** Under the default, the county rows
  fell short of the state totals by about 3% to 4% of national cases in
  2022, against about 2% without stratification, and by far more in some
  states. Individual counties can be over- or underestimated.
- **Report your settings.** Say which `method`, `cap`, `floor` and
  `population` you used, and consider running the alternatives above as
  sensitivity analyses.
- **A better method is being designed.** It will fit stratified
  estimates to each county’s published total. Until then, treat
  stratified estimates for counties with suppressed strata with caution.
- **Race denominators need a closer look.** Census and CDC populations
  by race can differ for the same county; this has not yet been measured
  carefully enough to report.
