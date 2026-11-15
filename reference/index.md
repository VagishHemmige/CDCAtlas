# Package index

## Data Access

Download data from CDC AtlasPlus, and manage saved downloads.

- [`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
  : Retrieve surveillance data from CDC AtlasPlus
- [`clear_atlas_cache()`](https://vagishhemmige.github.io/CDCAtlas/reference/clear_atlas_cache.md)
  : Delete saved CDC downloads

## Estimates below county level

Fill in suppressed counts, report cases with no county, and share county
counts across tracts or block groups.

- [`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
  : Fill in suppressed county counts
- [`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
  : Cases in state totals that no county accounts for
- [`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
  : Share county counts across smaller Census areas
