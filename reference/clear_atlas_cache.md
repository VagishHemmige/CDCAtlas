# Delete saved CDC downloads

[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md),
and the functions that compare county data with CDC's state totals, save
each response from CDC AtlasPlus and reuse it for 30 days.
`clear_atlas_cache()` deletes every saved response, so the next request
for any data downloads it again. To download one request again, use
`get_atlas(refresh = TRUE)` instead.

The responses are saved in the folder given by
`tools::R_user_dir("CDCAtlas", "cache")`.

## Usage

``` r
clear_atlas_cache()
```

## Value

The number of saved responses deleted, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
# Deletes your saved downloads, so not run here
clear_atlas_cache()
} # }
```
