# Internal AtlasPlus request helper (uncached)

Low-level helper that sends a POST JSON payload to the AtlasPlus
`qtOutputData` endpoint.

## Usage

``` r
.request_atlas_internal(payload)
```

## Details

Requires the internal `varvals` lookup table for factor labeling.
Implements rate limiting and user-agent headers for API politeness.
