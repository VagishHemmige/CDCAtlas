# region requests for reported data stop before asking CDC

    Code
      get_atlas("hiv", "region", 2022)
    Condition
      Error in `get_atlas()`:
      ! `geography = "region"` is only available for `disease = "estimate"`.
      i CDC doesn't return regions for reported data such as "hiv".
      i State data are available with `geography = "state"`; adding states up needs care with suppressed counts, and rates must be recomputed from cases and population.

# a failed request passes on CDC's message without the server trace

    Code
      get_atlas("hiv", "state", 2017)
    Condition
      Error:
      ! CDC AtlasPlus request failed (HTTP status 400).
      i CDC says: Execution Timeout Expired. The timeout period elapsed prior to completion of the operation or the server is not responding.
      i If this keeps happening, CDC may have changed AtlasPlus. Please report it at <https://github.com/VagishHemmige/CDCAtlas/issues>.

# a failed request without a readable message still explains itself

    Code
      get_atlas("hiv", "state", 2017)
    Condition
      Error:
      ! CDC AtlasPlus request failed (HTTP status 404).
      i If this keeps happening, CDC may have changed AtlasPlus. Please report it at <https://github.com/VagishHemmige/CDCAtlas/issues>.

