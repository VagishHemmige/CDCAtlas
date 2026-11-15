# .atlas_init_data() errors when CDC refuses the request

    Code
      .atlas_init_data()
    Condition
      Error:
      ! CDC AtlasPlus set-up request failed (HTTP status 503).
      i CDC says: Server busy

# .atlas_init_data() errors when the code table is missing

    Code
      .atlas_init_data()
    Condition
      Error:
      ! CDC AtlasPlus set-up data has no varvals code table.
      i CDC may have changed AtlasPlus.

# clear_atlas_cache() deletes every saved download

    Code
      deleted <- clear_atlas_cache()
    Message
      Deleted 2 saved CDC downloads.

# clear_atlas_cache() says when there is nothing to delete

    Code
      deleted <- clear_atlas_cache()
    Message
      No saved CDC downloads to delete.

# get_atlas() checks `refresh`

    Code
      get_atlas("hiv", "national", 2020, refresh = NA)
    Condition
      Error in `get_atlas()`:
      ! `refresh` must be `TRUE` or `FALSE`.

# an unknown code is kept as 'CDC code <id>', with a warning (#106)

    Code
      out <- .request_atlas_internal(list(VariableIDs = "210"))
    Condition
      Warning:
      CDC returned codes this version of CDCAtlas doesn't know.
      * data_status: 9999
      * race_ethnicity: 9998
      i They are labelled "CDC code <number>". Please report them at <https://github.com/VagishHemmige/CDCAtlas/issues>.

