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

