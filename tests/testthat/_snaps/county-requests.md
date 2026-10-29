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

