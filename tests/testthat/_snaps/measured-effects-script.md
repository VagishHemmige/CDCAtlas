# 'latest' steps back only on evidence that the year is released in part

    Code
      latest()
    Condition
      Error in `script_env$measured_effects_latest_year()`:
      ! Couldn't establish whether CDC has released hiv 2025, unstratified: the state response has indicators that weren't asked for.
      x Returned: Chlamydia.
      x Asked for: HIV diagnoses, HIV prevalence.
      i The response doesn't match the request, so nothing is inferred from it and an earlier year is not used instead.

---

    Code
      latest()
    Condition
      Error in `script_env$measured_effects_latest_year()`:
      ! Couldn't establish whether CDC has released hiv 2025, unstratified: the state response has rows for another year.
      x Returned: 2024.
      x Asked for: 2025.
      i The response doesn't match the request, so nothing is inferred from it and an earlier year is not used instead.

---

    Code
      latest()
    Condition
      Error in `script_env$measured_effects_latest_year()`:
      ! Couldn't establish whether CDC has released hiv 2025, unstratified: the state request failed.
      i A failed request is not evidence that the year is unreleased, so an earlier year is not used instead.
      i Give the years in `MEASURED_YEARS` instead of "latest" to go on without this check.
      Caused by error in `.request_atlas()`:
      ! CDC AtlasPlus returned no data for this request.

