# Keep every saved CDC response from these tests out of the user's own cache.
withr::local_envvar(
  R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = teardown_env()),
  .local_envir = teardown_env()
)
