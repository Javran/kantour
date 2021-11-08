module Kantour.Core.KcData.Master.Fetch
  (
  )
where

{-
  TODO: WIP
  Fetches master data (api_start2).

  Environment variables:

  - KANTOUR_CACHE_BASE: serves as base path for the cache.

    + <base>/api_start2.json: the data
    + <base>/api_start2.source.yaml: metadata to indicate where it comes from

      for this yaml file:

      source: one of github:... or url:... below, required.
      commit: required for github source.

    the cache is considered invalid if one of the following condition is met:
    - either file is missing or metadata does not contain required fields
    - source indicated in metadata differs from KANTOUR_MASTER_DATA_SOURCE.
      (note that `stock` and `file:` option should completely bypass caching
      mechanism so they won't trigger anything related to caching invalidation)
    - source is from github, and recorded commit string isn't the same
      as one present on GitHub.

    when cache is valid, data from cache will be loaded, otherwise those files are
    overwritten if KANTOUR_MASTER_DATA_SOURCE is one of `github:` or `url:` upon
    successful retrieve.

  - KANTOUR_MASTER_DATA_SOURCE:
    one of:
    + stock (from data shipped with this lib)
      (no caching)
    + github:kcwiki/kancolle-data:master/api/api_start2.json
      fetches from latest commit as specified
    + url:<url address>
      use this if we want to pin down a specific version somewhere
    + file:<filepath> load it locally, no caching.

 -}
