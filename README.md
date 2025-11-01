
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CDCAtlas

<!-- badges: start -->

<!-- badges: end -->

`CDCAtlas` provides a **polite, cache-aware R client** for selected CDC
**AtlasPlus** endpoints.

AtlasPlus is not a stable, public API.  
This package aims for safe and respectful usage:

- identifies via a custom **User-Agent**
- applies **rate limiting** and **retry backoff**
- **caches responses** to reduce server traffic
- handles schema changes gracefully when possible

## Installation

You can install the development version of CDCAtlas from
[GitHub](https://github.com/) with:

install.packages(“devtools”)
devtools::install_github(“VagishHemmige/CDCAtlas”)

## Example

This is a basic example which shows you how to solve a common problem:

``` r
#library(CDCAtlas)
## basic example code
```
