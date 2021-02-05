# skillsengineeR

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/dmolitor/skillsengineeR.svg?branch=master)](https://travis-ci.com/dmolitor/skillsengineeR)
[![codecov](https://codecov.io/gh/dmolitor/skillsengineeR/branch/master/graph/badge.svg)](https://codecov.io/gh/dmolitor/skillsengineeR)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/skillsengineeR)](https://cran.r-project.org/package=skillsengineeR)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/skillsengineeR)](https://cran.r-project.org/package=skillsengineeR)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The goal of skillsengineeR is to provide a convenient wrapper around the 
[SkillsEngine](https://skillsengine.com/) API. skillsengineeR provides functions for each
API endpoint and returns friendly output in the form of lists and data.frames.
It also performs basic error checking to ensure the user has entered queries
in the expected format, and provides functions that guide the user through the steps
of securely setting up API authorization keys.


## Installation

You can install skillsengineeR with:

``` r
devtools::install_github("https://github.com/dmolitor/skillsengineeR")
```

## Setting up API authorization
- The first step is either to sign up for [test API access](https://accounts.skillsengine.com/users/sign_up),
  or purchase full API access. Once this is completed, the Client ID and your Client Secret tokens
  should be accessible on the SkillsEngine account portal.

  API tokens, and other private authorization tokens, should never be hard-coded into scripts, but rather
  should be accessed securely in scripts. One way to do this is to set them as global variables in the
  .Renviron file. To set this up simply run:
  ```r
  set_id_secret()
  #> RStudio will open your .Renviron file in a new tab. Enter your Client ID and Client Secret as described below.
  #> Using the following naming convention is necessary for 'get_id_secret()' to work:
  #>  SKILLSENGINE_CLIENT_ID=<client id>
  #>  SKILLSENGINE_CLIENT_SECRET=<client secret>
  #> * Edit 'C:/Users/DanielMolitor/Documents/.Renviron'
  #> * Restart R for changes to take effect
  #> 
  #> Restart your R Session!
  ```

- Access the Client ID and Secret set in .Renviron
  ```r
  client <- get_id_secret()
  client_id <- client$id
  client_secret <- client$secret
  ```

- Obtain the authorization token, which is necessary for every SkillsEngine API endpoint
  ```r
  token <- get_access_token(client_id, client_secret)
  #> Valid request
  ```

- Confirm that the authorization token is valid
  ```r
  test_access(token)
  #> [1] "Token is authorized"
  ```

- Check the data freshness
  ```r
  freshness_tag <- freshness(token)
  #> Valid request
  freshness_tag
  #> [1] "2bf95b60aed242dc12d283bf19fc0b9e"
  ```
  The freshness tag can be stored and checked periodically to ensure that data is still fully up-to-date.
