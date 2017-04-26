# ghap

This R package contains a few useful functions for checking out, updating, organizing, and loading datasets stored on GHAP. This package is only useful to those who know what GHAP is and have access to the platform. It's only being made available publicly because it is much easier to install that way.

The main goal of this package currently is to make it very convenient to get data that you want that is stored on the GHAP git server with a single command, and manage behind the scenes more painful things like finding the proper repository link, git credentials, and organizing the checked-out repositories.

## Installation

You can install ghap from github with:

```r
# install.packages("devtools")
devtools::install_github("HBGDki/ghap")
```

## How to use

There are three main functions to use from this package: `get_study_list()`, `get_study_list_anthro()`, and `use_study()`. The first two functions provide a data frame of study meta data and the third function loads data for a given study ID that can be obtained from the meta data. If the git repository for the data has not been checked out, it will first check the data out. If it is checked out, it will do a pull to get updates before loading the data.

### One time only setup

With this package, all GHAP repositories are automatically organized and stored in a directory structure under a base git directory. Before using the main functions of the package, you need to specify where you want this directory to go. Make sure it is set to be on your persistent volume so it will still be there next time you start a new instance.

```r
set_git_base_path("~/git")
```

This will set an R environment variable that will be present in all subsequent sessions so that you never need to specify it again.

You can check that this has been set appropriately with the following:

```r
get_git_base_path()
```

### Listing studies

To know what data you want to use, you need to have a listing of possible data sets to look at. The functions `get_study_list()` and `get_study_list_anthro()` do this. They return a data frame of meta data that you can use to search for data sets you would like to use in your analyses.

```r
studies <- get_study_list()
```

When you run this for the first time, it will attempt to check out the git repositories that contain the study meta data and place them in your git base directory. Consequently it will ask you for your git credentials. On Linux, these credentials will be cached for the rest of your session so that you don't have to enter them again for all subsequent git operations. On Windows, a link to a utility will be opened in your web browser, which after installing will allow you to cache your credentials indefinitely.

Let's see what type of output to expect:

```r
head(studies, 3)
```

```
# A tibble: 3 × 42
   study_id short_id                         short_description
      <chr>    <chr>                                     <chr>
1 AAP-NSECH     <NA> National Survey of Early Childhood Health
2  AFCARS-A     afca                           AFCARS Adoption
3  AFCARS-F     afcf                        AFCARS Foster Care
# ... with 39 more variables: study_description <chr>, subject_count <int>,
#   alternate_id <chr>, status <chr>, grant_folder <chr>,
#   analysis_folder <chr>, study_type <chr>, intervention_type <chr>,
#   age_lower_limit <int>, units_for_age_lo <chr>, age_upper_limit <int>,
#   units_for_age_up <chr>, start_year <int>, stop_year <int>,
#   date_data_recd <chr>, country <chr>, population <chr>,
#   anthropometric_data <chr>, gest_age <chr>, neurocog_data <chr>,
#   ses_or_family_info <chr>, nutrition <chr>,
#   enteropathy___microbiology <chr>, morbidity___mortality <chr>,
#   other_laboratory <chr>, study_url <chr>, pi_name <chr>,
#   pi_contact_info <chr>, notes <chr>, mou_counterparty <chr>,
#   dm_contact_information <chr>, phi_present <chr>, data_accepted <chr>,
#   scope_of_data <chr>, data_restrictions <chr>, kikm_uri <chr>,
#   studyid <chr>, hasdata <int>, fstudy_id <chr>
```

There are a lot of variables here that we can use to narrow down on the study we are interested in.

There's also a function, `get_study_list_anthro()`, which returns a subset of studies that are guaranteed to return data in a standard format that includes anthropometry when used with `use_study()`. Also note that there very well may be studies in the larger list returned by `get_study_list()` that have the same kind of structure too but for some reason may not not included in the results of this function.

```r
astudies <- get_study_list_anthro()
```

Let's look at some columns of the last 3 records:

```r
tail(astudies[, c("study_id", "short_id", "short_description")], 3)
```

```
# A tibble: 3 × 3
         study_id short_id                              short_description
            <chr>    <chr>                                          <chr>
1      UVG Growth     gual Univ del Valle de Guatemala Longitudinal Study
2 WASH-Bangladesh      wsb                 WASH Benefits Bangladesh Trial
3        ZVITAMBO     zvit                                       ZVITAMBO
```

### Reading studies

The main function of this package is `use_study()`. When supplied with a study ID (either long or short ID that can be found in the meta data as seen above) will read in the appropriate data set for that study. If the repository for the data hasn't been checked out, it will check it out, and it will also do a git pull for updates prior to reading.

For example, suppose I want to get the data for the WASH Benefits study. I can obtain it easily with:

```r
wsb <- use_study("wsb")
```

That's it.

This function is primarily designed for reading in data of a specific format that includes anthropometry and hence it favores studies available from `get_study_list_anthro()` since for those we have specific paths to data files to read. In the case of other data sets, we aren't always guaranteed what form the data takes and which file to read, but in that case a guess is made and a message is displayed to that effect to the user.

### Checking out all repositories

While the main functionality of this package is to help you get the data you need with `use_study()` (all the git stuff is secondary), it can be a useful quick way to check out all the data repositories with the intention of reading and working with the data later (as checking out can be a lengthy process that you might want to do up front).

```r
astudies <- get_study_list_anthro()
for (id in astudies$short_id)
  tmp <- use_study(id)
```




