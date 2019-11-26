---
title: "Codebook: Donations to JustGiving Fundraiser pages with fundraiser info "
author: "David Reinstein, Toby Jolly, Gerhard Riener"
output:
  html_document:
    toc: true
    toc_depth: 4
    toc_float: true
    code_folding: 'hide'
    self_contained: true
  pdf_document:
    toc: yes
    toc_depth: 4
    latex_engine: xelatex
---

## Setup and import data 



## Add meta-data


```r
metadata(fdd)$name <- "Donations to Justgiving fundraising pages with page info"
metadata(fdd)$description <- "Dataset: All donations to JustGiving pages pulled via API from 2017-19 for 'highly effective' charities; reduced to one row per donation, with info on fundraising pages too"
metadata(fdd)$identifier <- "doi:TBA"
metadata(fdd)$datePublished <- "TBA"
metadata(fdd)$creator <- list(
  list(
  "@type" = "Person",
  givenName = "David", familyName = "Reinstein",
  email = "daaronr@gmail.com",
  affiliation = list("@type" = "Organization",
                     name = "University of Exeter")),
  list(
  "@type" = "Person",
  givenName = "Gerhard", familyName = "Riener",
  email = "gerhard.riener@gmail.com",
  affiliation = list("@type" = "Organization",
                     name = "University of Düsseldorf"))
)
  list(
  "@type" = "Person",
  givenName = "Toby", familyName = "Jolly")
```

```
## $`@type`
## [1] "Person"
## 
## $givenName
## [1] "Toby"
## 
## $familyName
## [1] "Jolly"
```

```r
metadata(fdd)$citation <- "Jolly, Reinstein, Riener (2019)"
metadata(fdd)$url <-
  "TBA"
metadata(fdd)$temporalCoverage <- "2017-19)"
metadata(fdd)$spatialCoverage <- "Mainly UK"
metadata(fdd)$keywords <- c("Charitable giving","Social fundraising")
```

## Codebook



