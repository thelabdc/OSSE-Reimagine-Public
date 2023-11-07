# OSSE-Reimagine-Public

@authors: [Rebecca Johnson](rebecca.johnson@dc.gov), [Ryan Moore](ryan.moore2@dc.gov)

## Introduction

This repo contains code relevant for the pre-analysis plan for Reimagining DC High Schools.

- [Project page](https://thelabprojects.dc.gov/reimagine-dc-high-schools)


#### Requirements:

For the power code, relevant packages are listed at the beginning of the script. They are reproduced here:

```
library(estimatr)
library(here)
library(reshape2)
library(scales)
library(tidyverse)
```


## Organization

The repo is organized in two primary directories:

- `code`
- `figs`

Additionally, this repository includes `README`, `.gitignore`, and `.Rproj` 
files.

## `/code/`

This directory includes these files:

[00_estimatepower.R](https://github.com/thelabdc/OSSE-Reimagine-Public/blob/main/code/00_estimatepower.R)
- Takes in:
  - Parameters on sample size and base rates for the survey and administrative data outcomes
  - Note that many of these parameters are ballpark figures / estimates --- e.g., for SY 2023-2024, we are posting the pre-analysis plan prior to having final counts on the number of treatment group and comparison group participants, so needed to make an educated guess at these counts 
- What it does:
  - Uses simulation to estimate the statistical power at different hypothetical effect sizes/scenarios involving the matching/weighting process and survey response rates
  - These simulations cover both the administrative data outcomes and the survey data outcomes
- Outputs:
  - `figs/power_curve.png`: power curve


