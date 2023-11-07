# OSSE-Reimagine

@authors: [Rebecca Johnson](rebecca.johnson@dc.gov), [Josephine Freis](josephine.freis@dc.gov), [Josh Hall](josh.hall@dc.gov), [Oriana Ma](oriana.ma@dc.gov)

## Introduction

This repo contains ...

The analysis was pre-registered on the Open Science Framework (OSF) 
[here](OSF URL).  The final report is [here](link).


#### Requirements:

The primary modules used here for Python are ...
The primary packages used here for R are ...
The relevant packages and modules are included at the top of each Python script, 
R script, and Jupyter notebook.

## Organization

The repo is organized in four primary directories:

- `code`
- `data`
- `docs`
- `figs`

Additionally, this repository includes `README`, `.gitignore`, and `.Rproj` 
files.

## `/code/`

This directory includes these files:

[00_estimatepower.R](https://github.com/thelabdc/OSSE-Reimagine/blob/main/code/00_estimatepower.R)
- Takes in: parameters on sample size and base rates for the survey and administrative data outcomes
- What it does: uses simulation to estimate the statistical power at different hypothetical effect sizes/scenarios involving the matching/weighting process and response rates
- Outputs:
  - `figs/power_curve.png`: power curve


## `/data/`

Links for the open data sets analyzed are below.

* [Description 1](Link1)

The code below can be used to get these data sets.

```bash
wget https://  [here]
```

### Sensitive data

Several data sets are are _not_ included in this repository due to the sensitive nature of the data.  They include

## `/docs/`

## `/figs/`
