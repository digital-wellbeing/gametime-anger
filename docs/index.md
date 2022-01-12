--- 
title: "Game play and aggressive affect | Online analysis supplement"
author: "Niklas Johannes, Matti Vuorre, Kristoffer Magnusson, & Andy Przybylski"
date: "2022-01-12"
site: bookdown::bookdown_site
output: bookdown::bs4_book
url: https://digital-wellbeing.github.io/gametime-anger
description: |
  Time spent playing two online shooters has no measurable effect on aggressive affect* (Johannes, Vuorre, Magnusson, & Przybylski, 2021)"
biblio-style: apalike
---

# Introduction

This repository ([GitHub](https://github.com/digital-wellbeing/gametime-anger) / [OSF](https://osf.io/zd6c2/)) contains the data and code required to reproduce all analyses reported in our manuscript, *Time spent playing two online shooters has no measurable effect on aggressive affect* (Johannes, Vuorre, Magnusson, & Przybylski, 2022). These analyses are presented in the [Online analysis supplement](https://digital-wellbeing.github.io/gametime-anger).

## Materials

- [Preprint](https://psyarxiv.com/)  
  - A publicly available version of our manuscript in advance of peer-review and formal publication
- [GitHub repository](https://github.com/digital-wellbeing/gametime-anger)  
  - A version controlled repository containing all the raw data and code in this project
- [OSF repository](https://osf.io/zd6c2/)  
  - An archived permanent copy of the GitHub repository as well as any other study materials.
- [Online analysis supplement](https://digital-wellbeing.github.io/gametime-anger)
  - The output document of our analyses, rendered as a website.

Note: This project uses data and code from our previous study: [Vuorre et al. (2021)](https://psyarxiv.com/8cxyh/). The code thus is mostly taken from that project, with adjustments for the current study.

## Reproducibility

The raw data are in the `data/` directory of this repository. The code that we used to clean and analyze the data are organized in R Markdown (`.Rmd`) files in this directory, which are meant to be run in the sequence indicated by their numeric prefixes. To run all the cleaning and analyses, and compile the resulting document ([the online analysis supplement](https://digital-wellbeing.github.io/gametime-longitudinal)), run `bookdown::render_book()` in R or click "Build Book" in the RStudio IDE.
