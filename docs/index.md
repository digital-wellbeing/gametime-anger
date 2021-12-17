--- 
title: "Game play and well-being | Online analysis supplement"
author: "Matti Vuorre, Niklas Johannes, Kristoffer Magnusson, & Andy Przybylski"
date: "2021-11-18"
site: bookdown::bookdown_site
output: bookdown::bs4_book
url: https://digital-wellbeing.github.io/gametime-longitudinal
description: |
  Online analysis supplement for "Time spent playing video games is unlikely to impact well-being (Vuorre, Johannes, Magnusson, & Przybylski, 2021)"
biblio-style: apalike
---

# Introduction

This repository ([GitHub](https://github.com/digital-wellbeing/gametime-longitudinal) / [OSF](https://osf.io/fb38n/)) contains the data and code required to reproduce all analyses reported in our manuscript, *Time spent playing video games is unlikely to impact well-being* (Vuorre, Johannes, Magnusson, & Przybylski, 2021). These analyses are presented in the [Online analysis supplement](https://digital-wellbeing.github.io/gametime-longitudinal).

## Materials

- [Preprint](https://psyarxiv.com/8cxyh)  
  - A publicly available version of our manuscript in advance of peer-review and formal publication
- [GitHub repository](https://github.com/digital-wellbeing/gametime-longitudinal)  
  - A version controlled repository containing all the raw data and code in this project
- [OSF repository](https://osf.io/fb38n/)  
  - An archived permanent copy of the GitHub repository
- [Online analysis supplement](https://digital-wellbeing.github.io/gametime-longitudinal)
  - The output document of our analyses

In addition to raw data, the repository contains many survey and telemetry variables that we did not analyse, but that may be of interest to further analyses of gameplay and well being. 

## Reproducibility

The raw data are in the `Data/` directory of this repository. The code that we used to clean and analyse the data are organised in R Markdown (`.Rmd`) files in this directory, which are meant to be run in the sequence indicated by their numeric prefixes. To run all the cleaning and analyses, and compile the resulting document ([the online analysis supplement](https://digital-wellbeing.github.io/gametime-longitudinal)), run `bookdown::render_book()` in R or click "Build Book" in the RStudio IDE.