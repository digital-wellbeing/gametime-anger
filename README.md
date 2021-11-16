# Longitudinal analysis of game time on feelings of anger

This repository ([GitHub](https://github.com/digital-wellbeing/gametime-anger) / [OSF](https://osf.io/)) contains the data and code required to reproduce all analyses reported in our manuscript, *Playing two violent video games has [] effect on feelings of anger: A second-ary analysis of the data of Vuorre et al. (2021)* (Johannes, Vuorre, Magnusson, & Przybylski, 2021). These analyses are presented in the [Online analysis supplement](https://digital-wellbeing.github.io/gametime-anger).

## Materials

- [Preprint](https://psyarxiv.com/)  
  - A publicly available version of our manuscript in advance of peer-review and formal publication
- [GitHub repository](https://github.com/digital-wellbeing/gametime-anger)  
  - A version controlled repository containing all the raw data and code in this project
- [OSF repository](https://osf.io/)  
  - An archived permanent copy of the GitHub repository as well as any other study materials.
- [Online analysis supplement](https://digital-wellbeing.github.io/gametime-anger)
  - The output document of our analyses, rendered as a website.

Note: This project uses data and code from our previous study: [Vuorre et al. (2021)](https://psyarxiv.com/8cxyh/). The code thus is mostly taken from that project, with adjustments for the current study.

## Reproducibility

The raw data are in the `Data/` directory of this repository. The code that we used to clean and analyze the data are organized in R Markdown (`.Rmd`) files in this directory, which are meant to be run in the sequence indicated by their numeric prefixes. To run all the cleaning and analyses, and compile the resulting document ([the online analysis supplement](https://digital-wellbeing.github.io/gametime-longitudinal)), run `bookdown::render_book()` in R or click "Build Book" in the RStudio IDE.