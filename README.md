<div align="center">
<!-- Title -->

# General psychopathology and trait aggression shared common psychological characteristics in community youths: A machine learning and network approach

<div align="left">
<!-- Badges -->

![Total lines](https://img.shields.io/tokei/lines/github.com/kamione/yes_youth_aggression?color=green&label=total%20lines)

Last update: 28 Oct 2022

<!-- Description -->

## Description

The code repository for the [paper]().

<!-- Setup -->

## Setup

```{bash}
cd path_to_a_download_folder
git clone https://github.com/kamione/yes_youth_aggression
```

Start [hku_yesaggression.Rproj](https://github.com/kamione/yes_youth_aggression/blob/master/hku_yesaggression.Rproj) if you would like to run within the RStudio environment.

<!-- Usage -->

## Usage

### How to run the scripts in the terminal

```{bash}
cd path_to_a_download_folder/yes_youth_aggression
Rscript scripts/01_preprocessing.R
Rscript scripts/02_data_split.R
Rscript scripts/03_data_visualization.R
Rscript scripts/04_discovery_predict_psychopathology.R
Rscript scripts/05_discovery_predict_aggression.R
Rscript scripts/06_holdout_validation.R
Rscript scripts/07_network_overlappedfeatures_outcomes.R
```

<!-- Citation -->

## Citation

```{bibtex}
@article{wong_2022_aggression,
    author = {Wong, Ting Yat and Fang, Zhiqian and Cheung, Charlton and Suen, Yi Nam and Lai Ming Hui, Christy and Chan, Sherry Kit Wa and Lee, Edwin Ho Ming and Lui, Simon SY and Wong Corine SM and Chang, Wing Chung and Sham, Pak Chung and Chen, Eric Yu Hai},
    title = "{General psychopathology and trait aggression shared common psychological characteristics in community youths: A machine learning and network approach}",
    journal = {},
    year = {2022},
    doi = {},
}
```
