Disturbance and recovery: microbial community (re)assembly following disturbance across realms
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->
This repository includes R code to reproduce the analyses shown in the article:

**Disturbance and recovery: a synthesis of microbial community assembly following disturbance across realms**

*by Stephanie D. Jurburg, Shane A. Blowes, Ashley Shade, Nico Eisenhauer, Jonathan M. Chase*

Here we give a brief overview on the code and data files in this repository. Note that most analyses were repeated for two different standardisations of sample-effort: one used a single standardisation *across* all studies, the second standardised effort *within* studies. Results were qualitatively similar, and we present the across study standardisation in the main text. There are two versions of most things in the repo, one for each standardisation.

## Data

Files in the data folder contain the processed data following the bioinformatics, effort standardisation, and null modelling (code for these steps available at:<https://github.com/drcarrot/DisturbanceSynthesis>)

**dispersions-across.txt**: disperion data standardised across studies

**dispersions-within.txt**: disperion data standardised within studies

**dispersions.zscores-across.txt**: null model results for disperion data standardised across studies

**dispersions.zscores-within.txt**: null model results for disperion data standardised within studies

**Resilience-across.txt**: turnover data standardised across studies

**Resilience-within.txt**: turnover data standardised within studies

**Resilience.zscores-across.txt**: null model results for turnover data standardised across studies

**Resilience.zscores-within.txt**: null model results for turnover data standardised within studies

**Rich-across.txt**: richness data standardised across studies

**Rich-within.txt**: richness data standardised within studies

## Code

Files in the code folder include:

**01\_**: code to fit models (written to run on scientific computing cluster)

**02\_**: scripts to examine model fit (convergence, posterior predictive checks), examine results and make figures

**03\_**: script to combine model output of two different responses and plot

**04\_**: scripts to wrangle and visually inspect models fit to the different data standardisations

## Results

Files in the model-fits-across folder have the model objects for models fit to data standardised across all studies

Files in the model-fits-within folder have the model objects for models fit to data standardised within each studies
