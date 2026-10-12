![Build Status](https://github.com/smartdata-analysis-and-statistics/precision-medicine-practicals/actions/workflows/render-quarto.yml/badge.svg)


# An Introduction to Precision Medicine

This practical is part of our comprehensive course An Introduction to Precision Medicine, which is available at:

👉 [https://sdas.thinkific.com/courses/precmedintro](https://sdas.thinkific.com/courses/precmedintro)

The course introduces the core concepts and methods of precision medicine, with a focus on developing and validating prognostic and predictive models using data from both clinical trials and non-randomized sources. Through interactive tutorials and hands-on practicals in R, participants learn how to explore data, construct risk prediction models, evaluate model performance, and apply these models in clinical decision-making.

This repository includes selected materials and practical exercises designed to support learning and implementation using R and Quarto.

## Practical website

The materials can be rendered as a Quarto website. The landing page links the learner-facing practicals and their solution documents:

- `part1.1_precision_medicine_task.qmd` — data preparation and exploration
- `part1.2_precision_medicine_task.qmd` — model development
- `part1.3_precision_medicine_task.qmd` — model validation

The corresponding files without `_task` contain worked solutions. Run `quarto preview` for local development or `quarto render` to build the complete site.

To install the R packages used across the practicals, run `source("setup.R")` from the project root. Package dependencies are declared in `DESCRIPTION`; use `renv` if you need a fully locked computational environment.

## License

All materials in this repository — including Quarto files, R code, and figures — are licensed under the 
[Creative Commons Attribution-NonCommercial 4.0 International License (CC BY-NC 4.0)](https://creativecommons.org/licenses/by-nc/4.0/).

You are welcome to use, adapt, and share these materials for non-commercial purposes, provided you give appropriate credit to **Smart Data Analysis and Statistics**.

⚠️ **Commercial use** — including distribution, modification, or integration into commercial products or services — is **not allowed without prior written permission**. To request permission, please contact us directly. 

We take copyright and license compliance seriously and are **prepared to pursue legal action** against individuals or organizations that use these materials in violation of the license terms.

## How to cite us

If you use or adapt this material, please cite both the course and the practical materials:

> Smart Data Analysis and Statistics (SDAS). (2025). *An Introduction to Precision Medicine* [Online course]. Available at: https://sdas.thinkific.com/courses/precmedintro
> 
> Smart Data Analysis and Statistics (SDAS). (2025). *Computer Practical Materials for "An Introduction to Precision Medicine"*. Available at: https://github.com/smartdata-analysis-and-statistics/precision-medicine-practicals
