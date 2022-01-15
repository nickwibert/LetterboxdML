# LetterboxdML
My individual final project for STA4241 - Statistical Learning in R. In this project, I used my personal movie-watching data from [Letterboxd.com](https://letterboxd.com/nickwibert) to train machine learning models--specifically, classification algorithms including ordered logistic regression, K-nearest neighbors, support vector machines, etc.--and evaluate their predictive performance on my movie ratings.

## Dataset
Letterboxd allows you to export your personal data directly from the website, but the information is limited to the movies you watched, the day you watched them, and what rating you assigned them out of five stars. To obtain more relevant covariates for model training, I wrote a web-scraping script using the `rvest` package to pull info such as Average Rating, Runtime, and Genre from the website. This script is named `lbxd_scraper.R` and can be found in the main directory.

## Analysis
All of my variable selection and model training procedures can be found in the `lbxd_analysis.R` script, which is broken up into various simulation studies based on the data and/or covariates being used. These simulation studies are loosely organized, but the results of my simulations are all gathered and reported in the final report.

## Report
The final report that I submitted is titled `sta4241_final.pdf`, and contains a full, detailed account of my data exploration and analysis as well as visualizations and a discussion of the results. I also made a PowerPoint presentation with the same findings for use in my oral presentation of the project, found under `Letterboxd Data Analysis.pptx` (there is also a PDF version).


