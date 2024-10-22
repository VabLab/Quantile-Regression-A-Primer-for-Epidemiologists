# Quantile Regression A Primer for Epidemiologists
#### By Aayush Khadka, Jillian L. Hebert, M. Maria Glymour, Fei Jiang, Amanda Irish, Kate A. Duchowny, Anusha M. Vable
Code for the "Quantile regressions as a tool to evaluate how an exposure shifts and reshapes the outcome distribution: A primer for epidemiologists" publication. ([Link to paper](https://academic.oup.com/aje/advance-article-abstract/doi/10.1093/aje/kwae246/7726840))


# Abstract

Quantifying how an exposure affects the entire outcome distribution is often important, e.g., for outcomes such as blood pressure which have non-linear effects on long-term morbidity and mortality. Quantile regressions offer a powerful way of estimating an exposure’s relationship with the outcome distribution but remain underused in epidemiology. We introduce quantile regressions with a focus on distinguishing estimators for quantiles of the conditional and unconditional outcome distributions. We also present an empirical example in which we fit mean and quantile regressions to investigate educational attainment’s association with later-life systolic blood pressure (SBP). We use data on 8,875 US-born respondents aged 50+ years from the US Health and Retirement Study. More education was negatively associated with mean SBP. Conditional and unconditional quantile regressions both suggested a negative association between education and SBP at all levels of SBP, but the absolute magnitudes of these associations were higher at higher SBP quantiles relative to lower quantiles. In addition to showing that educational attainment shifted the SBP distribution left-wards, quantile regression results revealed that education may have reshaped the SBP distribution through larger protective associations in the right tail, thus benefiting those at highest risk of cardiovascular diseases.


# Repository Content

- `README.md`: This file with an explanation of this repository.
- `1. Pull SBP`: R code to pull the first SBP measure for each participant.
- `2. Clean Covariates`: R code to clean model covaraites and merge to create final dataset for analysis.
- `3. Analysis.R`: R code used for the paper's empirical analysis.
- `4. Graphics.R`: R code for creating graphics used throughout the paper.
- `R Session Info.png`: R version and package information.

Note that no datasets will be made available on this repository due to data usage restrictions. Data can be accessed through the Health and Retirement Study (HRS) public survey files. 


# Contact Information

Aayush Khadka aayush.khadka@ucsf.edu  
Department of Family and Community Medicine, University of California, San Francisco

Jilly Hebert jilly.hebert@ucsf.edu  
Department of Family and Community Medicine, University of California, San Francisco


# Affiliations

Aayush Khadka, Jillian L. Hebert, and Anusha M. Vable: Department of Family and Community Medicine, University of California San Francisco. 

Anush M. Vable: Philip R Lee Institute for Health Policy Studies, University of California San Francisco. 

M. Maria Glymour: Department of Epidemiology, Boston University. 

Fei Jiang and Amanda Irish: Department of Epidemiology and Biostatistics, University of California San Francisco. 

Kate A. Duchowny: Institute for Social Research, University of Michigan Ann Arbor.


# References

Health and Retirement Study, (Tracker and RAND) public use dataset. Produced and distributed by the University of Michigan with funding from the National Institute on Aging (grant number NIA U01AG009740). Ann Arbor, MI, (2023). URL https://hrs.isr.umich.edu/data-products

R Core Team (2022). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/

