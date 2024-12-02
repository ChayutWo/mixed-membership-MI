# Hierarchical Dirichlet Process Mixture of Products of Multinomial Distributions: Applications to Survey Data with Potentially Missing Values

## Abstract
In social science research, understanding latent structures within populations through survey data with categorical responses is essential. Traditional methods like Factor Analysis and Latent Class Analysis have limitations, particularly in handling categorical data and accommodating mixed memberships in latent classes. Moreover, analyzing survey responses with missing values using these methods is challenging. This study introduces a Hierarchical Dirichlet Process Mixture of Products of Multinomial Distributions (HDPMPM) model, which leverages the flexibility of nonparametric Bayesian methods to address these limitations. The HDPMPM model allows for multiple latent classes within individuals, a potentially infinite number of mixture components, and incorporates missing data imputation directly into the model's Gibbs sampling process. By applying a truncated stick-breaking representation of the Dirichlet process, we derive a Gibbs sampling scheme for posterior inference. An application of the HDPMPM model to the 2016 American National Election Study (ANES) data demonstrates its effectiveness in identifying political profiles and handling missing data scenarios, including those that are Missing at Random (MAR) and Missing Completely at Random (MCAR). The results show that the HDPMPM model successfully recovers dominant profiles and manages complex latent structures in survey data, providing a robust tool for social science researchers dealing with categorical data and missing values.

**Keywords**: Mixed-membership Models; Bayesian Inference, Hierarchical Dirichlet Process; Missing data; Nonresponse

## Objectives

This study aims to overcome limitations of traditional methods like Factor Analysis and Latent Class Analysis in analyzing survey data with categorical responses. It introduces the Hierarchical Dirichlet Process Mixture of Products of Multinomial Distributions (HDPMPM) model, which addresses challenges such as mixed memberships, infinite mixture components, and missing data. By leveraging nonparametric Bayesian methods and integrating missing data imputation into its Gibbs sampling process, the HDPMPM model provides a robust tool for uncovering latent structures. Applied to the 2016 American National Election Study (ANES), the model effectively identifies political profiles and manages complex data scenarios.

## Dataset

We illustrate an application of the HDPMPM modeling using data collected in 2016 from the American National Election Study (ANES). This survey data is part of the series of election studies conducted since 1948 to provide data for the analysis of public opinion and voting behavior in the U.S. presidential elections. The 2016 survey included responses from pre-election interviews and post-election interviews of the same respondents. The total pre-election sample size was 4,271, consisting of face-to-face interviews (n=1,181) and internet interviews via questionnaires (n=3,090). The two groups of samples were drawn independently from U.S. citizens aged 18 or older using address-based sampling. The data include over 1,800 variables covering different aspects, including voting behavior, candidate and party evaluations, government evaluations, political issues, etc.  The data can be downloaded from [the Inter-university Consortium for Political and Social Research (ICPSR)](https://www.icpsr.umich.edu/web/ICPSR/studies/36824/summary).

## Software and analytical approaches

[The software and analytical approaches in this work center around custom R code developed specifically for implementing Gibbs sampling and posterior inference for the proposed model. This bespoke code efficiently handles the complexities of the Hierarchical Dirichlet Process Mixture of Products of Multinomial Distributions (HDPMPM) model, including the truncated stick-breaking representation and integrated missing data imputation. The R code is provided here in this repository.]:#

## Key file description
On-going

## Publications
On-going

**Author**

Chayut Wongkamthong - Senior Data Scientist at [KASIKORN Business-Technology Group](https://www.kbtg.tech/)

Olanrewaju M. Akande, Ph.D. - Research Scientist at [Meta](https://about.meta.com/)
