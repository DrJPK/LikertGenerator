# LikertGenerator: Creating simulated Likert style data sets

An R Package to create synthetic Likert Style data sets for teaching, testing or other purposes.

## Overview

If you have worked in the social sciences or any research role for any length of time, you will be familiar with Likert type items. Originally introduced by Renis Likert [(1932)](https://psycnet.apa.org/record/1933-01885-001) these items are usually used as part of a multi-item scale to measure respondents' attitudes or beliefs. Likert items usually have choices that are labelled as shown in the table, although as Likert himself acknowledged, the 5-point scale was only adopted for pragmatic reasons and other scale lengths would be perfectly reasonable.

|**SCALE DESCRIPTOR**|**1**|**2**|**3**|**4**|**5**|
|-----|:---:|:---:|:---:|:---:|:---:|
|AGREEMENT|Strongly Disagree|Disagree|Neither Agree nor Disagree|Agree|Strongly Agree|
|APPROVAL |Strongly Disapprove|Disapprove|Undecided|Approve|Strongly Approve|
|TEMPORAL	|Almost Never|Rarely|Sometimes|Often|Almost Always|

Even given their relative ubiquity within research, access to well-behaved Likert data sets, particularly larger sets, is often difficult which raises issues for teaching and learning purposes.

This package is designed to address this dearth of data by generating synthetic Likert Like data in a programmatic way. It is possible using this package to write a simple wrapper that takes, for example, a student's ID number as a seed and to then generate a replicatable data set for analysis.  This is helpful for teachers of statistics, as each student in the class will receive a dataset that is unique to them, thus minimising the possibilities of cheating and academic malpractice, while at the same time being deterministic in the outcomes. Students should draw similar conclusions to each other however the actual numbers reported will be different.

## Installation

Install from GitHub using **devtools**.

```R
library(devtools)
install_github("DrJPK/LikertGenerator")
```

## Usage

`library(LikertGenerator)` will load the basic packages.

* `generateDF()` will generate a wide format data frame where each row represents a single respondent. Generally you will not want to call this function directly.
* `generateData()` will generate either summary data or raw responses for single or multiple treatment groups.
* `make_longer()` takes the output of `generateData()` and converts it to long format for easy manipulation with other tidyverse packages.
* `to_numeric()` takes the output of `generateData()` and converts any columns representing actual data from factor to numeric format
* `label_variables()` takes the output of `generateData()` and labels the factor levels with human readable levels

## Datasets

Synthetic data sets created using this package and included in the package will be listed here along with their creation parameters

### boys_girls_attitudes

This is a simulated data set of 500 student responses without missing values. This represents 250 boys and 250 girls assigned equally to a control and an intervention group. By design, the mean attitude response across the 7 item scale for the boys in the control group should be 3.4 while for the girls this should be 2.9. The effect size for the boys is set so that the mean response of the boys in the intervention group is 3.9 while the mean response for the girls in the intervention group is 3.7.  Thus there should be observable differences for both genders by virtue of being in the intervention group and the effect should be stronger for girls than boys. The data should simulate a "closing the gap" type intervention. See `?boys_girls_attitudes` for full details.


