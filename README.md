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

* `generateDF()` will generate a wide format data frame where each row represents a single respondent.
* `generateData()` will generate either summary data or raw responses for single or multiple treatment groups.
* `make_longer()` takes the output of `generateData()` and converts it to long format for easy manipulation with other tidyverse packages.

## Datasets

Synthetic data sets created using this package and included in the package will be listed here along with their creation parameters


