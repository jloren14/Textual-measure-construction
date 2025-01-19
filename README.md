# Textual measure construction to predict the political party based on congress speeches
The repository contains the data and R script with the solution to the problem I was given during the course "Applied Textual Data Analysis" at NHH Norwegian School of Economics. The task was to construct a textual measure that predicts the political party of a politician in the US based on their speech in congress.

The assignment was solved in a group of 3 people (Julia Lorenc, Giuseppe Pio Lando & Claudia dal Prà). 

## Table of contents
* [Project Overview](#project-overview)
* [Data](#data)
* [Analysis](#analysis)
* [Dependencies](#dependencies)

## Project Overview
The goal of the assignment is to estimate the relationship between speech in congress (Senate and House of Representatives) in the US and political party, Republican versus Democrat, with an MNIR (Multinomial Inverse Regression) model.

## Data 
[CongressionalRecords.RData](https://drive.google.com/file/d/1cgreuhP8EktQm6VOXlQDOGq5BUQgtTA9/view?usp=sharing) contains individual speeches by congress members in the years 2021 to 2023. The raw data was scraped from [https://www.congress.gov/congressional-record/archive](https://www.congress.gov/congressional-record/archive).

## Analysis
The procedure for the analysis and algorithm evaluation are the following:

1. Data cleaning: remove punctuation, remove numbers, remove stopwords, make lower case, only consider terms with 3 to 20 letters, and delete excess white spaces
2. Transforming the data into bigrams
3. Making a document term matrix only including tokens (bi-grams) that appear in more than 5 but less than 100 documents
4. Estimating the relationship between bigrams spoken and political party (Republican vs Democrat) with MNIR using the years 2021 and 2022

As a result of the analysis and MNIR model contruction we were able to:
* Identify the bigrams strongest associated with Republicans and Democrats
* Predict the party for political speech in 2023

## Dependencies
The project requires the following R packages:
* parallel
* textir
* Matrix
* tidyverse
* slam
* dplyr
* tm
* tidytext
* stringr
* tokenizers
* stopwords
* proxy
