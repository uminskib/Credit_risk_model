# Credit risk model

## Introduction

The repository contains a credit risk modelling project based on a database that comes from the [Kaggle](https://www.kaggle.com/gauravduttakiit/loan-defaulter?select=columns_description.csv). Application data in this database contains 307511 individual customers who have had trouble repaying the loan or not. Each client is described by 121 variables. The main goal of the study is to create a best scoring card that will predict whether a customer is in default or not. In the repository are:

* data folder - there are main file with the credit application data and data files that was created during the analysis.
* report folder - it contains reports about fine-classing and coarse-classing that was created during the analysis.
* Model_Ryzyka_Kredytoweg.R - main file with whole code.
* Raport_Model_Ryzyka_Kredytowego.pdf - file containing a description of the analysis results.

The whole model was realized in February 2021 for the purpose of passing classes and was prepared in Polish language. This is a group project in which I was responsible for preparing the data for modeling: data cleaning, variable selection, fine and coarse classification process. All these steps are also described by me in the report. The analysis was conducted in R where well-developed fine-classing and coarse-classing packages are available.

## Technologies

* R 4.03
* Rstudio 1.3.1093
* zoo
* tidyr
* ggplot
* smbinning - coarse-classing
* woeBinning - fine-classing

Rest of required packages at the beginning of the script.

## Setup

To run the project:

1. Download the entire repository and unzip it or clone repo by git.
2. Install the required packages included at the begining of the script.
3. Run the rest of the script line by line to review the analysis

Script developed and tested in Rstudio 4.03.
