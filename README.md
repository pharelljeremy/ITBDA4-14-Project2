

# ITBDA4-14 Project 2: Big Data Analytics Techniques

**Student Name:** Jeremy Pharell Kamau  
**Module Code:** ITBDA4-14  
**Module Name:** Big Data Analytics  
**Campus:** Midrand Campus  
**Student Number:** CON-1471229-D0T3

## Project Overview

This repository contains the R scripts, datasets, and documentation for my ITBDA4-14 Project 2 submission. The project is divided into four main questions that demonstrate different big data analytics techniques using R and PostgreSQL.

Question 1 focuses on implementing a keyword search operation using the MapReduce framework. It includes a local demonstration driver and the Hadoop Streaming configuration settings for cluster deployment.

Question 2 builds a data frame from a collection of news files and applies Latent Dirichlet Allocation to identify the top topics. It includes three different visualisations to report the word distributions per topic.

Question 3 provides a proof of concept for a web text mining API. It scrapes a Wikipedia article, cleans the text, and applies two text summarisation techniques. Two visualisations are also generated to show the most frequent terms.

Question 4 demonstrates a complete ETL workflow. It scrapes unstructured data using regular expressions, cleans and imputes missing values, and loads the structured dataset into a PostgreSQL relational database for analytical querying.

## Prerequisites

To run the code in this repository, you will need R and RStudio installed. You will also need PostgreSQL running either locally or through Docker. DataGrip was used as the database client for Question 4.

The following R packages are required. You can install them by running the following line in your R console:

'''r
install.packages(c(
  "tm", "SnowballC", "topicmodels", "tidytext", "ggplot2", "dplyr", 
  "tidyr", "wordcloud", "RColorBrewer", "rvest", "httr", "lexRankr", 
  "textrank", "stringr", "readr", "purrr", "DBI", "RPostgres"
))
'''

## Running the Scripts

Each script in the 'notebooks' folder is self contained and can be run directly in RStudio. 

For Question 1, place the text files in a folder named 'input' in your working directory. Run the entire script to create the local demonstration driver and the Hadoop streaming scripts. You can test the local MapReduce operation by running 'run_local("cloud")' in the console.

For Question 2, ensure the 'sa_news' and 'map_reduce_test_docs' folders are correctly pathed at the top of the script. The script will automatically generate the three visualisation images in your working directory.

For Question 3, an active internet connection is required to scrape the Wikipedia page. The script uses polite scraping with a one second delay between requests. 

For Question 4, you must create the PostgreSQL table using the SQL script provided in the documentation before running the R script. The R script will output the 'cleaned_articles.csv' file which you can then import into your database using DataGrip.

## Documentation

The full written answers for all four questions, including the theoretical explanations, interpretations of the SQL queries, and the embedded visualisations, are available in the documentation' folder.
