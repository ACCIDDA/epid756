
#' @title Tour of Rstudio
#'
#' @section First Steps
#'
#' example R code
x <- rnorm(n = 100, mean = 10, sd = 5)
summary(x)
hist(x)

#' @question what if we run rnorm() without assigning to object x?
#'
#' @question what do you expect if we increase n?
bigger_x <- rnorm(n = 10000, mean = 10, sd = 5)
summary(bigger_x)
hist(bigger_x)

#' getting help
?rnorm
?summary
?hist

#' @section Practice with simulated data

#' Confirm you are working in the same directory as the rest of the code:
getwd()
?setwd

#' [base::file.path()] is for making platform independent paths
df <- readRDS(file.path("data", "simdata.rds"))

#' Now, review several functions for inspecting data:
#' `?View`, `?names`, `?head`, `?tail`, `?nrow`, `?ncol`
#' after looking at the documentation, guess and check what the below will do

View(df)
names(df)
head(df)
head(df, n = 10)
#' @question how different from the version without `n = `?
tail(df)
tail(df, n = 20)
#' @question how different from the version without `n = `?
nrow(df)
ncol(df)

#' get summary statistics for age variable
summary(df$age)

#' @section Using packages
#'
#' A useful library to summarize multiple variables in a table
install.packages("tableone") # only run once
library(tableone) # run every time you want to use it
?CreateTableOne

CreateTableOne(data = df,
               vars = c("age", "sex", "health_seek", "comorbid", "dx", "died"),
               strata = "vax",
               factorVars = c("health_seek", "comorbid", "dx", "died"),
               test = FALSE)

#' for next class
#' only run if you've never installed before - just need to install once
install.packages("tidyverse")
#' run every time you start a new R session to load the packages
library(tidyverse)
