## script birds and ecosystem services

remotes::install_github("julianflowers/myScrapers")

library(myScrapers);library(tidyverse); library(RISmed); library(europepmc); library(fulltext)


search <- "birds ecosystem service*[tw]"

ncbi_key <- Sys.getenv("ncbi_key")

start <- 1990
end <- 2021

res <- pubmedAbstractR(search = search, start = start, end = end, ncbi_key = ncbi_key, n = 233)

res_sel <- res$abstracts %>%
  .[c(2, 3, 7, 8, 12, 13, 14, 21, 23, 25, 38, 39, 43, 47, 49, 51, 70, 76, 79, 81, 89, 90, 94, 100, 102, 104, 118, 130, 134, 138,
      150, 154, 158, 169, 170, 182, 196, 203, 207, 214, 217, 226, 228),] %>%
  select(pmid, title, abstract)


res_sel %>%
  gt::gt()
