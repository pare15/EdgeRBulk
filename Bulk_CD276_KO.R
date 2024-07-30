library(tidyverse)
library(limma)
library(edgeR)
library(dplyr)
library(EnhancedVolcano)
library(readr)
library(ggprism)

#load data
Bulk_CD276KO <- read.csv("Documents/LearnR/EdgeR/GeneRenamed(in).csv", header = T, sep = ",")
#create a DGEList
cells <- factor(c("W", "W", "W", "S", "S", "S","KO", "KO", "KO")) #3  3 are Scramble/Sham (infected with nonspecific Cas9), and the final 3 are KO (infected with CD276-specific Cas9)
y <- DGEList(counts = Bulk_CD276KO[, 20:28], genes= Bulk_CD276KO[, 1:1], group = cells)
#filter
keep <- filterByExpr(y)#keeps rows that have worthwhile counts in a minimum number of samples
table(keep)
y <- y[keep, , keep.lib.sizes = FALSE]
#Normalize library sizes
y <- normLibSizes(y)
#make data frame to store the row names of the filtered data
ydat <- as.data.frame(y)

#design matrix as desired
design <- model.matrix(~0+cells)
rownames(design) <- colnames(y)


#differential expression (quasi-likelihood F-tests) using endwise negative binomial glm
fit <- glmQLFit(y, design)
#perform the likelihood ratio tests, the contrast compares groups, ex.: 0xA, -1xB, 1xC
qlf <- glmQLFTest(fit, contrast=c(1, -1, 0))
#look at the top genes, sorting by preferred value
topTags(qlf, n=20, adjust.method = "BH", sort.by = "PValue", p.value=1)
