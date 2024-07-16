library(tidyverse)
library(limma)
library(edgeR)
library(dplyr)
library(EnhancedVolcano)
library(readr)
library(ggprism)
#an example of an edit
#load data
Bulk_CD276KO <- read.csv("Documents/LearnR/EdgeR/GeneRenamed(in).csv", header = T, sep = ",")
#create a DGEList
cells <- factor(c("W", "W", "W", "S", "S", "S","KO", "KO", "KO")) #3  3 are Scramble/Sham (infected with nonspecific Cas9), and the final 3 are KO (infected with CD276-specific Cas9)
y <- DGEList(counts = Bulk_CD276KO[, 20:28], genes= Bulk_CD276KO[, 1:1], group = cells)
#filter
keep <- filterByExpr(y)#keeps rows that have worthwhile counts in a minimum number of samples
table(keep)
y <- y[keep, , keep.lib.sizes = FALSE]
#make data frame to store the row names of the filtered data
ydat <- as.data.frame(y)
#Normalize library sizes
y <- normLibSizes(y)

#################For Extracting the Normalized Data#################
#calculate TMM normalization factors:
y <- calcNormFactors(y)
#get the normalized counts:
normalized_counts <- cpm(y, log=FALSE)
#convert matrix to data frame
ncdf<- as.data.frame(normalized_counts)
#take row names of filtered data and tack onto the normalized data
rownames(ncdf) <- ydat[,1]
#export the data
write.csv(ncdf, "Documents/LearnR/GSEA/FND_3.csv")


#explore data
# png(file="Documents/LearnR/EdgeR/MDStest.png",
#     height=2000,
#     width=2000,
#     res=300)

#MDS Plot
colors <- c("red", "blue", "green")
group_colors <- colors[cells]
plotMDS(y, col = group_colors, pch = 16, main = "MDS Plot")
legend("topright", legend = levels(cells), col = colors, pch = 16)


# dev.off()
#design matrix as desired
# Patient <- factor(c("Scram1", "Scram2", "Scram3", "KO1", "KO2", "KO3"))
design <- model.matrix(~0+cells) 
rownames(design) <- colnames(y)

#dispersion estimation
# png(file="Documents/LearnR/EdgeRGraphs/DE.png",
#     height=2000,
#     width=4000,
#     res=300)
y <- estimateDisp(y, design, robust=TRUE)
plotBCV(y)
# dev.off()
#differential expression (quasi-likelihood F-tests)
#endwise negative binomial glm
fit <- glmQLFit(y, design)
#perform the likelihood ratio tests, the contrast compares groups, ex.: 0xA, -1xB, 1xC
qlf <- glmQLFTest(fit, contrast=c(1, -1, 0))
#look at the top genes, sorting by preferred value
topTags(qlf, n=20, adjust.method = "BH", sort.by = "PValue", p.value=1)

#volcano
#convert list to data frame
data_frame_qlf <- as.data.frame(qlf)
#pull out the p values as a numeric vector, since p.adjust cannot handle data frames
pvalue_vector <- data_frame_qlf$PValue
#adjust p values using the Benjamini-Hochberg procedure
fdr <- p.adjust(pvalue_vector,"BH")
#shaping new data frame
voldata <- select(data_frame_qlf, genes, logFC)
#values from the first column of voldata as the row names of voldata
rownames(voldata) <- voldata[,1]
#-1 means to exclude the first column, but use second argument to ensure voldata2 remains a data frame
voldata2 <- voldata[,-1, drop = FALSE]
#add adjusted p-values as a new column in voldata2
voldata2$fdr <- fdr#
# png(file="Documents/LearnR/EdgeRGraphs/VOL.png",
#     height=2000,
#     width=2300,
#     res=300)
EnhancedVolcano(voldata2,
                lab = rownames(voldata2),
                x = 'logFC',
                y = 'fdr',
                title='Scramble vs Knockout',
                #selectLab = c("CD276"),
                # xlim = c(-10, 10),
                pCutoff = 10e-15,
                FCcutoff = 1.5,
                # pointSize = 1.0,
                # labSize = 6.0,
                # colAlpha = 1,
                # cutoffLineType = 'blank',
                # cutoffLineCol = 'black',
                # cutoffLineWidth = 0.8,
                # hline = c(10e-20,
                #           10e-20 * 10e-30,
                #           10e-20 * 10e-60,
                #           10e-20 * 10e-90),
                # hlineCol = c('pink', 'hotpink', 'purple', 'black'),
                # hlineType = c('solid', 'longdash', 'dotdash', 'dotted'),
                # hlineWidth = c(1.0, 1.5, 2.0, 2.5),
                # gridlines.major = FALSE,
                # gridlines.minor = FALSE
                )
# dev.off()


#pathway analysis
#kegga() takes a DGELRT or DGEExact object
keg <- kegga(qlf, species.KEGG = "hsa")
topKEGG(keg, sort = "up")
#quasi-likelihood dispersion
fit <- glmQLFit(y, design, robust=TRUE)
head(fit$coefficients)
plotQLDisp(fit)

A <- qlf[["table"]]
A <- filter(A, PValue<0.05)
A
summary(decideTests(qlf))
# write.csv(A, "Documents/LearnR/EdgeR/A.csv")#save the significant genes

#likelihood ratio tests
# fit <- glmFit(y, design)#genewise negative binomial glm
# lrt <- glmLRT(fit)#perform the likelihood ratio tests
# topTags(lrt, n=20)
#
# B <- lrt[["table"]]
# B <- filter(B, PValue<0.05)
# write.csv(B, "Documents/LearnR/EdgeR/B.csv")
#common genes
A_genes <- row.names(A)#common genes from QLF tests
B_genes <- row.names(B)#common genes from LRT tests
c <- intersect(A_genes, B_genes)#determines common genes found in both tests

#look at cpm for individual samples of top genes
o <- order(lrt$table$PValue)
cpm(y)[o[1:10],]
summary(decideTests(lrt))
#Plot log-fold change against log-counts per million
plotMD(lrt)
abline(h=c(-1, 1), col = "blue")
#gene ontology analysis
# install.packages("GO.db")#‘GO.db’ is not available for this version of R
# go <- goana(lrt)
# topGO(go, ont = "BP", sort = "Up", n=30, truncate=30)
#KEGG analysis - The gene ontology (GO) enrichment analysis and the KEGG pathway
#enrichment analysis are the common downstream procedures to interpret the
#differential expression results in a biological context.
#Given a set of genes that are up- or down-regulated under a certain contrast of
#interest, a GO (or pathway) enrichment analysis will find which GO terms (or
#pathways) are over- or under-represented using annotations for the genes in that set.

