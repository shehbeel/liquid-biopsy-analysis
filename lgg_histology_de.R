# Title: DE Analysis of Liquid Biopsy Data
# Author: Shehbeel Arif
# Pre-Clinical Laboratory Research Unit
# The Center for Data Driven Discovery in Biomedicine (D3b) 
# Children's Hospital of Philadelphia (CHOP)

# Load the libraries
library(DESeq2)

# Load count matrix
dat <- read.csv("lb_plasma_matrix.csv", header=T, row.names=1)

# Load column data
#coldata <- read.table("lb_plasma_colData.txt", header=T, sep="\t")
coldata <- read.csv("lgg_lb_meta.csv", header=T)

# Create DESeq Data matrix object
h.dds <- DESeqDataSetFromMatrix(dat, coldata, ~Short_Histology)
r.dds <- DESeqDataSetFromMatrix(dat, coldata, ~Relapse)

# Remove lowly expressed genes
h.keep <- rowSums(counts(h.dds)) >= 10
h.dds <- h.dds[h.keep,]
r.keep <- rowSums(counts(r.dds)) >= 10
r.dds <- r.dds[r.keep,]

# Main DESeq
h.ddsDE <- DESeq(h.dds)
r.ddsDE <- DESeq(r.dds)

# Export normalized read counts
h.normCounts <- counts(h.ddsDE, normalized=TRUE)
r.normCounts <- counts(r.ddsDE, normalized=TRUE)
write.csv(h.normCounts, "lb_plasma_relapsed_normalized_counts.csv")
write.csv(r.normCounts, "lb_plasma_histology_normalized_counts.csv")


# DESeq Results
h.res <- results(h.ddsDE, alpha=0.05)
r.res <- results(r.ddsDE, alpha=0.05)

# To see a quick summary of the DE analysis results
summary(h.res)
summary(r.res)

# Export DE Results
h.resOrdered <- h.res[order(h.res$padj),]
r.resOrdered <- r.res[order(r.res$padj),]
write.csv(h.resOrdered, "lb_plasma_histology_DE.csv")
write.csv(r.resOrdered, "lb_plasma_relapse_DE.csv")

# Plot MA plot for DE genes
plotMA(h.ddsDE, ylim=c(-5,5))
plotMA(r.ddsDE, ylim=c(-5,5))









