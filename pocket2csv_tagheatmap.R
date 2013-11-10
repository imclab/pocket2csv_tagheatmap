# Parse HTML and convert attributes to data frame
library(XML)

file = "ril_export.html"

doc = htmlParse(file)
nodeset <- getNodeSet(doc, "//a")

# Create data frame
href <- sapply(nodeset, xmlGetAttr, "href")
time_added <- sapply(nodeset, xmlGetAttr, "time_added")
time <- as.POSIXct(as.numeric(time_added), origin="1970-01-01")
tags <- sapply(nodeset, xmlGetAttr, "tags")
tags[tags==""] <- "none"
data <- data.frame(href, time, tags)

# Find unique tags
taglist <- sort(unique(unlist(strsplit(as.character(tags), ","))))

# Create matrix
tagmatrix <- matrix(0, length(tags), length(taglist))

# Create the tag matrix and transform into a data frame (classification)
for (i in 1:length(taglist)) {
  tagmatrix[grep(paste("\\b", taglist[i], "\\b", sep=""), tags), i] <- 1
}
tagframe <- data.frame(tagmatrix)
names(tagframe) <- taglist

# Concatenate to original data frame and output
data <- data.frame(data, tagframe)
write.table(data, "ril_output.csv", sep=";", col.names=TRUE, row.names=FALSE)


# Make heatmap of correlations
tagframe <- tagframe[-which(names(tagframe)=="none")]
c = cor(tagframe)
library("RColorBrewer")
c[which(c==1)] = NA
hmcol <- brewer.pal(11, "RdYlGn")
cb <- (seq(1:12)-6.5)/5.5*0.4
h <- heatmap(c, Rowv=NA, Colv=NA, scale="none", margin=c(9,1), col=hmcol, breaks=cb)
