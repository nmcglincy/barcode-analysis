library(data.table)
library(parallel)
# 
files = list.files(pattern = "*liteT.txt")
data = list()
# num.lines = 10000
# 
# GET APPROACH RIGHT ON 10K LINES, THEN PERFORM WHOLE THING
for (i in files) {
  data[[i]] = assign(i,
                     fread(input = i,
                          data.table = FALSE,
                          sep = "\t",
                          # nrows = num.lines,
                          header = FALSE,
                          stringsAsFactors = FALSE,
                          verbose = FALSE,
                          showProgress = FALSE))
  names(data[[i]]) = c("read.name", "barcode7", "blah", "sample.barcode", "read")
}
names(data) = c("112A", "112B", "D19A", "D19B")
# 
# SANITY CHECK
# str(data)
# LOOKS GOOD
# 
# REMOVE DUPLICATED READS FOR EACH BARCODE7
library(plyr)
library(dplyr)
library(stringr)
# 
# FILTER FUNCTION - DISCARD UNWANTED COLUMNS
# 20150113 - DON'T THINK I NEED THE ARRANGE COMMAND
uniqueReadsPerBarcode = function(x) {
  require(dplyr)
  x %>%
    select(read.name, barcode7, read) %>%
    group_by(barcode7) %>%
    distinct(read)
}
# 
data.unq = mclapply(data, uniqueReadsPerBarcode, mc.cores = 36)
# str(data.unq)
# 
# CHECKING THE EFFECT OF THE FILTER
filter.summ = inner_join(ldply(mclapply(data, nrow, mc.cores = 36)),
                         ldply(mclapply(data.unq, nrow, mc.cores = 36)),
                         by = ".id")
names(filter.summ) = c("library", "no.reads_before.filter", "no.reads_after.filter")
filter.summ = filter.summ %>%
  mutate(prop.remaining = no.reads_after.filter/no.reads_before.filter)
write.csv(filter.summ, 
          file = "filter-summary.csv",
          quote = FALSE,
          row.names = FALSE)
# 
# EDA OF EFFECT OF FILTERING
# SUMMARY FUNCTION
readsPerBarcodeSumm = function(x) {
  require(dplyr)
  x %>%
    group_by(barcode7) %>%
    summarise(lnth = length(read))
}
# 
pre.filter  = ldply(mclapply(data, readsPerBarcodeSumm, mc.cores = 36))
post.filter = ldply(mclapply(data.unq, readsPerBarcodeSumm, mc.cores = 36))
pre.filter = pre.filter %>%
  mutate(filter = "before")
post.filter = post.filter %>%
  mutate(filter = "after")
filt.an.df = ldply(list(pre.filter,post.filter))
filt.an.df$filter = factor(filt.an.df$filter, levels = c("before", "after"))
# 
library(ggplot2)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(filt.an.df, aes(x = lnth, colour = filter)) +
  stat_ecdf() +
  facet_wrap(~ .id, ncol = 2) +
  ylab("ECDF") +
  xlab("Number for reads for each 7 digit barcode") +
  scale_colour_manual(name = "uniqueReadsPerBarcode()",
                      values = cbbPalette[c(1,7)]) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +    
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(vjust = 0, size = 16),
        axis.title.y = element_text(vjust = 1, size = 16),
        axis.text.x = element_text(size=14, vjust = 0.5),
        axis.text.y  = element_text(size=14),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14))
ggsave("effect-uniqueReadsPerBarcode.png", dpi = 400)
graphics.off()
#
# lapply(data.unq, head)
# 
# I WANT TO SPLIT BARCODE 7 INTO TWO COLUMNS
# barcode2 - THE FIRST TWO NUCLEOTIDE
# barcode5 - THE REMAINING FIVE NUCLEOTIDES
# 
# I WANT TO EXTRACT TWO SUB-PORTIONS OF THE READ
# barcode2.flank - THE FIRST SEVEN NTS OF THE READ
# barcode5.flank - THE LAST SEVEN NTS OF THE READ
# 
# SUBSEQUENCE SELECTION FUNCTION
selectSubSequence = function(x) {
  require(dplyr)
  x %>%
    mutate(up.flank = str_c(str_sub(barcode7, start = 1, end = 2), str_sub(read, start = 1, end = 5)),
           down.flank = str_c(str_sub(read, start = -5, end = -1), str_sub(barcode7, start = -5, end = -1))) %>%
    select(read.name, up.flank, down.flank)
}
# 
data.unq.split = mclapply(data.unq, selectSubSequence, mc.cores = 36)
# lapply(data.unq.split, head)
# 
# LOOKS LIKE A GOOD FORMAT
# THE PLAN IS TO MAKE A SEQUENCE LOGO OF SOME SORT FOR THE .flank VARIBLES, WHICH STILL SHOW THE 
# SEQUENCE PREFERENCE AT THE JOINS, BUT THERE'S NO WAY TO GET AROUND THAT THIS IS A PRODUCT OF THE 
# FREQUENCY OF THE INSERT SEQUENCE AND THE LIKLIHOOD OF LIGATION FOR EACH SEQUENCE. I'LL MAKE THE 
# LOGO AND THEN SPEAK TO NICK...
#
# TWO THINGS I WANT TO ACHEIVE:
# 2. MOST AND LEAST POPULAR SEQUENCES FOR EACH LIBRARY
# UP FLANK
upFlankWinners = function(x) {
  require(dplyr)
  x %>%
    group_by(up.flank) %>%
    summarise(up.flank.freq = length(read.name)) %>%
	  arrange(desc(up.flank.freq)) %>%
	  slice(1:100000)
}
# 
up.freq.summ = mclapply(data.unq.split, upFlankWinners, mc.cores = 36)
# str(up.freq.summ)
# lapply(up.freq.summ, head)
# 
csvWritter = function(i, y, nom) {
  # WRITES THE FIRST 100 LINES, WHICH CORRESPONDS TO THE TOP 100 OF THE SORTED DATAFRAME
  write.csv(y[[i]][1:100,],
            file = paste(i, nom, "top100.csv", sep = "_"),
            quote = FALSE,
            row.names = FALSE)
}
mclapply(names(lapply(up.freq.summ, names)), csvWritter, y = up.freq.summ, nom = "upflank", mc.cores = 36)
# 
# DOWN FLANK
downFlankWinners = function(x) {
  require(dplyr)
  x %>%
    group_by(down.flank) %>%
    summarise(down.flank.freq = length(read.name)) %>%
    arrange(desc(down.flank.freq)) %>%
    slice(1:100000)
}
# 
down.freq.summ = mclapply(data.unq.split, downFlankWinners, mc.cores = 36)
mclapply(names(lapply(down.freq.summ, names)), csvWritter, y = down.freq.summ, nom = "downflank", mc.cores = 36)
# 
# 1. SEQUENCE LOGO FOR INFORMATION AND PROBABITY FOR EACH LIBRARY
# 
# RWebLogo is the most straightforward way to acheive this, though you don't get as much background
# info
# 20150114 - this seems to be the part where it's sticking, I've move the reporting functions to prior to this; I expect that they will be
# quicker, so I'll actually get those reports back. I also had the same intuition to split up the graphing function, so that I can see things
# come back piece-wise.
# Also, I wonder if require() is resulting in some sort of test each time, which would also slow things down.
# 20150115 - this didn't work any better, after discussing with Nick, do the weblogo on the top 10K or 100K
library(RWebLogo)
# Graphing function
weblogoGraph.upinfo = function(i) {
  # require(RWebLogo)
  weblogo(seqs = up.freq.summ[[i]]$up.flank,
          file.out = paste(i, "_up_info100k.pdf", sep = ""),
          errorbars = FALSE,
          open = FALSE,
          verbose = FALSE,
          sequence.type = "dna",
          color.scheme = "classic",
          annotate = c(paste(rep("B", 2), 1:2, sep = ""), paste(rep("R", 5), 1:5, sep = "")))
}
weblogoGraph.upprob = function(i) {
	weblogo(seqs = up.freq.summ[[i]]$up.flank,
          file.out = paste(i, "_up_prob100k.pdf", sep = ""),
          units = "probability",
          errorbars = FALSE,
          open = FALSE,
          verbose = FALSE,
          sequence.type = "dna",
          color.scheme = "classic",
          annotate = c(paste(rep("B", 2), 1:2, sep = ""), paste(rep("R", 5), 1:5, sep = "")))
}
weblogoGraph.downinfo = function(i) {
	weblogo(seqs = down.freq.summ[[i]]$down.flank,
          file.out = paste(i, "_down_info100k.pdf", sep = ""),
          errorbars = FALSE,
          open = FALSE,
          verbose = FALSE,
          sequence.type = "dna",
          color.scheme = "classic",
          annotate = c(paste(rep("R", 5), 1:5, sep = ""), paste(rep("B", 5), 1:5, sep = "")))
}
weblogoGraph.downprob = function(i)  {
	weblogo(seqs = down.freq.summ[[i]]$down.flank,
          file.out = paste(i, "_down_prob100k.pdf", sep = ""),
          units = "probability",
          errorbars = FALSE,
          open = FALSE,
          verbose = FALSE,
          sequence.type = "dna",
          color.scheme = "classic",
          annotate = c(paste(rep("R", 5), 1:5, sep = ""), paste(rep("B", 5), 1:5, sep = "")))
}
# 
# FOR EACH LIBRARY AND INFORMATION AND PROBABILITY BASED GRAPH OF BOTH THE UP AND DOWN FLANKS
mclapply(names(lapply(up.freq.summ, names)), weblogoGraph.upinfo, mc.cores = 36)
mclapply(names(lapply(up.freq.summ, names)), weblogoGraph.upprob, mc.cores = 36)
mclapply(names(lapply(down.freq.summ, names)), weblogoGraph.downinfo, mc.cores = 36)
mclapply(names(lapply(down.freq.summ, names)), weblogoGraph.downprob, mc.cores = 36)
# 
