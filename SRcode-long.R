#!/usr/bin/env Rscript
library(ggplot2)
library(reshape2)
library(stringr)

## Reading Data into Workspace ##
data = read.table ('casa_paths_extracted.txt', sep = ' ') 
colnames (data) = c('name', 'empty' , 'VCL', 'VAP', 'VSL', 'LIN', 'WOB', 'PROG', 'BCF','segment') # Adds titles to columns
data$spermid = 1:nrow(data) # adds sperm ID to the table based on position number.
scale = 2.0
## Generating VCL Plots
mdata = melt(data, id.vars = c("name", "spermid", "segment"))

detailed = data
detailed$extracted = sapply(detailed$segment, function(x){
	if (as.character(x) == "waste") {
		return("s")
	}
	if (as.character(x) == "ewaste") {
		return("e")
	}
	if (as.character(x) == "pure") {
		return("p")
	}
	return(str_extract(x, "[a-zA-Z]+"))
})

detailed$segnum = sapply(detailed$segment, function(x){
	if (as.character(x) == "waste") {
		return(0)
	}
	if (as.character(x) == "ewaste") {
		return(0)
	}
	if (as.character(x) == "pure") {
		return(-1)
	}
	as.numeric(str_extract(x, "[0-9]+"))
})

mdetailed = melt(detailed, id.vars = c("name", "segment", "spermid", "extracted", "segnum"))

mdetailed$prettyextracted = sapply(mdetailed$extracted, function(x) {
  if (x == "e") {
      return("Extracted")
  }
  if (x == "s") {
    return("In Device")
  }
  return(x)
})

g = ggplot(data = mdetailed[mdetailed$variable == "VCL" & mdetailed$extracted != "pure", ], aes(x=segnum, y=value)) + 
  geom_boxplot(aes(fill = prettyextracted, linetype = factor(segnum))) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid"), guide="none") +
  theme_bw() +
  geom_smooth(method = "lm", aes(color = prettyextracted)) +
  theme(plot.title=element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_x_continuous(name = "Segment Number",breaks=seq(2,4,1)) +
  scale_y_continuous(name = "VCL(um/sec)",limits=c(0,70))+
  guides(fill=guide_legend(title="Source"),color=guide_legend(title="Source"))+
  ggtitle("VCL") 
 

pdf("VCL_long.pdf", height = 3 * scale, width = 4 * scale)
print(g)
dev.off()

## Generating VSL Plots

g = ggplot(data = mdetailed[mdetailed$variable == "VSL" & mdetailed$extracted != "pure", ], aes(x=segnum, y=value)) + 
  geom_boxplot(aes(fill = prettyextracted, linetype = factor(segnum))) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid"), guide="none") +
  theme_bw() +
  geom_smooth(method = "lm", aes(color = prettyextracted)) +
  theme(plot.title=element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_x_continuous(name = "Segment Number",breaks=seq(2,4,1)) +
  scale_y_continuous(name = "VSL(um/sec)",limits=c(0,70))+
  guides(fill=guide_legend(title="Source"),color=guide_legend(title="Source"))+
  ggtitle("VSL") 

scale = 2.0
pdf("VSL_long.pdf", height = 3 * scale, width = 4 * scale)
print(g)
dev.off()

## Generating VAP Plots

g = ggplot(data = mdetailed[mdetailed$variable == "VAP" & mdetailed$extracted != "pure", ], aes(x=segnum, y=value)) + 
  geom_boxplot(aes(fill = prettyextracted, linetype = factor(segnum))) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid"), guide="none") +
  theme_bw() +
  geom_smooth(method = "lm", aes(color = prettyextracted)) +
  theme(plot.title=element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_x_continuous(name = "Segment Number",breaks=seq(2,4,1)) +
  scale_y_continuous(name = "VAP(um/sec)",limits=c(0,70))+
  guides(fill=guide_legend(title="Source"),color=guide_legend(title="Source"))+
  ggtitle("VAP") 


pdf("VAP_long.pdf", height = 3 * scale, width = 4 * scale)
print(g)
dev.off()

## Generating LIN plots
g = ggplot(data = mdetailed[mdetailed$variable == "LIN" & mdetailed$extracted != "pure", ], aes(x=segnum, y=value)) + 
  geom_boxplot(aes(fill = prettyextracted, linetype = factor(segnum))) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid"), guide="none") +
  theme_bw() +
  geom_smooth(method = "lm", aes(color = prettyextracted)) +
  theme(plot.title=element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_x_continuous(name = "Segment Number",breaks=seq(2,4,1)) +
  scale_y_continuous(name = "LIN",limits=c(0,1))+
  guides(fill=guide_legend(title="Source"),color=guide_legend(title="Source"))+
  ggtitle("LIN") 


pdf("LIN_long.pdf", height = 3 * scale, width = 4 * scale)
print(g)
dev.off()

## Generating WOB plots
g = ggplot(data = mdetailed[mdetailed$variable == "WOB" & mdetailed$extracted != "pure", ], aes(x=segnum, y=value)) + 
  geom_boxplot(aes(fill = prettyextracted, linetype = factor(segnum))) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid"), guide="none") +
  theme_bw() +
  geom_smooth(method = "lm", aes(color = prettyextracted)) +
  theme(plot.title=element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_x_continuous(name = "Segment Number",breaks=seq(2,4,1)) +
  scale_y_continuous(name = "WOB",limits=c(0,1))+
  guides(fill=guide_legend(title="Source"),color=guide_legend(title="Source"))+
  ggtitle("WOB") 


pdf("WOB_long.pdf", height = 3 * scale, width = 4 * scale)
print(g)
dev.off()

## Generating Makler Count Plots
counts = read.table ('makler_counts.txt')
colnames (counts) = c('segment','non-motile','motile')

counts$segnum = sapply(ccounts$segment, function(x){as.numeric(str_extract(x, "[0-9]+"))})
ccounts = melt(counts, id.vars=c('segment','non-motile','motile','segnum'))

g = ggplot(data=ccounts[ccounts$variable=='non-motile' & ccounts$segment != 'waste'], aes(x=segnum, y=value))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("Non-Motile Sperm Count") +
  theme(plot.title=element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_x_continuous(name = "Segment Number",breaks=seq(2,4,1)) +
  scale_y_continuous(name = "Millions/mL")

pdf("NM_long.pdf", height = 3 * scale, width = 4 * scale)
print(g)
dev.off()


  
  


