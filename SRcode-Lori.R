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
detailed$extracted = sapply(detailed$segment, function(x){return(str_extract(x, "[a-zA-Z]+"))})
detailed$segnum = sapply(detailed$segment, function(x){as.numeric(str_extract(x, "[0-9]+"))})
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

g = ggplot(data = mdetailed[mdetailed$variable == "VCL" & mdetailed$segnum > 1.5 & mdetailed$extracted != "waste" & mdetailed$extracted != "ewaste" & mdetailed$extracted != "pure", ], aes(x=segnum, y=value)) + 
  geom_boxplot(aes(fill = prettyextracted, linetype = factor(segnum))) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid"), guide="none") +
  theme_bw() +
  geom_smooth(method = "lm", aes(color = prettyextracted)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title=element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_x_continuous(name = "Segment number") +
  scale_y_continuous(name = "VCL(um/sec)",limits=c(0,70))+
  guides(fill=guide_legend(title="Source"),color=guide_legend(title="Source"))+
  ggtitle("VCL") 
 

pdf("VCL.pdf", height = 3 * scale, width = 4 * scale)
print(g)
dev.off()

## Generating VSL Plots

g = ggplot(data = mdetailed[mdetailed$variable == "VSL" & mdetailed$segnum > 1.5 & mdetailed$extracted != "waste" & mdetailed$extracted != "ewaste" & mdetailed$extracted != "pure", ], aes(x=segnum, y=value)) + 
  geom_boxplot(aes(fill = prettyextracted, linetype = factor(segnum))) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid"), guide="none") +
  theme_bw() +
  geom_smooth(method = "lm", aes(color = prettyextracted)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title=element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_x_continuous(name = "Segment number") +
  scale_y_continuous(name = "VSL(um/sec)",limits=c(0,70))+
  guides(fill=guide_legend(title="Source"),color=guide_legend(title="Source"))+
  ggtitle("VSL") 

scale = 2.0
pdf("VSL.pdf", height = 3 * scale, width = 4 * scale)
print(g)
dev.off()

## Generating VAP Plots

g = ggplot(data = mdetailed[mdetailed$variable == "VAP" & mdetailed$segnum > 1.5 & mdetailed$extracted != "waste" & mdetailed$extracted != "ewaste" & mdetailed$extracted != "pure", ], aes(x=segnum, y=value)) + 
  geom_boxplot(aes(fill = prettyextracted, linetype = factor(segnum))) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid"), guide="none") +
  theme_bw() +
  geom_smooth(method = "lm", aes(color = prettyextracted)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title=element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_x_continuous(name = "Segment number") +
  scale_y_continuous(name = "VAP(um/sec)",limits=c(0,70))+
  guides(fill=guide_legend(title="Source"),color=guide_legend(title="Source"))+
  ggtitle("VAP") 


pdf("VAP.pdf", height = 3 * scale, width = 4 * scale)
print(g)
dev.off()

## Generating LIN plots
g = ggplot(data = mdetailed[mdetailed$variable == "LIN" & mdetailed$segnum > 1.5 & mdetailed$extracted != "waste" & mdetailed$extracted != "ewaste" & mdetailed$extracted != "pure", ], aes(x=segnum, y=value)) + 
  geom_boxplot(aes(fill = prettyextracted, linetype = factor(segnum))) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid"), guide="none") +
  theme_bw() +
  geom_smooth(method = "lm", aes(color = prettyextracted)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title=element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_x_continuous(name = "Segment number") +
  scale_y_continuous(name = "LIN",limits=c(0,1))+
  guides(fill=guide_legend(title="Source"),color=guide_legend(title="Source"))+
  ggtitle("LIN") 


pdf("LIN.pdf", height = 3 * scale, width = 4 * scale)
print(g)
dev.off()

## Generating WOB plots
g = ggplot(data = mdetailed[mdetailed$variable == "WOB" & mdetailed$segnum > 1.5 & mdetailed$extracted != "waste" & mdetailed$extracted != "ewaste" & mdetailed$extracted != "pure", ], aes(x=segnum, y=value)) + 
  geom_boxplot(aes(fill = prettyextracted, linetype = factor(segnum))) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid"), guide="none") +
  theme_bw() +
  geom_smooth(method = "lm", aes(color = prettyextracted)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title=element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_x_continuous(name = "Segment number") +
  scale_y_continuous(name = "WOB",limits=c(0,1))+
  guides(fill=guide_legend(title="Source"),color=guide_legend(title="Source"))+
  ggtitle("WOB") 


pdf("WOB.pdf", height = 3 * scale, width = 4 * scale)
print(g)
dev.off()
  
  


