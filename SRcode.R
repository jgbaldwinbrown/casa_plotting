library(ggplot2)
library(reshape2)
library(stringr)

data = read.table ('casa_paths_extracted.txt', sep = ' ')
colnames (data) = c('name', 'empty' , 'VCL', 'VAP', 'VSL', 'LIN', 'WOB', 'PROG', 'BCF','segment')
data$spermid = 1:nrow(data)

mdata = melt(data, id.vars = c("name", "spermid"))

mdata2 = melt(data, id.vars = c("name", "spermid", "LIN"))
g = ggplot(data = mdata2[mdata2$variable == "VCL", ], aes(name, value)) + geom_boxplot()

pdf("output.pdf", height = 3, width = 4)
print(g)
dev.off()

data$run = sapply(data$name, function(x) {strsplit(x, split ="/")[[1]][2]})

mdata = melt(data, id.vars = c("name", "spermid", "run", "segment"))

g = ggplot(data = mdata[mdata$variable == "VCL", ], aes(run, value, fill = segment)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

pdf("output2.pdf", height = 6, width = 8)
print(g)
dev.off()

g = ggplot(data = mdata[mdata$variable == "VCL", ], aes(segment, value, fill = run)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

pdf("output3.pdf", height = 6, width = 8)
print(g)
dev.off()

g = ggplot(data = mdata[mdata$variable == "VSL", ], aes(segment, value, fill = run)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  lims(y=c(0,70))

pdf("output4.pdf", height = 6, width = 8)
print(g)
dev.off()

detailed = data
detailed$extracted = sapply(detailed$segment, function(x){return(str_extract(x, "[a-zA-Z]+"))})
detailed$segnum = sapply(detailed$segment, function(x){as.numeric(str_extract(x, "[0-9]+"))})
mdetailed = melt(detailed, id.vars = c("name", "segment", "spermid", "extracted", "segnum"))
g = ggplot(data = mdetailed[mdetailed$variable == "VCL", ], aes(segnum, value, color = extracted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

pdf("scatter1.pdf", height = 6, width = 8)
print(g)
dev.off()

detailed = data
detailed$extracted = sapply(detailed$segment, function(x){return(str_extract(x, "[a-zA-Z]+"))})
detailed$segnum = sapply(detailed$segment, function(x){as.numeric(str_extract(x, "[0-9]+"))})
mdetailed = melt(detailed, id.vars = c("name", "segment", "spermid", "extracted", "segnum"))
g = ggplot(data = mdetailed[mdetailed$variable == "VCL" & mdetailed$segnum > 1.5, ], aes(segnum, value, color = extracted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

pdf("scatter2.pdf", height = 6, width = 8)
print(g)
dev.off()

model = aov(VSL ~ segment, data = data[data$segment %in% c("1", "2"),])
summary(model)
TukeyHSD(model, conf.level = 0.95)
mean(data$VSL[data$segment == "1"])
mean(data$VSL[data$segment == "2"])

# install.packages("ggplot2")

