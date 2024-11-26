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

## Generating Makler Count Plots
counts = read.table ('makler_counts.txt')
colnames (counts) = c('segment','non-motile','motile')

counts$segnum = sapply(counts$segment, function(x){as.numeric(str_extract(x, "[0-9]+"))})
ccounts = melt(counts, id.vars=c('segment','segnum'))

model = lm(VAP ~ segnum + extracted, data = detailed)
mysummary = summary(model)
print(mysummary)

model2 = lm(VAP ~ segnum * extracted, data = detailed)
mysummary2 = summary(model2)
print(mysummary2)

AIC(model)
AIC(model2)

extonly = detailed[detailed$extracted == 'e' & detailed$segnum > 1.5,]
model = lm(VAP ~ segnum, data = extonly)
mysummary = summary(model)
print(mysummary)

segonly = detailed[detailed$extracted == 's' & detailed$segnum > 1.5,]
model = lm(VAP ~ segnum, data = segonly)
mysummary = summary(model)
print(mysummary)
anova(model)
print(summary(anova(model)))
