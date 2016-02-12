HGNCpages <- read.csv("~/Documents/Web Analytics/Data/HGNCpages010116to310116.csv", quote="", stringsAsFactors=FALSE)
source("~/Documents/Web Analytics/RforWeb/DrupFixAnalysis.R")
#commented out the Date fix in the clean data function above.
HGNCpages <- cleanData(HGNCpages)
found <- simplify2array(regexec("gene_symbol_report", HGNCpages[,1]))
gene_symbols <- which(found > -1)
avg_gen <- HGNCpages[gene_symbols, "Avg..generation.time"]

# this is what I reported as one of my metrics
summary(avg_gen)

# but that doesn't take into account the fact that some averages are smother due to more visits so...
# attempt to come up with a weighted average.  
# total generation time for that page *should* be gotten by multiplying the number of page views
gen_time <- HGNCpages[gene_symbols, "Avg..generation.time"]*HGNCpages[gene_symbols, "Pageviews"]
weighted.avg <- sum(gen_time)/sum(HGNCpages[gene_symbols, "Pageviews"])

# not sure how to get the weighted version of the rest of it 

# visualising it:
plot(avg_gen, xlab = "Pages sorted from most to least viewed", ylab = "Average Generation time (sec)", main = "Gene Symbol Reports")
plot(x= HGNCpages[gene_symbols, "Pageviews"], y=avg_gen, xlab = "Number page views", ylab = "Average Generation time (sec)", main = "Gene Symbol Reports")


###############GA############
Analytics.HGNC.Pages.20160112.20160211 <- read.csv("~/Downloads/Analytics HGNC Pages 20160112-20160211.csv", comment.char="#",stringsAsFactors = FALSE)
Analytics.HGNC.Pages.20160112.20160211 <- Analytics.HGNC.Pages.20160112.20160211[1:5000,]
Analytics.HGNC.Pages.20160112.20160211[,2] <- as.numeric(sub(",","",Analytics.HGNC.Pages.20160112.20160211[,2]))
found <- simplify2array(regexec("gene_symbol_report", Analytics.HGNC.Pages.20160112.20160211[,1]))
gene_symbols <- which(found > -1)
sum(Analytics.HGNC.Pages.20160112.20160211[gene_symbols,2])/287940

found <- simplify2array(regexec("genefamilies/set", Analytics.HGNC.Pages.20160112.20160211[,1]))
gene_families <- which(found > -1)
sum(Analytics.HGNC.Pages.20160112.20160211[gene_families,2])/287940


siteSpeed <- read.csv("~/Documents/Web Analytics/Data/Analytics HGNC Site Speed Page Timings 20160101-20160131.csv", comment.char="#", stringsAsFactors=FALSE)
siteSpeed <- siteSpeed[1:1000,]
siteSpeed [,2]<- as.numeric(siteSpeed[,2])
siteSpeed [,3]<- as.numeric(sub(",","",siteSpeed[,3]))
found <- simplify2array(regexec("gene_symbol_report", siteSpeed[,1]))
gene_symbols <- which(found > -1)
summary(siteSpeed[gene_symbols,2])

gen_time <- siteSpeed[gene_symbols,2]*siteSpeed[gene_symbols,3]
sum(gen_time)/sum(siteSpeed[gene_symbols,3])
