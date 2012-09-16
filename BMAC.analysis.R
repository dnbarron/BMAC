setwd("c:/documents and settings/dbarron/my documents/reputation/BMAC")
library(plyr)
library(ggplot2)
library(reshape)

bmac2011 <- read.csv("bmac2011.csv")
meantotal2011 <- tapply(bmac2011$Total,bmac2011$Industry,mean)
industry2011 <- names(meantotal2011)


bmac2010 <- read.csv("bmac2010.csv")
meantotal2010 <- tapply(bmac2010$Total,bmac2010$Industry,mean)
industry2010 <- names(meantotal2010)
bmac2009 <- read.csv("bmac2009.csv")
meantotal2009 <- tapply(bmac2009$TOTAL,bmac2009$Industry,mean)
industry2009 <- names(meantotal2009)
cbind(industry2009,industry2010)
meandata <- merge(data.frame(Industry=industry2010,Mean2010=meantotal2010),
  data.frame(Industry=industry2009,Mean2009=meantotal2009), by="Industry",all=TRUE)
meandata2 <- merge(data.frame(Industry=industry2011,Mean2011=meantotal2011), meandata,by="Industry", all=TRUE)
melt.md <- melt(meandata2,id.vars="Industry")
Year <- c(rep(2011,29),rep(2010,29),rep(2009,29))
melt.md$Year <- factor(Year)
names(melt.md) <- c("Industry","variable","Total","Year")
ix <- order(melt.md$Year,melt.md$Total)
melt.md <- melt.md[ix,]

g <- ggplot(data=melt.md,aes(x=Industry,y=Total,colour=Year)) + geom_point(size=4)
g + coord_flip()

library(quantmod)
st <- "2010-1-1"
# 3i
getSymbols("III",src="google",from=st)
chartSeries(III, TA=NULL)
# Abbey
getSymbols("ABBY",src="google",from=st)
# Aberdeen Asset Management
getSymbols("ABD",src="google",from=st)
getSymbols("ADM",src="google",from=st)

ind <- levels(bmac2010$Industry)
banks <- subset(bmac2010,Industry==ind[2])
ix <- order(bmac2010$Industry)
bmac2010 <- bmac2010[ix,]
op <- par(mfrow=c(2,2))
pdf("BMAC2010.pdf")
for (i in 1:230) barplot(as.matrix(bmac2010[i,c(6,8,10,12,14,16,20,22)]), main=paste(bmac2010[i,2],"\n",bmac2010[i,3]))
dev.off()
stars(as.matrix(banks[,c(6,8,10,12,14,16,20,22)]),location=NULL, labels=banks[,2])   