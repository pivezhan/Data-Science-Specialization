setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\statistical inference\\swirl")
library(swirl)
swirl()
head(pValues)
length(pValues<.05)
sum(pValues<.05)
p.adjust(p_Values,method"bonferroni")
p.adjust(p_Values,method="bonferroni")
p.adjust(method="bonferroni")
p.adjust
sum(p.adjust(pValues,method="bonferroni") < 0.05)
sum(p.adjust(pValues,method="BH") < 0.05)
til(trueStatus)
tail(trueStatus)
tail(TRUE,"zero")
table(pValues2 < 0.05, trueStatus)
24/500
p.adjust(pValues,method="bonferroni")
table(p.adjust(pValues,method="bonferroni"))
table(p.adjust(pValues,method="bonferroni")<.05,TrueStatus)
table(p.adjust(pValues,method="bonferroni")<.05, TrueStatus)
table(p.adjust(pValues,method="bonferroni")<.05, trueStatus)
table(p.adjust(pValues2,method="bonferroni")<.05, trueStatus)
table(p.adjust(pValues2,method="BH")<.05, trueStatus)
3.5
print(g2)
head(sh)
nh
resampledMedians
median(resampledMedians)
median(sh)
sam<-fh+nh*B
sam <- sample(fh,nh*B,replace=TRUE)
resam <- matrix(sam,B,nh)
meds <-apply(resam,1,median)
meds-fh
median(meds)-median(fh)
sd(meds)
sd(resampledMedians)
qnorm(resampledMedians,c(.025,.975))
quantile(resampledMedians,c(.025,.975))
quantile(meds,c(.025,.975))
dim(InsectSprays)
names(InsectSprays)
Bdata$count
range(Bdata$count)
Cdata
range(Cdata$count)
BCcounts
group
testStat
obs<-testStat(BCcounts,group)
obs
mean(v)
mean( Bdata$count-Cdata$count)
sample
sample(group)
perms <- sapply(1 : 10000, function(i) testStat(BCcounts, sample(group)))
mean(perms-obs)
mean(perms>obs)
testStat(DEcounts,group)
perms <- sapply(1 : 10000, function(i) testStat(DEcounts, sample(group)))
