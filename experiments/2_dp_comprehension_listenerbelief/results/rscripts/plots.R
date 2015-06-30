library(ggplot2)
theme_set(theme_bw(18))
setwd("~/cogsci/projects/stanford/projects/modals/modals/experiments/72_modals_comprehension_evidence_room/results/")
source("rscripts/helpers.r")
load("data/r.RData")
summary(r)
r.lbelief = r
r.lbelief$Experiment = "listener"
baremust.l = r.lbelief[r.lbelief$item_type %in% c("bare","must","might"),c("Directness","item_type","item")]
nrow(baremust.l)
head(baremust.l)

load("~/cogsci/projects/stanford/projects/modals/modals/experiments/80_modals_comprehension_speakerbelief/results/data/r.RData")
r.spbelief = r
r.spbelief$Experiment = "speaker"
baremust.s = r.spbelief[r.spbelief$item_type %in% c("bare","must","might"),c("Directness","item_type","item")]
nrow(baremust.s)
head(baremust.s)
r.spbelief$response = r.spbelief$Response
r.spbelief$Response = NULL
  
r.combined = merge(r.lbelief,r.spbelief,all=T)
r.combined$assignmentid = NULL
head(r.combined)
write.table(r.combined,file="data/combined.comprehension.experiments.txt",row.names=F,col.names=T,quote=F,sep="\t")
write.table(r.combined,file="../../../writing/2014/cuny_2015/poster/data/combined.comprehension.experiments.txt",row.names=F,col.names=T,quote=F,sep="\t")


baremust = droplevels(rbind(baremust.s,baremust.l))
baremust$Experiment = as.factor(as.character(baremust$Experiment))

un = unique(baremust[,c("Directness","item")])
nrow(un)
un$StrengthBinNumObs = cut2(un$Directness,g=3)
un$StrengthBinSize = cut(un$Directness,breaks=3)
row.names(un) = paste(un$Directness, un$item)
summary(un)
baremust$StrengthBinNumObs = un[paste(baremust$Directness, baremust$item),]$StrengthBinNumObs
baremust$StrengthBinSize = un[paste(baremust$Directness, baremust$item),]$StrengthBinSize

summary(baremust)

tfreq = as.data.frame(table(baremust$StrengthBinNumObs,baremust$item_type))
t = as.data.frame(prop.table(table(baremust$StrengthBinNumObs,baremust$item_type),mar=2))
head(t)
head(tfreq)
colnames(t) = c("StrengthBin","Modal","Proportion")
colnames(tfreq) = c("StrengthBin","Modal","Freq")
t$Freq = tfreq$Freq
dodge = position_dodge(.9) 

t$Utterance = factor(x=t$Modal,levels=c("bare","must","might"))
t$Strength = factor(x=t$StrengthBin,levels=rev(levels(t$StrengthBin)))

ggplot(t, aes(x=Utterance,y=Proportion,fill=Strength)) +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_text(aes(label=Freq,y=0.02),position=dodge)
ggsave("graphs/strength_dist_cuny_strengthbinnumobs.pdf",width=6)


tfreq = as.data.frame(table(baremust$StrengthBinSize,baremust$item_type))
t = as.data.frame(prop.table(table(baremust$StrengthBinSize,baremust$item_type),mar=2))
head(t)
head(tfreq)
colnames(t) = c("StrengthBin","Modal","Proportion")
colnames(tfreq) = c("StrengthBin","Modal","Freq")
t$Freq = tfreq$Freq
dodge = position_dodge(.9) 

t$Utterance = factor(x=t$Modal,levels=c("bare","must","might"))
t$Strength = factor(x=t$StrengthBin,levels=rev(levels(t$StrengthBin)))
t$EvidenceBin = rep(c("weak","medium","strong"),3)
# for justine:
write.table(t[,c("Utterance","EvidenceBin","Proportion")],file="data/evidence_given_utterance.txt",quote=F,row.names=F,col.names=F,sep=",")
ggplot(t, aes(x=Utterance,y=Proportion,fill=Strength)) +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_text(aes(label=Freq,y=0.02),position=dodge)
ggsave("graphs/strength_dist_cuny_strengthbinsize.pdf",width=6)

agr = aggregate(Directness ~ item_type, FUN=mean, data=baremust)
agr$CILow = aggregate(Directness ~ item_type, FUN=ci.low, data=baremust)$Directness
agr$CIHigh = aggregate(Directness ~ item_type, FUN=ci.high, data=baremust)$Directness
agr$YMin = agr$Directness - agr$CILow
agr$YMax = agr$Directness + agr$CIHigh

ggplot(agr, aes(x=item_type, y=Directness)) +
  geom_bar(stat="identity",fill="gray80",color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25)
ggsave("graphs/mean_strength_cuny.pdf",width=5)


agr = aggregate(response ~ item_type,data=r,FUN=mean)
agr$SD = aggregate(response ~ item_type,data=r,FUN=sd)$response
agr
agr$CILow = aggregate(response ~ item_type,data=r, FUN=ci.low)$response
agr$CIHigh = aggregate(response ~ item_type,data=r,FUN=ci.high)$response
agr$YMin = agr$response - agr$CILow
agr$YMax = agr$response + agr$CIHigh

agr$Modal = factor(x=as.character(agr$item_type),levels=c("bare","must","probably","might"))
ggplot(agr, aes(x=Modal,y=response)) +
  geom_bar(stat="identity",fill="gray80",color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  xlab("Utterance") +
  ylab("Probability")
ggsave("graphs/means_bars.pdf",width=5.5)

ggplot(agr, aes(x=Modal,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25)
ggsave("graphs/means.pdf")


agr = aggregate(response ~ item_type + item,data=r,FUN=mean)
agr$CILow = aggregate(response ~ item_type+ item,data=r, FUN=ci.low)$response
agr$CIHigh = aggregate(response ~ item_type+ item,data=r,FUN=ci.high)$response
agr$YMin = agr$response - agr$CILow
agr$YMax = agr$response + agr$CIHigh

agr$Modal = factor(x=as.character(agr$item_type),levels=c("bare","must","probably","might"))
ggplot(agr, aes(x=Modal,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  facet_wrap(~item) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
ggsave("graphs/means_byitem.pdf")


# histograms of evidence type
t = as.data.frame(prop.table(table(r$item_type,r$evidence),mar=1))
head(t)
colnames(t) = c("Modal","EvidenceType","Proportion")
t$Condition = factor(x=as.character(t$Modal),levels=c("bare","must","probably","might"))

ggplot(t, aes(x=Condition,y=Proportion,fill=EvidenceType)) +
  geom_bar(stat="identity")
ggsave("graphs/evidence_dist.pdf")

# histograms of evidence type by item
t = as.data.frame(prop.table(table(r$item,r$item_type,r$evidence),mar=c(1,2)))
head(t)
colnames(t) = c("Item","Modal","EvidenceType","Proportion")
t[t$Item == "coffee" & t$Modal == "must",]
t$Condition = factor(x=as.character(t$Modal),levels=c("bare","must","probably","might"))

ggplot(t, aes(x=Condition,y=Proportion,fill=EvidenceType)) +
  geom_bar(stat="identity") +
  facet_wrap(~Item) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
ggsave("graphs/evidence_dist_byitem.pdf")


t = as.data.frame(prop.table(table(r$item,r$item_type,r$evidence),mar=c(1,2)))
head(t)
colnames(t) = c("Item","Modal","Evidence","Proportion")
t$Directness = directness[paste(t$Item,t$Evidence),]$prob
head(t)

ggplot(t, aes(x=Directness,y=Proportion)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Modal)
ggsave("graphs/directness.pdf")

t = as.data.frame(prop.table(table(r$item,r$item_type,r$EvidenceDirectnessCategorical),mar=c(1,2)))
t
colnames(t) = c("Item","Modal","EvidenceDirectness","Proportion")
ggplot(t, aes(x=EvidenceDirectness,y=Proportion)) +
  geom_bar(stat="identity") +
  facet_grid(Item~Modal)
ggsave("graphs/directness_categorical.pdf")

agr = aggregate(Proportion~Modal+EvidenceDirectness,FUN=mean,data=t)
agr$CILow = aggregate(Proportion~Modal+EvidenceDirectness,FUN=ci.low,data=t)$Proportion
agr$CIHigh = aggregate(Proportion~Modal+EvidenceDirectness,FUN=ci.high,data=t)$Proportion
agr$YMin = agr$Proportion - agr$CILow
agr$YMax = agr$Proportion + agr$CIHigh

dodge = position_dodge(.9)

ggplot(agr, aes(x=EvidenceDirectness,y=Proportion)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~Modal)
ggsave("graphs/directness_categorical_means.pdf")

r$BinaryStrength = as.factor(ifelse(r$Directness < .75, "weak","strong"))
r$MixedEvidence = as.factor(paste(r$BinaryStrength,r$EvidenceTypeCategorical))
summary(r[r$EvidenceTypeCategorical == "inferential",])
r$FinalEvidence = as.character(r$MixedEvidence)
r[r$EvidenceDirectnessCategorical == "direct",]$FinalEvidence = "direct"
r[r$EvidenceTypeCategorical == "reportative",]$FinalEvidence = "report"
r$FinalEvidence = as.factor(as.character(r$FinalEvidence))
# do sth more reasonable than exclude these two categories
r = droplevels(subset(r, ! FinalEvidence %in% c("weak perceptual","weak wishful")))

t = as.data.frame(prop.table(table(r$item_type,r$FinalEvidence),mar=c(1)))
head(t)
t = droplevels(subset(t, Var1 %in% c("bare","must")))
t$EType = factor(x=t$Var2, levels=c("direct","strong inferential","weak inferential","report"))
ggplot(t, aes(x=Var1,y=Freq,fill=EType)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_fill_manual(values=c("#023858", "#1d91c0", "#7fcdbb", "#ffffcc"), name="Evidence type") +
  xlab("Utterance") +
  ylab("Proportion") +
  theme(legend.position=c(.5,.8))
ggsave("graphs/cuny_evidencedist.pdf",width=7)

r$EvidenceType = as.factor(ifelse(r$EvidenceDirectnessCategorical == "direct","direct",ifelse(r$EvidenceTypeCategorical == "reportative","reportative","indirect")))
un = unique(r[,c("Directness","item","EvidenceType")])
un$StrengthBinNumObs = cut2(un$Directness,g=3)
un$StrengthBinSize = cut(un$Directness,breaks=3)
un$Strength = un$StrengthBinSize
un$WordStrength = cut(un$Directness,breaks=3,labels=c("weak","medium","strong"))
nrow(un)
un

ggplot(un, aes(x=Directness,fill=WordStrength)) +
  geom_density(alpha=.5)

ggplot(un, aes(x=Directness, fill=Strength)) +
  geom_histogram() +
  xlab("Strength")
  #geom_density(aes(fill=WordStrength),alpha=.5)
ggsave("graphs/strength_histogram_cuny.pdf",width=6,height=4)

ggplot(un, aes(x=Directness,fill=EvidenceType)) +
  geom_histogram()

ggplot(un, aes(x=Directness)) +
  geom_histogram()
ggsave("graphs/evidencedirectness.pdf",width=6)
