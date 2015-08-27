theme_set(theme_bw(18))
setwd("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/experiments/3_dp_production/results/")
library(Hmisc)

source("rscripts/helpers.r")

load("data/r.RData")

# histogram of bare/must choices by 3-binned evidence strength
baremust = droplevels(subset(r, response %in% c("bare","must","might")))
un = unique(baremust[,c("Directness","item")])
nrow(un)
un$StrengthBinNumObs = cut2(un$Directness,g=3)
un$StrengthBinSize = cut(un$Directness,breaks=3)
row.names(un) = paste(un$Directness, un$item)
summary(un)
baremust$StrengthBinNumObs = un[paste(baremust$Directness, baremust$item),]$StrengthBinNumObs
baremust$StrengthBinSize = un[paste(baremust$Directness, baremust$item),]$StrengthBinSize
summary(baremust)

tfreq = as.data.frame(table(baremust$StrengthBinNumObs,baremust$response))
t = as.data.frame(prop.table(table(baremust$StrengthBinNumObs,baremust$response),mar=1))
head(t)
head(tfreq)
colnames(t) = c("StrengthBin","Modal","Proportion")
colnames(tfreq) = c("StrengthBin","Modal","Proportion")
t$Freq = tfreq$Proportion

ggplot(t, aes(x=StrengthBin,y=Proportion,fill=Modal)) +
  geom_bar(stat="identity")# +
  #geom_text(aes(label=Freq,y=Proportion))
ggsave("graphs/utterance_dist_strengthbinnumobs.pdf",width=6)

t = as.data.frame(prop.table(table(baremust$StrengthBinSize,baremust$response),mar=1))
head(t)
colnames(t) = c("StrengthBin","Modal","Proportion")

#t$Utterance = factor(x=t$Modal,levels=c("bare","must","might"))
t$Strength = factor(x=t$StrengthBin,levels=rev(levels(t$StrengthBin)))

ggplot(t, aes(x=Strength,y=Proportion,fill=Modal)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_x_discrete(breaks=levels(t$Strength),labels=paste(c("strong\n","medium\n","weak\n"),levels(t$Strength))) +
  ylab("Probability of utterance choice")
ggsave("graphs/utterance_dist_strengthbinsize.pdf",width=7)



# histograms of modal choice by evidence type
# CAN'T DO THIS RIGHT NOW BECAUSE WE HAVEN'T ANNOTATED EVIDENCE TYPE
# t = as.data.frame(prop.table(table(r$EvidenceTypeCategorical,r$response),mar=1))
# head(t)
# t[t$Var1 == "evidence2",]
# colnames(t) = c("EvidenceType","Modal","Proportion")
# t$ModalChoice = factor(x=as.character(t$Modal),levels=c("bare","must","probably","might"))
# t = droplevels(subset(t, EvidenceType != "wishful"))
# t$Evidence = as.factor(ifelse(t$EvidenceType == "perceptual","direct",ifelse(t$EvidenceType == "reportative","report","inferential")))
# 
# ggplot(t, aes(x=Evidence,y=Proportion,fill=ModalChoice)) +
#   geom_bar(stat="identity")
# ggsave("graphs/modal_dist_cuny_old.pdf",width=6)
# 
# ggplot(t, aes(x=EvidenceType,y=Proportion,fill=ModalChoice)) +
#   geom_bar(stat="identity")
# ggsave("graphs/modal_dist.pdf")
# 
# ggplot(t, aes(x=EvidenceType, y=Proportion, color=ModalChoice, group=ModalChoice)) +
#   geom_point() +
#   geom_line()
# ggsave("graphs/modal_choices_points.pdf")
# 


# histograms of modal choice by evidence type
t = as.data.frame(prop.table(table(r$EvidenceTypeCategorical,r$response),mar=2))
head(t)
colnames(t) = c("EvidenceType","Modal","Proportion")
t$ModalChoice = factor(x=as.character(t$Modal),levels=c("bare","must","probably","might"))

ggplot(t, aes(x=ModalChoice,y=Proportion,fill=EvidenceType)) +
  geom_bar(stat="identity")
ggsave("graphs/evidence_dist.pdf")

# histograms of modal choice by evidence type and item
t = as.data.frame(prop.table(table(r$item,r$evidence_type,r$response),mar=c(1,2)))
head(t)
t[t$Var1 == "coffee" & t$Var2 =="evidence2",]
colnames(t) = c("Item","EvidenceType","Modal","Proportion")
#t$ModalChoice = factor(x=as.character(t$Modal),levels=c("bare","must","probably","might"))

ggplot(t, aes(x=EvidenceType,y=Proportion,fill=Modal)) +
  geom_bar(stat="identity") +
  facet_wrap(~Item)
ggsave("graphs/modal_dist_byitem.pdf")

ggplot(t, aes(x=EvidenceType, y=Proportion, color=Modal, group=Modal)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Item)
ggsave("graphs/modal_choices_points_byitem.pdf")


# modal choice by evidence directness
t = as.data.frame(prop.table(table(r$Directness,r$response),mar=1))
head(t)
colnames(t) = c("Directness","Modal","Proportion")
#t$ModalChoice = factor(x=as.character(t$Modal),levels=c("bare","must","probably","might"))
t$Directness = as.numeric(as.character(t$Directness))

ggplot(t, aes(x=Directness,y=Proportion,color=Modal)) +
  geom_point() +
  #geom_line()
  geom_smooth()
ggsave("graphs/modal_choices_bydirectness.pdf")


# modal choice by evidence directness and categorical evidence type
# t = as.data.frame(prop.table(table(r$EvidenceTypeCategorical,r$Directness,r$response),mar=1))
# head(t)
# t[t$Var1 == .651,]
# colnames(t) = c("EvidenceType","Directness","Modal","Proportion")
# t$ModalChoice = factor(x=as.character(t$Modal),levels=c("bare","must","probably","might"))
# t$Directness = as.numeric(as.character(t$Directness))
# 
# ggplot(t, aes(x=Directness,y=Proportion,color=ModalChoice)) +
#   geom_point() +
#   #geom_line() +
#   geom_smooth() +
#   facet_wrap(~EvidenceType,scales="free_y")
# ggsave("graphs/modal_choices_bydirectness.pdf")
# 
# 
# t = as.data.frame(prop.table(table(r$EvidenceTypeCategorical,r$response),mar=1))
# head(t)
# t[t$Var1 == "inferential",]
# colnames(t) = c("Evidence","Modal","Proportion")
# t$ModalChoice = factor(x=as.character(t$Modal),levels=c("bare","must","probably","might"))
# t$EvidenceType = factor(x=as.character(t$Evidence),levels=c("perceptual","reportative","inferential","wishful"))
# 
# ggplot(t, aes(x=EvidenceType,y=Proportion,color=ModalChoice,group=ModalChoice)) +
#   geom_point() +
#   geom_line() 
#   #geom_smooth() +
#   #facet_wrap(~EvidenceType,scales="free_y")
# ggsave("graphs/modal_choices_byevidencetype.pdf")
# 

## plot just bare and must
baremust = r[r$response %in% c("bare","must") & r$EvidenceTypeCategorical != "wishful",]
baremust = droplevels(baremust)
nrow(baremust)

t = as.data.frame(prop.table(table(baremust$Directness,baremust$response),mar=1))
head(t)
t[t$Var1 == .651,]
colnames(t) = c("Directness","Modal","Proportion")
t$ModalChoice = factor(x=as.character(t$Modal),levels=c("bare","must","probably","might"))
t$Directness = as.numeric(as.character(t$Directness))

ggplot(t, aes(x=Directness,y=Proportion,color=ModalChoice)) +
  geom_point() +
  #geom_line()
  geom_smooth(method="lm")
ggsave("graphs/baremus_choices_bydirectness.pdf")



t = as.data.frame(prop.table(table(baremust$EvidenceTypeCategorical,baremust$response),mar=1))
head(t)
t[t$Var1 == "inferential",]
colnames(t) = c("Evidence","Modal","Proportion")
t$ModalChoice = factor(x=as.character(t$Modal),levels=c("bare","must","probably","might"))
t$EvidenceType = factor(x=as.character(t$Evidence),levels=c("perceptual","reportative","inferential","wishful"))

ggplot(t, aes(x=EvidenceType,y=Proportion,color=ModalChoice,group=ModalChoice)) +
  geom_point() +
  geom_line() 
#geom_smooth() +
#facet_wrap(~EvidenceType,scales="free_y")
ggsave("graphs/baremust_modal_choices_byevidencetype.pdf")
