library(ggplot2)
theme_set(theme_bw(18))
setwd("~/cogsci/projects/stanford/projects/modals/modals/experiments/80_modals_comprehension_speakerbelief/results/")
source("rscripts/helpers.r")
load("data/r.RData")
summary(r)

ggplot(r, aes(x=Response,fill=item_type)) +
  geom_histogram() +
  facet_wrap(~workerid)
ggsave("graphs/subjectvariability.pdf",width=12,height=15)

agr = aggregate(Response ~ item_type,data=r,FUN=mean)
agr$SD = aggregate(Response ~ item_type,data=r,FUN=sd)$Response
agr
agr$CILow = aggregate(Response ~ item_type,data=r, FUN=ci.low)$Response
agr$CIHigh = aggregate(Response ~ item_type,data=r,FUN=ci.high)$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh

agr$Modal = factor(x=as.character(agr$item_type),levels=c("bare","must","probably","might"))
ggplot(agr, aes(x=Modal,y=Response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25)
ggsave("graphs/means.pdf")

ggplot(agr, aes(x=Modal,y=Response)) +
  geom_bar(stat="identity",fill="gray80",color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  xlab("Utterance") +
  ylab("Probability")
ggsave("graphs/means_bars.pdf",width=5.5)

agr = aggregate(Response ~ item_type + item,data=r,FUN=mean)
agr$CILow = aggregate(Response ~ item_type+ item,data=r, FUN=ci.low)$Response
agr$CIHigh = aggregate(Response ~ item_type+ item,data=r,FUN=ci.high)$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh

agr$Modal = factor(x=as.character(agr$item_type),levels=c("bare","must","probably","might"))
ggplot(agr, aes(x=Modal,y=Response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  facet_wrap(~item) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
ggsave("graphs/means_byitem.pdf")

r.speakerbelief = r
load("~/cogsci/projects/stanford/projects/modals/modals/experiments/72_modals_comprehension_evidence_room/results/data/r.RData")
r$Response = r$response
r.listenerbelief = r
r.listenerbelief$Belief = "listener"
r.speakerbelief$Belief = "speaker"
nrow(r.listenerbelief)
nrow(r.speakerbelief)
r = merge(r.listenerbelief,r.speakerbelief,all=T)
nrow(r)
head(r)

agr = aggregate(Response ~ item_type + Belief,data=r,FUN=mean)
agr$SD = aggregate(Response ~ item_type + Belief,data=r,FUN=sd)$Response
agr
agr$CILow = aggregate(Response ~ item_type + Belief,data=r, FUN=ci.low)$Response
agr$CIHigh = aggregate(Response ~ item_type + Belief,data=r,FUN=ci.high)$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh

agr$Modal = factor(x=as.character(agr$item_type),levels=c("bare","must","probably","might"))
ggplot(agr, aes(x=Modal,y=Response,color=Belief)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25)
ggsave("graphs/means_bybelief.pdf")

dodge = position_dodge(.9)
agr = droplevels(agr[agr$Modal != "probably",])
ggplot(agr, aes(x=Modal,y=Response,fill=Belief)) +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  xlab("Utterance") +
  ylab("Probability of (degree of belief in) q")
ggsave("graphs/means_bybelief_cuny.pdf")

agr = aggregate(Response ~ item_type + item + Belief,data=r,FUN=mean)
agr$CILow = aggregate(Response ~ item_type+ item + Belief,data=r, FUN=ci.low)$Response
agr$CIHigh = aggregate(Response ~ item_type+ item + Belief,data=r,FUN=ci.high)$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh

agr$Modal = factor(x=as.character(agr$item_type),levels=c("bare","must","probably","might"))
ggplot(agr, aes(x=Modal,y=Response,color=Belief)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  facet_wrap(~item) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
ggsave("graphs/means_byitem_bybelief.pdf")



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

