theme_set(theme_bw(18))
setwd("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/experiments/2_dp_comprehension_listenerbelief/results/")

source("rscripts/helpers.r")
load("data/r.RData")

summary(r)
r.lbelief = r
r.lbelief$response_evidence = r.lbelief$response
r.lbelief$Experiment = "listener"
baremust.l = r.lbelief
#baremust.l = r.lbelief[r.lbelief$item_type %in% c("bare","must","might"),c("Directness","item_type","item")]
r.lbelief$response = r.lbelief$listenerbelief
r.lbelief$listenerbelief = NULL
nrow(baremust.l)
head(baremust.l)

load("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/experiments/1_dp_comprehension_speakerbelief/results/data/r.RData")
r.spbelief = r
r.spbelief$Experiment = "speaker"
baremust.s = r.spbelief
#baremust.s = r.spbelief[r.spbelief$item_type %in% c("bare","must","might"),c("Directness","item_type","item")]
nrow(baremust.s)
head(baremust.s)
r.spbelief$response_evidence = r.spbelief$response
r.spbelief$response = r.spbelief$listenerbelief
r.spbelief$listenerbelief = NULL
  
r.combined = merge(r.lbelief,r.spbelief,all=T)
head(r.combined)
summary(r.combined)
write.table(r.combined,file="data/combined.comprehension.experiments.txt",row.names=F,col.names=T,quote=F,sep="\t")


un = unique(r.combined[,c("Directness","item","response_evidence")])
un$Strength = cut(un$Directness,breaks=3,labels=c("weak","medium","strong"))
un$StrengthBin = cut(un$Directness,breaks=3)
un
nrow(un)

library(fields)
ggplot(un, aes(x=Directness, fill=Strength)) +
  geom_histogram() +
  scale_fill_manual(values=rev(designer.colors(n=3, col=c("#046C9A","#ABDDDE"))),name="Evidence\nstrength") +
  #scale_fill_manual(values=wes_palette("Rushmore"),name="Evidence\nstrength") +  
  xlab("Probability of q (evidence strength)") +
  ylab("Number of cases") #+
#  theme(legend.position="top")
ggsave("graphs/strength_histogram.pdf",width=5.5,height=3.2)


### plot beliefs about q
agr = aggregate(response ~ item_type + Experiment, FUN=mean, data=r.combined)
agr$CILow = aggregate(response ~ item_type + Experiment, FUN=ci.low, data=r.combined)$response
agr$CIHigh = aggregate(response ~ item_type + Experiment, FUN=ci.high, data=r.combined)$response
agr$YMin = agr$response - agr$CILow
agr$YMax = agr$response + agr$CIHigh
head(agr)
agr$Utterance = factor(x=as.character(agr$item_type),levels=c("wohl","vermutlich","muss","bare"))
dodge = position_dodge(.9)

ggplot(agr, aes(x=Utterance, y=response, fill=Experiment)) +
  geom_bar(stat="identity",color="black",position=dodge) +
  #  scale_fill_manual(values=designer.colors(n=3, col=c("#046C9A","#ABDDDE")),name="Belief",breaks=levels(agrr$Experiment),label=c("listener (Exp. 3a)", "speaker (Exp. 3b)")) +
  scale_fill_manual(values=wes_palette("Moonrise2"),name="Belief",breaks=levels(agr$Experiment),label=c("listener\n(Exp. 3a)", "speaker\n(Exp. 3b)")) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  #facet_wrap(~ResponseType) +
  scale_y_continuous("Probability of belief in q",breaks=seq(0,1,by=.1)) #+
  #theme(legend.key.size = unit(1.2, "cm"))
#  theme(legend.position="top",plot.margin=unit(c(-0.5,0,0,0),units="cm"))
ggsave("graphs/mean_beliefs.pdf",width=5.5,height=3.5)
#ggsave("pics/mean_beliefs_greg.pdf",width=7.8,height=3)


## evidence
r.combined$StrengthBinSize = cut(r.combined$Directness,breaks=3)
r.combined$Strength = cut(r.combined$Directness,breaks=3,labels=c("weak","medium","strong"))

head(r.combined)
r.combined$Strength_strong = ifelse(r.combined$Strength == "strong",1,0)
r.combined$Strength_medium = ifelse(r.combined$Strength == "medium",1,0)
r.combined$Strength_weak = ifelse(r.combined$Strength == "weak",1,0)

#library(dplyr)
toplot = droplevels(r.combined) %>% 
  select(Strength_strong,Strength_medium,Strength_weak,item_type) %>%
  gather(strength, Proportion, -item_type)

agr = aggregate(Proportion~strength+item_type,data=toplot,FUN="mean")
agr$CILow = aggregate(Proportion~strength+item_type,data=toplot,FUN="ci.low")$Proportion
agr$CIHigh = aggregate(Proportion~strength+item_type,data=toplot,FUN="ci.high")$Proportion
agr$YMin = agr$Proportion - agr$CILow
agr$YMax = agr$Proportion + agr$CIHigh
t=agr
t$StrengthBin = gsub("Strength_","",t$strength)
head(t)
dodge = position_dodge(.9) 

t$Utterance = factor(x=as.character(t$item_type),levels=c("wohl","vermutlich","muss","bare"))
t$Strength = factor(x=t$StrengthBin,levels=c("weak","medium","strong"))

ggplot(t, aes(x=Utterance,y=Proportion,fill=Strength)) +
  geom_bar(stat="identity",position=dodge,color="black") +
  scale_fill_manual(values=rev(designer.colors(n=3, col=c("#046C9A","#ABDDDE"))),name="Evidence\nstrength") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  #scale_fill_manual(values=wes_palette("Royal1"),name="Evidence strength") + #Moonrise2, Darjeeling2, Moonrise3, Chevalier
  ylab("Probability of evidence strength") +
  #facet_wrap(~ResponseType) +
  theme(plot.margin=unit(c(0,0,0,0),units="cm"),axis.title.y=element_text(size=14))  
ggsave("graphs/evidence_dist.pdf")


## diffs in evidence distributions between speakers and listeners
r.combined$StrengthBinSize = cut(r.combined$Directness,breaks=3)
r.combined$Strength = cut(r.combined$Directness,breaks=3,labels=c("weak","medium","strong"))

head(r.combined)
r.combined$Strength_strong = ifelse(r.combined$Strength == "strong",1,0)
r.combined$Strength_medium = ifelse(r.combined$Strength == "medium",1,0)
r.combined$Strength_weak = ifelse(r.combined$Strength == "weak",1,0)

#library(dplyr)
toplot = droplevels(r.combined) %>% 
  select(Strength_strong,Strength_medium,Strength_weak,item_type,Experiment) %>%
  gather(strength, Proportion, -item_type, -Experiment)

agr = aggregate(Proportion~strength+item_type+Experiment,data=toplot,FUN="mean")
agr$CILow = aggregate(Proportion~strength+item_type+Experiment,data=toplot,FUN="ci.low")$Proportion
agr$CIHigh = aggregate(Proportion~strength+item_type+Experiment,data=toplot,FUN="ci.high")$Proportion
agr$YMin = agr$Proportion - agr$CILow
agr$YMax = agr$Proportion + agr$CIHigh
t=agr
t$StrengthBin = gsub("Strength_","",t$strength)
head(t)
dodge = position_dodge(.9) 

t$Utterance = factor(x=as.character(t$item_type),levels=c("wohl","vermutlich","muss","bare"))
t$Strength = factor(x=t$StrengthBin,levels=c("weak","medium","strong"))

ggplot(t, aes(x=Utterance,y=Proportion,fill=Strength)) +
  geom_bar(stat="identity",position=dodge,color="black") +
  scale_fill_manual(values=rev(designer.colors(n=3, col=c("#046C9A","#ABDDDE"))),name="Evidence\nstrength") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  #scale_fill_manual(values=wes_palette("Royal1"),name="Evidence strength") + #Moonrise2, Darjeeling2, Moonrise3, Chevalier
  ylab("Probability of evidence strength") +
  facet_wrap(~Experiment) +
  theme(plot.margin=unit(c(0,0,0,0),units="cm"),axis.title.y=element_text(size=14))  
ggsave("graphs/evidence_dist_byexperiment.pdf")


# average evidence strength by utterance (also do it by item and experiment)
summary(r.combined)
agr = aggregate(Directness ~ item_type, data=r.combined,FUN="mean")
agr$CILow = aggregate(Directness ~ item_type, data=r.combined,FUN="ci.low")$Directness
agr$CIHigh = aggregate(Directness ~ item_type, data=r.combined,FUN="ci.high")$Directness
agr$YMin = agr$Directness - agr$CILow
agr$YMax = agr$Directness + agr$CIHigh
agr$Utterance = factor(x=as.character(agr$item_type),levels=c("wohl","vermutlich","muss","bare"))

ggplot(agr, aes(x=Utterance,y=Directness)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25)
ggsave("graphs/average_strength_by_utterance.pdf",width=5)

m = lmer(Directness ~ item_type + (1+item_type|subject) + (1+item_type|item),data=r.combined) 
summary(m)

# helmert-coded utterance contrasts
contrasts(r.combined$item_type) = cbind("bare.vs.rest"=c(3/4,-1/4,-1/4,-1/4),"wohl.vs.mussverm"=c(0,2/3,-1/3,-1/3),"muss.vs.vermutlich"=c(0,0,1/2,-1/2))

m = lmer(Directness ~ item_type + (1+item_type|subject) + (1|item),data=r.combined) 
summary(m)

library(lmerTest)
m = lmer(Directness ~ item_type + (1+item_type|subject) + (1|item),data=r.combined) 
summary(m)

# sanity check: shouldn't differ by experiment
agr = aggregate(Directness ~ item_type+Experiment, data=r.combined,FUN="mean")
agr$CILow = aggregate(Directness ~ item_type+Experiment, data=r.combined,FUN="ci.low")$Directness
agr$CIHigh = aggregate(Directness ~ item_type+Experiment, data=r.combined,FUN="ci.high")$Directness
agr$YMin = agr$Directness - agr$CILow
agr$YMax = agr$Directness + agr$CIHigh
agr$Utterance = factor(x=as.character(agr$item_type),levels=c("wohl","vermutlich","muss","bare"))
dodge=position_dodge(.9)

ggplot(agr, aes(x=Utterance,y=Directness,fill=Experiment)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25)

# check for item differences
agr = aggregate(Directness ~ item_type+item, data=r.combined,FUN="mean")
agr$CILow = aggregate(Directness ~ item_type+item, data=r.combined,FUN="ci.low")$Directness
agr$CIHigh = aggregate(Directness ~ item_type+item, data=r.combined,FUN="ci.high")$Directness
agr$YMin = agr$Directness - agr$CILow
agr$YMax = agr$Directness + agr$CIHigh
agr$Utterance = factor(x=as.character(agr$item_type),levels=c("wohl","vermutlich","muss","bare"))
dodge=position_dodge(.9)

ggplot(agr, aes(x=Utterance,y=Directness)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  ylab("Strength") +
  facet_wrap(~item)
ggsave("graphs/average_strength_by_item.pdf",width=7.5)

