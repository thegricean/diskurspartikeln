theme_set(theme_bw(18))
library(tidyverse)
source("helpers.r")

########################
# 1. EVIDENCE STRENGTH #
########################

# load english evidence strengths
load("/Users/judithdegen/cogsci/projects/modals/experiments/1_evidence_directness/data/d.RData")
english = d
english$Language = "English"
english$trial = english$order
english$response = english$prob
mapping = read.table("../data/evidence_id_mapping_english.txt",header=T,sep="\t",encoding="utf8",quote="")
mapping
row.names(mapping) = mapping$evidence
english$evidence_id = mapping[as.character(english$evidence),]$evidence_id
table(english$domain,english$evidence_id) # how many ratings for each piece of evidence?

# load german evidence strengths
load("/Users/judithdegen/cogsci/projects/diskurspartikeln/experiments/4_dp_priors_evidencestrength/results/data/r.RData")
german = r
german$workerID = as.numeric(as.character(german$subject)) + 40
german$domain = as.factor(ifelse(german$item == "abendessen","dinner",ifelse(german$item == "kaffee", "coffee", ifelse(german$item == "hund","dog","rain"))))
german$Language = "German"
table(german$domain,german$evidence_id) # how many ratings for each piece of evidence?

# how many participants in each?
length(levels(as.factor(as.character(english$workerID)))) #40
length(levels(as.factor(as.character(german$workerID)))) #39 -- why one less??

# merge datasets
nrow(english)
nrow(german)
d = merge(english, german, all=T) %>%
  select(workerID, domain, evidence, evidence_id, Language, trial, response)
nrow(d)
summary(d)
d$workerID = as.factor(as.character(d$workerID))
d$Language = as.factor(as.character(d$Language))

# plot histograms side by side
ggplot(d, aes(x=response)) +
  geom_histogram() +
  facet_wrap(~Language)

# plot histograms of mean evidence strengths side by side, first aggregating by item
agr = d %>%
  group_by(Language,domain,evidence_id) %>%
  summarise(mean=mean(response),ci.low=ci.low(response),ci.high=ci.high(response),median=median(response))
agr = as.data.frame(agr)
agr

ggplot(agr, aes(x=mean)) +
  geom_histogram(binwidth=.05) +
  facet_wrap(~Language) +
  scale_x_continuous(name="Mean evidence strength") +
  scale_y_continuous(name="Number of cases")  

english = droplevels(agr[agr$Language == "English",])
english = english[order(english[,c("mean")],decreasing=T),]
combos = paste(english$domain,gsub("evidence","",english$evidence_id),sep="-")

d = d %>%
  mutate(combo=paste(domain,gsub("evidence","",as.character(evidence_id)),sep="-")) %>%
  mutate(newcombo=fct_relevel(combo,combos))

# add evidence types
priors = read.csv("../data/evidence_priors.txt")
priors = priors %>%
  mutate(combo=paste(item_english,gsub("evidence","",as.character(evidence_id)),sep="-")) %>%
  select(combo,evidence_type,red_evidence_type) 
  
d = d %>%
  left_join(priors,by=c("combo"))

ggplot(d, aes(x=newcombo,y=response,fill=red_evidence_type)) +
  geom_boxplot(outlier.colour="gray60") +
  stat_summary(fun.y=mean, geom="point", shape=16, size=4) +
  facet_wrap(~Language) +
  scale_x_discrete(name="Piece of evidence") +
  scale_y_continuous(name="Evidence strength")  +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave("../graphs/evidencestrength-boxplots-byreducedevidencetype.pdf",height=4,width=10)

ggplot(d, aes(x=newcombo,y=response,fill=evidence_type)) +
  geom_boxplot(outlier.colour="gray60") +
  stat_summary(fun.y=mean, geom="point", shape=16, size=4) +
  facet_wrap(~Language) +
  scale_x_discrete(name="Piece of evidence") +
  scale_y_continuous(name="Evidence strength")  +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave("../graphs/evidencestrength-boxplots-byevidencetype.pdf",height=4,width=10)

ggplot(d, aes(x=newcombo,y=response,fill=red_evidence_type,color=evidence_type)) +
  geom_boxplot(outlier.colour="gray60",color="black") +
  stat_summary(fun.y=mean, geom="point", shape=16, size=3) +
  facet_wrap(~Language) +
  scale_x_discrete(name="Piece of evidence") +
  scale_y_continuous(name="Evidence strength")  +
  scale_fill_manual(values=c("gray50","gray90"),name="Evidence type\n(reduced)") +
  scale_color_discrete(name="Evidence type\n(full)") +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave("../graphs/evidencestrength-boxplots.pdf",height=3.2,width=8.5)

# analysis: do the two language distributions differ? nope
m = lmer(response ~ Language + (1|workerID) + (1+Language|combo), data=d)
summary(m)

library(lmerTest)
d = d %>%
  mutate(evidence_type=fct_relevel(evidence_type, "perceptual","reportative"))
m.1 = lmer(response ~ Language + evidence_type + (1|workerID) + (1+Language|combo), data=d)
summary(m.1)

m.2 = lmer(response ~ Language + red_evidence_type + (1|workerID) + (1+Language|combo), data=d)
summary(m.2)



#################
# 2. PRODUCTION #
#################

# load english production data
load("/Users/judithdegen/cogsci/projects/modals/experiments/71_modals_forced_production/results/data/r.RData")
english_p = r
english_p$Language = "English"

head(english_p)
nrow(english_p)

# load german production data
load("/Users/judithdegen/cogsci/projects/diskurspartikeln/experiments/3_dp_production/results/data/r.RData")
german_p = r
german_p$Language = "German"
german_p$workerid = german_p$subject + 40
german_p$item = german_p$item_english

# merge datasets
d = merge(english_p, german_p, all=T)  %>%
  select(workerid, item, Language, evidence_type, trial, response, EvidenceTypeCategorical, Directness)
nrow(d)
d$workerid = as.factor(as.character(d$workerid))
d$Language = as.factor(as.character(d$Language))
summary(d)
nrow(d)

# how many participants in each?
length(levels(as.factor(as.character(english_p$workerid)))) #40
length(levels(as.factor(as.character(german_p$subject)))) #38 -- why 2 less?

# plot overall distribution of utterances
d$Utterance = factor(x=d$response,levels=c("bare","must","muss","probably","might","wohl","vermutlich"))
agr = d %>%
  mutate(bare=ifelse(Utterance == "bare",1,0),
         must=ifelse(Utterance == "must",1,0),
         might=ifelse(Utterance == "might",1,0),
         probably=ifelse(Utterance == "probably",1,0),
         vermutlich=ifelse(Utterance == "vermutlich",1,0),
         muss=ifelse(Utterance == "muss",1,0),
         wohl=ifelse(Utterance == "wohl",1,0)) %>%
  select(Language,bare,must,might,probably,vermutlich,muss,wohl) %>%
  gather(Utterance,Produced,-Language) %>%
  group_by(Language,Utterance) %>%
  summarize(Probability=mean(Produced),
            cilow=ci.low(Produced),
            cihigh=ci.high(Produced)) %>%
  ungroup() %>%
  mutate(YMax = Probability + cihigh,
         YMin = Probability - cilow)
agr = as.data.frame(agr)
agr = droplevels(agr[agr$Probability > 0,])
agr$Utt = factor(x=agr$Utterance,levels=c("bare","must","muss","probably","might","wohl","vermutlich"))

ggplot(agr, aes(x=Utt,y=Probability)) +
  geom_bar(stat="identity",fill="gray80",color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  facet_wrap(~Language, scales="free") +
  scale_x_discrete("Utterance") +
  scale_y_continuous("Probability of utterance")
ggsave("production-distribution.pdf",width=8,height=4)

# plot mean evidence strength by utterance
agr = d %>%
  group_by(Language,response) %>%
  summarise(mean=mean(Directness),ci.low=ci.low(Directness),ci.high=ci.high(Directness))
agr = as.data.frame(agr)
head(agr)

dodge = position_dodge(.9)
agr$YMin = agr$mean - agr$ci.low
agr$YMax = agr$mean + agr$ci.high
agr$Utterance = factor(x=agr$response,levels=c("bare","must","muss","probably","might","wohl","vermutlich"))

ggplot(agr, aes(x=Utterance,y=mean)) +
  geom_bar(stat="identity",position=dodge,fill="gray70",color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  facet_wrap(~Language,scales="free") +
  scale_x_discrete(name="Utterance") +
  scale_y_continuous(name="Mean evidence strength")
  #scale_fill_manual(values=c("white","gray70"))
ggsave("mean-production-evidence.pdf",height=4,width=8)

#sub plot
ggplot(agr, aes(x=Utterance,y=mean)) +
  geom_bar(stat="identity",position=dodge,fill="gray70",color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  facet_wrap(~Language,scales="free") +
  scale_x_discrete(name="Production (Exp. 2)") +
  scale_y_continuous(name="Mean evidence strength") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))#+
#scale_fill_manual(values=c("white","gray70"))
ggsave("mean-production-evidence-sub.pdf",height=4,width=5)


bins = substr(levels(cut_number(d$Directness,n=5)),8,11)
bins[5] = 1
d$StrengthBin = as.numeric(as.character(cut_number(d$Directness,n=5,labels=seq(1,5))))
table(d$StrengthBin,d$Language)
agr = d %>%
  mutate(bare=ifelse(Utterance == "bare",1,0),
         must=ifelse(Utterance == "must",1,0),
         might=ifelse(Utterance == "might",1,0),
         probably=ifelse(Utterance == "probably",1,0),
         vermutlich=ifelse(Utterance == "vermutlich",1,0),
         muss=ifelse(Utterance == "muss",1,0),
         wohl=ifelse(Utterance == "wohl",1,0)) %>%
  select(Language,StrengthBin,bare,must,muss,might,probably,vermutlich,wohl) %>%
  gather(Utterance,Produced,-Language,-StrengthBin) %>%
  group_by(Language,Utterance,StrengthBin) %>%
  summarize(Probability=mean(Produced),
            cilow=ci.low(Produced),
            cihigh=ci.high(Produced)) %>%
  ungroup() %>%
  mutate(YMax = Probability + cihigh,
         YMin = Probability - cilow)
agr = as.data.frame(agr)
agr = droplevels(agr[agr$Probability > 0,])
agr$Utt = factor(x=agr$Utterance,levels=c("bare","must","muss","probably","might","vermutlich","wohl"))
  
ggplot(agr, aes(x=StrengthBin, y=Probability, fill=Utt)) + 
  geom_area() +
  guides(fill=guide_legend("Utterance")) +
  scale_x_discrete(name="Evidence strength",breaks=seq(1,5),labels=bins) +
  ylab("Probability of utterance") +
  facet_wrap(~Language)
# ggsave("production-by-strength.pdf",height=4,width=9)

agr = agr[order(agr[,c("Utt")]),]

epalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73")
gpalette <- c("#999999", "#E69F00", "#0072B2", "#D55E00")

english = ggplot(droplevels(agr[agr$Language == "English",]), aes(x=StrengthBin, y=Probability, fill=Utt)) + 
  geom_area(colour="black",size=.05) +
  guides(fill=guide_legend("Utterance",reverse=TRUE)) +
  scale_fill_manual(values=epalette) +
  scale_x_discrete(name="Evidence strength",breaks=seq(1,5),labels=bins) +
  ylab("Probability of utterance")

german = ggplot(droplevels(agr[agr$Language == "German",]), aes(x=StrengthBin, y=Probability, fill=Utt)) + 
  geom_area(colour="black",size=.05) +
  guides(fill=guide_legend("Utterance",reverse=TRUE)) +
  scale_fill_manual(values=gpalette) +
  scale_x_discrete(name="Evidence strength",breaks=seq(1,5),labels=bins) +
  ylab("Probability of utterance")

pdf(file="production-by-strength.pdf",width=13,height=4.5)
grid.arrange(english,german,nrow=1)
dev.off()

english = droplevels(subset(d, Language == "English"))
contrasts(english$response) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
english$complete_item = as.factor(paste(english$item,english$evidence_type))
m = lmer(Directness ~ response + (1|workerid) + (1|item), data = english)
summary(m)

german = droplevels(subset(d, Language == "German"))
contrasts(german$response) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
german$complete_item = as.factor(paste(german$item,german$evidence_type))
m = lmer(Directness ~ response + (1|workerid) + (1|item), data = german)
summary(m)


######################
# 3AB. COMPREHENSION #
######################

# load english listener belief data
load("/Users/judithdegen/cogsci/projects/modals/experiments/72_modals_comprehension_evidence_room/results/data/r.RData")
english_cl = r
english_cl$Language = "English"
english_cl$Belief = "listener"
head(english_cl)

# load english speaker belief data
load("/Users/judithdegen/cogsci/projects/modals/experiments/80_modals_comprehension_speakerbelief/results/data/r.RData")
english_cs = r
english_cs$response = english_cs$Response
english_cs$Language = "English"
english_cs$Belief = "speaker"
english_cs$workerid = english_cs$workerid + 60 # adjust so no workerid overlap
head(english_cs)

# load german listener belief data
load("/Users/judithdegen/cogsci/projects/diskurspartikeln/experiments/2_dp_comprehension_listenerbelief/results/data/r.RData")
german_cl = r
german_cl$Language = "German"
german_cl$Belief = "listener"
german_cl$item = german_cl$item_english
german_cl$workerid = german_cl$subject + 120
head(german_cl)

# load german speaker belief data
load("/Users/judithdegen/cogsci/projects/diskurspartikeln/experiments/1_dp_comprehension_speakerbelief/results/data/r.RData")
german_cs = r
german_cs$Language = "German"
german_cs$Belief = "speaker"
german_cs$item = german_cs$item_english
german_cs$workerid = german_cs$subject + 180
head(german_cs)

# how many participants in each?
length(levels(as.factor(as.character(english_cl$workerid)))) #60
length(levels(as.factor(as.character(english_cs$workerid)))) #60
length(levels(as.factor(as.character(german_cl$subject)))) #56 -- why four less??
length(levels(as.factor(as.character(german_cs$subject)))) # 62

# merge datasets
nrow(english_cl)
nrow(english_cs)
nrow(german_cl)
nrow(german_cs)
english = merge(english_cl,english_cs, all=T)
english$item_type = as.character(english$item_type)
german = merge(german_cl,german_cs, all=T)
german$item_type = as.character(german$item_type)
german$evidence = german$response
german$response = german$listenerbelief
nrow(english)
nrow(german)
d = merge(english, german, all=T) %>%
  select(workerid, item, item_type, Language, trial, response, evidence, Belief, Directness)
nrow(d)
d$workerid = as.factor(as.character(d$workerid))
d$Language = as.factor(as.character(d$Language))
d$evidence = as.factor(as.character(d$evidence))
d$Belief = as.factor(as.character(d$Belief))
d$item_type = as.factor(as.character(d$item_type))
summary(d)

save(d, file="../data/d_comprehension.RData")

# plot mean belief by utterance
agr = d %>%
  group_by(Language,Belief,item_type) %>%
  summarise(mean=mean(response),ci.low=ci.low(response),ci.high=ci.high(response))
agr = as.data.frame(agr)
head(agr)

dodge = position_dodge(.9)
agr$YMin = agr$mean - agr$ci.low
agr$YMax = agr$mean + agr$ci.high
agr$Utterance = factor(x=agr$item_type,levels=c("bare","must","muss","probably","might","wohl","vermutlich"))
agr$Belief = as.factor(ifelse(agr$Belief == "listener","listener\n(Exps. 3a)","speaker\n(Exps. 3b)"))

ggplot(agr, aes(x=Utterance,y=mean,fill=Belief)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",position=dodge,color="black",show_guide=F) +  
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~Language,scales="free") +
  scale_x_discrete(name="Utterance") +
  scale_y_continuous(name="Mean degree of belief") +
  scale_fill_manual(values=c("white","gray70"))
ggsave("mean-beliefs.pdf",height=3.5,width=7.5)

# plot mean belief by utterance for speakers only
agr = d[d$Belief == "speaker",] %>%
  group_by(Language,item_type) %>%
  summarise(mean=mean(response),ci.low=ci.low(response),ci.high=ci.high(response))
agr = as.data.frame(agr)
head(agr)

dodge = position_dodge(.9)
agr$YMin = agr$mean - agr$ci.low
agr$YMax = agr$mean + agr$ci.high
agr$Utterance = factor(x=agr$item_type,levels=c("bare","must","muss","probably","might","wohl","vermutlich"))

ggplot(agr, aes(x=Utterance,y=mean)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",position=dodge,color="black",fill="gray70",show_guide=F) +  
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~Language,scales="free") +
  scale_x_discrete(name="Comprehension (Exp. 3)") +
  scale_y_continuous(name="Mean degree of belief in p") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
ggsave("mean-comprehension-evidence-sub.pdf",height=4,width=5)


english = droplevels(subset(d, Language == "English" & Belief == "listener"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(response ~ item_type + (1|workerid) + (1|item), data = english)
summary(m)

english = droplevels(subset(d, Language == "English" & Belief == "speaker"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(response ~ item_type + (1|workerid) + (1|item), data = english)
summary(m)

english = droplevels(subset(d, Language == "English"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(response ~ item_type*Belief + (1|workerid) + (1|item), data = english)
summary(m)

german = droplevels(subset(d, Language == "German" & Belief == "listener"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(response ~ item_type + (1|workerid) + (1|item), data = german)
summary(m)

german = droplevels(subset(d, Language == "German" & Belief == "speaker"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(response ~ item_type + (1|workerid) + (1|item), data = german)
summary(m)

german = droplevels(subset(d, Language == "German"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(response ~ item_type*Belief + (1|workerid) + (1|item), data = german)
summary(m)

# plot mean inferred evidence strength by utterance
agr = d %>%
  group_by(Language,Belief,item_type) %>%
  summarise(mean=mean(Directness),ci.low=ci.low(Directness),ci.high=ci.high(Directness))  
agr = as.data.frame(agr)
head(agr)

dodge = position_dodge(.9)
agr$YMin = agr$mean - agr$ci.low
agr$YMax = agr$mean + agr$ci.high
agr$Utterance = factor(x=agr$item_type,levels=c("bare","must","muss","probably","might","wohl","vermutlich"))
agr$Belief = as.factor(ifelse(agr$Belief == "listener","listener\n(Exps. 3a)","speaker\n(Exps. 3b)"))

ggplot(agr, aes(x=Utterance,y=mean,fill=Belief)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",position=dodge,color="black",show_guide=F) +  
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~Language,scales="free") +
  scale_x_discrete(name="Utterance") +
  scale_y_continuous(name="Mean strength of inferred evidence") +
  scale_fill_manual(values=c("white","gray70"))
ggsave("mean-evidence.pdf",height=3.5,width=7.5)

english = droplevels(subset(d, Language == "English" & Belief == "listener"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(Directness ~ item_type + (1|workerid) + (1|item), data = english)
summary(m)

english = droplevels(subset(d, Language == "English" & Belief == "speaker"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(Directness ~ item_type + (1|workerid) + (1|item), data = english)
summary(m)

english = droplevels(subset(d, Language == "English"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(Directness ~ item_type*Belief + (1|workerid) + (1|item), data = english)
summary(m)

german = droplevels(subset(d, Language == "German" & Belief == "listener"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(Directness ~ item_type + (1|workerid) + (1|item), data = german)
summary(m)

german = droplevels(subset(d, Language == "German" & Belief == "speaker"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(Directness ~ item_type + (1|workerid) + (1|item), data = german)
summary(m)

german = droplevels(subset(d, Language == "German"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(Directness ~ item_type*Belief + (1|workerid) + (1|item), data = german)
summary(m)

# histogram of inferred evidence pieces
ggplot(d, aes(x=evidence)) +
  geom_histogram() +
  facet_grid(Language~item_type)

