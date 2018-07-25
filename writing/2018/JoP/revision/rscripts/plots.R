theme_set(theme_bw(18))
set.seed(42)
library(tidyverse)
library(brms)
library(gridExtra)
source("helpers.r")
source("createLaTeXTable.R")

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
  select(combo,evidence_type) 
  
d = d %>%
  left_join(priors,by=c("combo"))
d$red_evidence_type = as.factor(ifelse(d$evidence_type == "inferential","inferential","other"))
d$red_evidence_type_direct = as.factor(ifelse(d$evidence_type == "direct","direct","other"))

ggplot(d, aes(x=newcombo,y=response,fill=evidence_type)) +
  geom_boxplot(outlier.colour="gray60") +
  stat_summary(fun.y=mean, geom="point", shape=16, size=4) +
  facet_wrap(~Language) +
  scale_x_discrete(name="Piece of evidence") +
  scale_y_continuous(name="Evidence strength")  +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave("../graphs/evidencestrength-boxplots-byreducedevidencetype.pdf",height=4,width=10)

ggplot(d, aes(x=newcombo,y=response,fill=evidence_type)) +
  geom_boxplot(outlier.colour="gray60",color="black") +
  stat_summary(fun.y=mean, geom="point", shape=16, size=3) +
  facet_wrap(~Language) +
  scale_x_discrete(name="Piece of evidence") +
  scale_y_continuous(name="Evidence strength")  +
  # scale_fill_manual(values=c("gray50","gray90"),name="Evidence type\n(reduced)") +
  scale_fill_discrete(name="Evidence type") +
  # scale_color_discrete(name="Evidence type\n(full)") +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1,size=10))
ggsave("../graphs/evidencestrength-boxplots.pdf",height=4.2,width=10)

# analysis: do the two language distributions differ? nope
m = lmer(response ~ Language + (1|workerID) + (1+Language|combo), data=d)
summary(m)

library(lmerTest)
d = d %>%
  mutate(evidence_type=fct_relevel(evidence_type, "direct","reported"))
m.1 = lmer(response ~ Language + evidence_type + (1|workerID) + (1+Language|combo), data=d)
summary(m.1)

# d = d %>%
  # mutate(red_evidence_type=fct_relevel(red_evidence_type, "other"))
m.2 = lmer(response ~ Language + red_evidence_type_direct + (1|workerID) + (1+Language|combo), data=d)
summary(m.2)

# CONTINUE BY RUNNING BRM?
m.1.b = brm(response ~ Language + evidence_type + (1|workerID) + (1+Language|combo), data=d)
summary(m.1.b)

m.2.b = brm(response ~ Language + red_evidence_type + (1|workerID) + (1+Language|combo), data=d)
summary(m.2.b)

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
  select(workerid, item, Language, evidence_type, trial, response, EvidenceTypeCategorical, Directness) %>%
  rename(evidence_id=evidence_type)
nrow(d)
d$workerid = as.factor(as.character(d$workerid))
d$Language = as.factor(as.character(d$Language))
summary(d)
nrow(d)

# how many participants in each?
length(levels(as.factor(as.character(english_p$workerid)))) #40
length(levels(as.factor(as.character(german_p$subject)))) #38 

# add evidence types
d = d %>%
  mutate(combo=paste(item,gsub("evidence","",as.character(evidence_id)),sep="-")) %>%
  mutate(newcombo=fct_relevel(combo,combos))

priors = read.csv("../data/evidence_priors.txt")
priors = priors %>%
  mutate(combo=paste(item_english,gsub("evidence","",as.character(evidence_id)),sep="-")) %>%
  select(combo,evidence_type) 

d = d %>%
  left_join(priors,by=c("combo"))
d$red_evidence_type = as.factor(ifelse(d$evidence_type == "inferential","inferential","other"))
d$red_evidence_type_direct = as.factor(ifelse(d$evidence_type == "direct","direct","other"))
d$evidence_type_nowishful = as.character(d$evidence_type)
d[d$evidence_type == "wishful",]$evidence_type_nowishful = "inferential"
d$evidence_type_nowishful = as.factor(as.character(d$evidence_type_nowishful))

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
  summarise(Probability=mean(Produced),
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


# plot by evidence strength (figure 4a)
bins = substr(levels(cut_number(d$Directness,n=5)),8,11)
bins[5] = "1"
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
  summarise(Probability=mean(Produced),
            cilow=ci.low(Produced),
            cihigh=ci.high(Produced)) %>%
  ungroup() %>%
  mutate(YMax = Probability + cihigh,
         YMin = Probability - cilow)
agr = as.data.frame(agr)
agr = droplevels(agr[agr$Probability > 0,])
agr$Utt = factor(x=agr$Utterance,levels=rev(c("bare","must","muss","probably","might","vermutlich","wohl")))


agr = agr[order(agr[,c("Utt")]),]

epalette <- rev(c("#999999", "#E69F00", "#56B4E9", "#009E73"))
gpalette <- rev(c("#999999", "#E69F00", "#0072B2", "#D55E00"))

english = ggplot(droplevels(agr[agr$Language == "English",]), aes(x=StrengthBin, y=Probability, fill=Utt)) + 
  geom_area(colour="black",size=.05) +
  guides(fill=guide_legend("Utterance",reverse=TRUE)) +
  scale_fill_manual(values=epalette) +
  scale_x_continuous(name="Evidence strength",breaks=seq(1,5),labels=bins) +
  ylab("Utterance proportion")

german = ggplot(droplevels(agr[agr$Language == "German",]), aes(x=StrengthBin, y=Probability, fill=Utt)) + 
  geom_area(colour="black",size=.05) +
  guides(fill=guide_legend("Utterance",reverse=TRUE)) +
  scale_fill_manual(values=gpalette) +
  scale_x_continuous(name="Evidence strength",breaks=seq(1,5),labels=bins) +
  ylab("Utterance proportion")

pdf(file="../graphs/production-by-strength.pdf",width=13,height=4.5)
grid.arrange(english,german,nrow=1)
dev.off()

# plot by evidence type (figure 4b)
# english production data

gpalette <- c("#999999", "#E69F00", "#0072B2", "#D55E00")
epalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73")

agr_e_sums = d %>%
  filter(Language == "English") %>%
  group_by(evidence_type) %>%
  count() %>%
  rename(sum="n")

agr_e = d %>%
  filter(Language == "English") %>%
  group_by(evidence_type,Utterance) %>%
  count() %>%
  left_join(agr_e_sums,by="evidence_type") %>%
  ungroup() %>%
  mutate(Proportion = n/sum) %>%
  mutate(evidence_type=fct_relevel(evidence_type,"wishful","inferential","reported"),Utterance=fct_relevel(Utterance,"bare","must","probably"))

english_type = ggplot(agr_e, aes(x=evidence_type, y=Proportion, fill=Utterance)) + 
  geom_bar(stat="identity",position = position_fill(reverse = TRUE),color="black") +
  guides(fill=guide_legend("Utterance")) +
  scale_fill_manual(values=epalette) +
  scale_x_discrete(name="Evidence type") +
  ylab("Utterance proportion") 
english_type

# german production data
agr_d_sums = d %>%
  filter(Language == "German") %>%
  group_by(evidence_type) %>%
  count() %>%
  rename(sum="n")

agr_d = d %>%
  filter(Language == "German") %>%
  group_by(evidence_type,Utterance) %>%
  count() %>%
  left_join(agr_d_sums,by="evidence_type") %>%
  ungroup() %>%
  mutate(Proportion = n/sum) %>%
  mutate(evidence_type=fct_relevel(evidence_type,"wishful","inferential","reported"),Utterance=fct_relevel(Utterance,"bare","muss","vermutlich","wohl"))

german_type = ggplot(agr_d, aes(x=evidence_type, y=Proportion, fill=Utterance)) + 
  geom_bar(stat="identity",position = position_fill(reverse = TRUE),color="black") +
  guides(fill=guide_legend("Utterance")) +
  scale_fill_manual(values=gpalette) +
  scale_x_discrete(name="Evidence type") +
  ylab("Utterance proportion") 
german_type

pdf(file="../graphs/production-by-type.pdf",width=13,height=4.5)
grid.arrange(english_type,german_type,nrow=1)
dev.off()

##############
## analysis
#############
d$BareChosen = d$response == "bare"
d$MustChosen = d$response == "must"
d$MussChosen = d$response == "muss"
d$ProbablyChosen = d$response == "probably"
d$VermutlichChosen = d$response == "vermutlich"
d$MightChosen = d$response == "might"
d$WohlChosen = d$response == "wohl"
d$Inferential = d$red_evidence_type == "inferential"

# residualize evidence strength against evidence type
# m.resid = lm(Directness ~ evidence_type,data=d)
# summary(m.resid)
# d$residDirectness = residuals(m.resid)
# pairscor.fnc(d[,c("Directness","residDirectness","evidence_type")])

# ENGLISH ANALYSIS
english = droplevels(subset(d, Language == "English"))
contrasts(english$response) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
english = english %>%
  mutate(ev= case_when(
    red_evidence_type_direct == "direct" ~ 1,
    red_evidence_type_direct == "other" ~ 0,
  )) %>%
  mutate(cDirectness = Directness - mean(Directness), cEvidence = ev - mean(ev))

# lmer analysis

# bare
m.bare = glmer(BareChosen ~ cDirectness + cEvidence + (1|workerid) + (1|combo), data = english, family="binomial")
summary(m.bare)
createLatexTable(m.bare,predictornames=c("Intercept","Evidence strength","Evidence direct"))

# must
m.must = glmer(MustChosen ~ cDirectness + cEvidence + (1|workerid) + (1|combo), data = english, family="binomial")
summary(m.must)
createLatexTable(m.must,predictornames=c("Intercept","Evidence strength","Evidence direct"))

# probably
m.probably = glmer(ProbablyChosen ~ cDirectness + cEvidence + (1|workerid) + (1|combo), data = english, family="binomial")
summary(m.probably)
createLatexTable(m.probably,predictornames=c("Intercept","Evidence strength","Evidence direct"))

# might
m.might = glmer(MightChosen ~ cDirectness + cEvidence + (1|workerid) + (1|combo), data = english, family="binomial")
summary(m.might)
createLatexTable(m.might,predictornames=c("Intercept","Evidence strength","Evidence direct"))

# backwards analysis of evidence directness by utterance (dispreferred conceptually)
m = lmer(Directness ~ response + (1|workerid) + (1|item), data = english)
summary(m)

# backwards analysis of evidence type by utterance (dispreferred conceptually)
m = glmer(red_evidence_type_direct ~ response + (1|workerid) + (1|item), data = english,family="binomial")
summary(m)

# GERMAN ANALYSIS
german = droplevels(subset(d, Language == "German"))
contrasts(german$response) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
german = german %>%
  mutate(ev= case_when(
    red_evidence_type_direct == "direct" ~ 1,
    red_evidence_type_direct == "other" ~ 0,
  )) %>%
  mutate(cDirectness = Directness - mean(Directness), cEvidence = ev - mean(ev))

# lmer analysis

# bare
m.bare = glmer(BareChosen ~ cDirectness + cEvidence + (1|workerid) + (1|combo), data = german, family="binomial")
summary(m.bare)
createLatexTable(m.bare,predictornames=c("Intercept","Evidence strength","Evidence direct"))

# muss
m.muss = glmer(MussChosen ~ cDirectness + cEvidence + (1|workerid) + (1|combo), data = german, family="binomial")
summary(m.muss)
createLatexTable(m.muss,predictornames=c("Intercept","Evidence strength","Evidence direct"))

# vermutlich
m.vermutlich = glmer(VermutlichChosen ~ cDirectness + cEvidence + (1|workerid) + (1|combo), data = german, family="binomial")
summary(m.vermutlich)
createLatexTable(m.vermutlich,predictornames=c("Intercept","Evidence strength","Evidence direct"))

# wohl
m.wohl = glmer(WohlChosen ~ cDirectness + cEvidence + (1|workerid) + (1|combo), data = german, family="binomial")
summary(m.wohl)
createLatexTable(m.wohl,predictornames=c("Intercept","Evidence strength","Evidence direct"))

# backwards analysis of evidence directness by utterance (dispreferred conceptually)
m = lmer(Directness ~ response + (1|workerid) + (1|combo), data = german)
summary(m)

# backwards analysis of evidence type by utterance (dispreferred conceptually)
m = glmer(red_evidence_type_direct ~ response + (1|workerid) + (1|combo), data = german,family="binomial")
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

# add evidence types
d = d %>%
  mutate(combo=paste(item,gsub("evidence","",as.character(evidence)),sep="-")) %>%
  mutate(newcombo=fct_relevel(combo,combos))

priors = read.csv("../data/evidence_priors.txt")
priors = priors %>%
  mutate(combo=paste(item_english,gsub("evidence","",as.character(evidence_id)),sep="-")) %>%
  select(combo,evidence_type) 

d = d %>%
  left_join(priors,by=c("combo"))
d$red_evidence_type = as.factor(ifelse(d$evidence_type == "inferential","inferential","other"))
d$red_evidence_type_direct = as.factor(ifelse(d$evidence_type == "direct","direct","other"))
d$evidence_type_nowishful = as.character(d$evidence_type)
d[d$evidence_type == "wishful",]$evidence_type_nowishful = "inferential"
d$evidence_type_nowishful = as.factor(as.character(d$evidence_type_nowishful))

d = d %>%
  mutate(ev= as.factor(case_when(
    red_evidence_type_direct == "direct" ~ 1,
    red_evidence_type_direct == "other" ~ 0,
  )))

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
m = lmer(response ~ item_type + (1|workerid) + (1|combo), data = english)
summary(m)

english = droplevels(subset(d, Language == "English" & Belief == "speaker"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(response ~ item_type + (1|workerid) + (1|combo), data = english)
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
  group_by(Language,item_type) %>%
  summarise(mean=mean(Directness),ci.low=ci.low(Directness),ci.high=ci.high(Directness))  
agr = as.data.frame(agr)
head(agr)

dodge = position_dodge(.9)
agr$YMin = agr$mean - agr$ci.low
agr$YMax = agr$mean + agr$ci.high
agr$Utterance = factor(x=agr$item_type,levels=c("bare","must","muss","probably","might","wohl","vermutlich"))
# agr$Belief = as.factor(ifelse(agr$Belief == "listener","listener\n(Exps. 3a)","speaker\n(Exps. 3b)"))

ggplot(agr, aes(x=Utterance,y=mean)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",position=dodge,color="black",show_guide=F,fill="gray80") +  
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~Language,scales="free") +
  scale_x_discrete(name="Utterance") +
  scale_y_continuous(name="Mean inferred evidence strength") +
  scale_fill_manual(values=c("white","gray70"))
ggsave("mean-evidence.pdf",height=4,width=7.5)

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

# plot inferred evidence types by utterance
agr = d %>%
  group_by(Language,item_type,evidence_type) %>%
  tally() %>%
  group_by(Language,item_type) %>%
  mutate(sum=sum(n)) %>%
  ungroup() %>%
  mutate(Proportion=n/sum)

dodge = position_dodge(.9)
agr$Utterance = factor(x=agr$item_type,levels=c("bare","must","muss","probably","might","vermutlich", "wohl"))
# agr$Belief = as.factor(ifelse(agr$Belief == "listener","listener\n(Exps. 3a)","speaker\n(Exps. 3b)"))

ggplot(agr,aes(x=Utterance,y=Proportion,fill=evidence_type)) +
  geom_bar(stat="identity") +
  facet_wrap(~Language,scales="free") +
  scale_x_discrete(name="Utterance") +
  scale_fill_discrete(name="Evidence type") +
  scale_y_continuous(name="Proportion of inferred evidence type") +
  theme(legend.position="top")
ggsave("inferred-evidencetype.pdf",height=5,width=8)

# evidence type analysis
english = droplevels(subset(d, Language == "English" & Belief == "listener"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = glmer(ev ~ item_type + (1|workerid) + (1|item), data = english,family="binomial")
summary(m)

english = droplevels(subset(d, Language == "English" & Belief == "speaker"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = glmer(ev ~ item_type + (1|workerid) + (1|item), data = english,family="binomial")
summary(m)

english = droplevels(subset(d, Language == "English"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = glmer(ev ~ item_type*Belief + (1|workerid) + (1|item), data = english,family="binomial")
summary(m)

# need exclude vermutlich here because there are 0 direct data points
german = droplevels(subset(d, Language == "German" & Belief == "listener" & item_type != "vermutlich"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0),"wohl.vs.muss"=c(0,0,1))
m = glmer(ev ~ item_type + (1|workerid) + (1|item), data = german,family="binomial")
summary(m)

german = droplevels(subset(d, Language == "German" & Belief == "speaker"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = glmer(ev ~ item_type + (1|workerid) + (1|item), data = german,family="binomial")
summary(m)

german = droplevels(subset(d, Language == "German"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = glmer(ev ~ item_type*Belief + (1|workerid) + (1|item), data = german,family="binomial")
summary(m)

m = glmer(ev ~ item_type + (1|workerid) + (1|item), data = german,family="binomial")
summary(m)
