## REQUIRES PACKAGES AND PRODUCTION DATA FROM plots.R IN SAME DIRECTORY

# brm analysis

# bare
m.bare.b = brm(BareChosen ~ cDirectness + evidence_type_nowishful + (1|workerid) + (1|combo), data = english, family="bernoulli")
summary(m.bare.b)
plot(m.bare.b)
# fixed effects:
m.bare.b.effects = as.data.frame(fixef(m.bare.b))
m.bare.b.effects$Predictor = row.names(fixef(m.bare.b))
m.bare.b.effects$P = -555
# posterior probability of estimates < or > 0
m.bare.b.effects[m.bare.b.effects$Predictor == "Intercept",]$P = mean(posterior_samples(m.bare.b, pars = "b_Intercept") > 0)
m.bare.b.effects[m.bare.b.effects$Predictor == "cDirectness",]$P = mean(posterior_samples(m.bare.b, pars = "b_cDirectness") > 0)
m.bare.b.effects[m.bare.b.effects$Predictor == "evidence_typeinferential",]$P = mean(posterior_samples(m.bare.b, pars = "b_evidence_typeinferential") < 0)
m.bare.b.effects[m.bare.b.effects$Predictor == "evidence_typereported",]$P = mean(posterior_samples(m.bare.b, pars = "b_evidence_typereported") > 0)
m.bare.b.effects[m.bare.b.effects$Predictor == "evidence_typewishful",]$P = mean(posterior_samples(m.bare.b, pars = "b_evidence_typewishful") < 0)

m.bare.b.effects = m.bare.b.effects %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  select(Predictor,Estimate,Est.Error,"2.5%ile","97.5%ile",P)
write.table(m.bare.b.effects,file="../data/modeloutput/production_english_bare.txt",row.names=F,sep=",",quote=F)

# must
m.must.b = brm(MustChosen ~ cDirectness + evidence_type + (1+|workerid) + (1|item), data = english, family="bernoulli",control = list(adapt_delta = .99))
summary(m.must.b)
# fixed effects:
m.must.b.effects = as.data.frame(fixef(m.must.b))
m.must.b.effects$Predictor = row.names(fixef(m.must.b))
m.must.b.effects$P = -555
# posterior probability of estimates < or > 0
m.must.b.effects[m.must.b.effects$Predictor == "Intercept",]$P = mean(posterior_samples(m.must.b, pars = "b_Intercept") > 0)
m.must.b.effects[m.must.b.effects$Predictor == "cDirectness",]$P = mean(posterior_samples(m.must.b, pars = "b_cDirectness") > 0)
m.must.b.effects[m.must.b.effects$Predictor == "evidence_typeinferential",]$P = mean(posterior_samples(m.must.b, pars = "b_evidence_typeinferential") < 0)
m.must.b.effects[m.must.b.effects$Predictor == "evidence_typereported",]$P = mean(posterior_samples(m.must.b, pars = "b_evidence_typereported") > 0)
m.must.b.effects[m.must.b.effects$Predictor == "evidence_typewishful",]$P = mean(posterior_samples(m.must.b, pars = "b_evidence_typewishful") < 0)

m.must.b.effects = m.must.b.effects %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  select(Predictor,Estimate,Est.Error,"2.5%ile","97.5%ile",P)
write.table(m.must.b.effects,file="../data/modeloutput/production_english_must.txt",row.names=F,sep=",",quote=F)

# probably
m.probably.b = brm(ProbablyChosen ~ cDirectness + evidence_type + (1+cDirectness+evidence_type|workerid) + (1+cDirectness+evidence_type|item), data = english, family="bernoulli",control = list(adapt_delta = .99))
summary(m.probably.b)
# fixed effects:
m.probably.b.effects = as.data.frame(fixef(m.probably.b))
m.probably.b.effects$Predictor = row.names(fixef(m.probably.b))
m.probably.b.effects$P = -555
# posterior probability of estimates < or > 0
m.probably.b.effects[m.probably.b.effects$Predictor == "Intercept",]$P = mean(posterior_samples(m.probably.b, pars = "b_Intercept") > 0)
m.probably.b.effects[m.probably.b.effects$Predictor == "cDirectness",]$P = mean(posterior_samples(m.probably.b, pars = "b_cDirectness") > 0)
m.probably.b.effects[m.probably.b.effects$Predictor == "evidence_typeinferential",]$P = mean(posterior_samples(m.probably.b, pars = "b_evidence_typeinferential") < 0)
m.probably.b.effects[m.probably.b.effects$Predictor == "evidence_typereported",]$P = mean(posterior_samples(m.probably.b, pars = "b_evidence_typereported") > 0)
m.probably.b.effects[m.probably.b.effects$Predictor == "evidence_typewishful",]$P = mean(posterior_samples(m.probably.b, pars = "b_evidence_typewishful") < 0)

m.probably.b.effects = m.probably.b.effects %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  select(Predictor,Estimate,Est.Error,"2.5%ile","97.5%ile",P)
write.table(m.probably.b.effects,file="../data/modeloutput/production_english_probably.txt",row.names=F,sep=",",quote=F)

# might
m.might.b = brm(MightChosen ~ cDirectness + evidence_type + (1+cDirectness+evidence_type|workerid) + (1+cDirectness+evidence_type|item), data = english, family="bernoulli",control = list(adapt_delta = .99))
summary(m.might.b)
# fixed effects:
m.might.b.effects = as.data.frame(fixef(m.might.b))
m.might.b.effects$Predictor = row.names(fixef(m.might.b))
m.might.b.effects$P = -555
# posterior probability of estimates < or > 0
m.might.b.effects[m.might.b.effects$Predictor == "Intercept",]$P = mean(posterior_samples(m.might.b, pars = "b_Intercept") > 0)
m.might.b.effects[m.might.b.effects$Predictor == "cDirectness",]$P = mean(posterior_samples(m.might.b, pars = "b_cDirectness") > 0)
m.might.b.effects[m.might.b.effects$Predictor == "evidence_typeinferential",]$P = mean(posterior_samples(m.might.b, pars = "b_evidence_typeinferential") < 0)
m.might.b.effects[m.might.b.effects$Predictor == "evidence_typereported",]$P = mean(posterior_samples(m.might.b, pars = "b_evidence_typereported") > 0)
m.might.b.effects[m.might.b.effects$Predictor == "evidence_typewishful",]$P = mean(posterior_samples(m.might.b, pars = "b_evidence_typewishful") < 0)

m.might.b.effects = m.might.b.effects %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  select(Predictor,Estimate,Est.Error,"2.5%ile","97.5%ile",P)
write.table(m.might.b.effects,file="../data/modeloutput/production_english_might.txt",row.names=F,sep=",",quote=F)



# GERMAN ANALYSIS

german = droplevels(subset(d, Language == "German"))
contrasts(german$response) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
german$complete_item = as.factor(paste(german$item,german$evidence_type))
german = german %>%
  mutate(cDirectness = Directness - mean(Directness))

# bare
m.bare.b = brm(BareChosen ~ cDirectness + evidence_type + (1+cDirectness+evidence_type|workerid) + (1+cDirectness+evidence_type|item), data = german, family="bernoulli",control = list(adapt_delta = .99))
summary(m.bare.b)
# fixed effects:
m.bare.b.effects = as.data.frame(fixef(m.bare.b))
m.bare.b.effects$Predictor = row.names(fixef(m.bare.b))
m.bare.b.effects$P = -555
# posterior probability of estimates < or > 0
m.bare.b.effects[m.bare.b.effects$Predictor == "Intercept",]$P = mean(posterior_samples(m.bare.b, pars = "b_Intercept") > 0)
m.bare.b.effects[m.bare.b.effects$Predictor == "cDirectness",]$P = mean(posterior_samples(m.bare.b, pars = "b_cDirectness") > 0)
m.bare.b.effects[m.bare.b.effects$Predictor == "evidence_typeinferential",]$P = mean(posterior_samples(m.bare.b, pars = "b_evidence_typeinferential") < 0)
m.bare.b.effects[m.bare.b.effects$Predictor == "evidence_typereported",]$P = mean(posterior_samples(m.bare.b, pars = "b_evidence_typereported") > 0)
m.bare.b.effects[m.bare.b.effects$Predictor == "evidence_typewishful",]$P = mean(posterior_samples(m.bare.b, pars = "b_evidence_typewishful") < 0)

m.bare.b.effects = m.bare.b.effects %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  select(Predictor,Estimate,Est.Error,"2.5%ile","97.5%ile",P)
write.table(m.bare.b.effects,file="../data/modeloutput/production_german_bare.txt",row.names=F,sep=",",quote=F)

# muss
m.muss.b = brm(MussChosen ~ cDirectness + evidence_type + (1+cDirectness+evidence_type|workerid) + (1+cDirectness+evidence_type|item), data = german, family="bernoulli",control = list(adapt_delta = .99))
summary(m.muss.b)
# fixed effects:
m.muss.b.effects = as.data.frame(fixef(m.muss.b))
m.muss.b.effects$Predictor = row.names(fixef(m.muss.b))
m.muss.b.effects$P = -555
# posterior probability of estimates < or > 0
m.muss.b.effects[m.muss.b.effects$Predictor == "Intercept",]$P = mean(posterior_samples(m.muss.b, pars = "b_Intercept") > 0)
m.muss.b.effects[m.muss.b.effects$Predictor == "cDirectness",]$P = mean(posterior_samples(m.muss.b, pars = "b_cDirectness") > 0)
m.muss.b.effects[m.muss.b.effects$Predictor == "evidence_typeinferential",]$P = mean(posterior_samples(m.muss.b, pars = "b_evidence_typeinferential") < 0)
m.muss.b.effects[m.muss.b.effects$Predictor == "evidence_typereported",]$P = mean(posterior_samples(m.muss.b, pars = "b_evidence_typereported") > 0)
m.muss.b.effects[m.muss.b.effects$Predictor == "evidence_typewishful",]$P = mean(posterior_samples(m.muss.b, pars = "b_evidence_typewishful") < 0)

m.muss.b.effects = m.muss.b.effects %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  select(Predictor,Estimate,Est.Error,"2.5%ile","97.5%ile",P)
write.table(m.muss.b.effects,file="../data/modeloutput/production_german_muss.txt",row.names=F,sep=",",quote=F)

# vermutlich
m.vermutlich.b = brm(VermutlichChosen ~ cDirectness + evidence_type + (1+cDirectness+evidence_type|workerid) + (1+cDirectness+evidence_type|item), data = german, family="bernoulli",control = list(adapt_delta = .99))
summary(m.vermutlich.b)
# fixed effects:
m.vermutlich.b.effects = as.data.frame(fixef(m.vermutlich.b))
m.vermutlich.b.effects$Predictor = row.names(fixef(m.vermutlich.b))
m.vermutlich.b.effects$P = -555
# posterior probability of estimates < or > 0
m.vermutlich.b.effects[m.vermutlich.b.effects$Predictor == "Intercept",]$P = mean(posterior_samples(m.vermutlich.b, pars = "b_Intercept") > 0)
m.vermutlich.b.effects[m.vermutlich.b.effects$Predictor == "cDirectness",]$P = mean(posterior_samples(m.vermutlich.b, pars = "b_cDirectness") > 0)
m.vermutlich.b.effects[m.vermutlich.b.effects$Predictor == "evidence_typeinferential",]$P = mean(posterior_samples(m.vermutlich.b, pars = "b_evidence_typeinferential") < 0)
m.vermutlich.b.effects[m.vermutlich.b.effects$Predictor == "evidence_typereported",]$P = mean(posterior_samples(m.vermutlich.b, pars = "b_evidence_typereported") > 0)
m.vermutlich.b.effects[m.vermutlich.b.effects$Predictor == "evidence_typewishful",]$P = mean(posterior_samples(m.vermutlich.b, pars = "b_evidence_typewishful") < 0)

m.vermutlich.b.effects = m.vermutlich.b.effects %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  select(Predictor,Estimate,Est.Error,"2.5%ile","97.5%ile",P)
write.table(m.vermutlich.b.effects,file="../data/modeloutput/production_german_vermutlich.txt",row.names=F,sep=",",quote=F)

# wohl
m.wohl.b = brm(WohlChosen ~ cDirectness + evidence_type + (1+cDirectness+evidence_type|workerid) + (1+cDirectness+evidence_type|item), data = german, family="bernoulli",control = list(adapt_delta = .99))
summary(m.wohl.b)
# fixed effects:
m.wohl.b.effects = as.data.frame(fixef(m.wohl.b))
m.wohl.b.effects$Predictor = row.names(fixef(m.wohl.b))
m.wohl.b.effects$P = -555
# posterior probability of estimates < or > 0
m.wohl.b.effects[m.wohl.b.effects$Predictor == "Intercept",]$P = mean(posterior_samples(m.wohl.b, pars = "b_Intercept") > 0)
m.wohl.b.effects[m.wohl.b.effects$Predictor == "cDirectness",]$P = mean(posterior_samples(m.wohl.b, pars = "b_cDirectness") > 0)
m.wohl.b.effects[m.wohl.b.effects$Predictor == "evidence_typeinferential",]$P = mean(posterior_samples(m.wohl.b, pars = "b_evidence_typeinferential") < 0)
m.wohl.b.effects[m.wohl.b.effects$Predictor == "evidence_typereported",]$P = mean(posterior_samples(m.wohl.b, pars = "b_evidence_typereported") > 0)
m.wohl.b.effects[m.wohl.b.effects$Predictor == "evidence_typewishful",]$P = mean(posterior_samples(m.wohl.b, pars = "b_evidence_typewishful") < 0)

m.wohl.b.effects = m.wohl.b.effects %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  select(Predictor,Estimate,Est.Error,"2.5%ile","97.5%ile",P)
write.table(m.wohl.b.effects,file="../data/modeloutput/production_german_wohl.txt",row.names=F,sep=",",quote=F)