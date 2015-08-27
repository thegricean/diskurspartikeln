theme_set(theme_bw(18))
setwd("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/experiments/4_dp_priors_evidencestrength/results/")

source("rscripts/helpers.r")

load("data/r.RData")
r = read.table("data/results.txt",quote="", sep="\t", header=T)
head(r)
nrow(r)
names(r)
r$trial = r$slide_number_in_experiment - 2
r = r[,c("subject", "rt", "evidence_id", "language","age","gender","trial","enjoyment","asses","comments","time_in_minutes","response","evidence","item")]
head(r)

table(r$item,r$evidence_id)
save(r,file="data/r.RData")

##################

ggplot(aes(x=gender), data=r) +
  geom_histogram()

ggplot(aes(x=rt), data=r) +
  geom_histogram() +
  scale_x_continuous(limits=c(0,50000))

ggplot(aes(x=age), data=r) +
  geom_histogram()

ggplot(aes(x=age,fill=gender), data=r) +
  geom_histogram()

ggplot(aes(x=enjoyment), data=r) +
  geom_histogram()

ggplot(aes(x=asses), data=r) +
  geom_histogram()

ggplot(aes(x=time_in_minutes), data=r) +
  geom_histogram()

ggplot(aes(x=age,y=time_in_minutes,color=gender), data=unique(r[,c("subject","age","time_in_minutes","gender")])) +
  geom_point() +
  geom_smooth()

unique(r$comments)

####################

## PLOTS

ggplot(r, aes(x=response)) +
  geom_histogram()

ggplot(r, aes(x=response)) +
  geom_histogram() +
  facet_wrap(~item)

agr = r %>% 
  group_by(item, evidence_id) %>%
  summarise(mean=mean(response),ci.low=ci.low(response),ci.high=ci.high(response),evidence=unique(evidence))
agr$YMin = agr$mean - agr$ci.low
agr$YMax = agr$mean + agr$ci.high

library(grid)
ggplot(agr, aes(x=evidence,y=mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax),width=.25) +
  facet_wrap(~item,scales="free_x",nrow=1) +
  theme(axis.text.x=element_text(angle=55,vjust=1,hjust=1,size=10),plot.margin=unit(c(1,0,0,0),"in"))
ggsave("graphs/evidence_strength.pdf",width=10,height=11)
