if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)
finalData=read_rds('data/output/HCRIS_Data.rds')
#number 1
answer1<-final.hcris %>%group_by(provider_number,fyear)%>%
summarize(count=n())
filtered1<-answer1%>% filter(count>1)

test<-unique(filtered1$provider_number)

table1<-table(filtered1$fyear,filtered1$count)
backtoDF<-as.data.frame.matrix(table1)
backtoDF<-mutate(backtoDF,countSum=backtoDF$`2`+backtoDF$`3`)
backtoDF<-rownames_to_column(backtoDF,'Year')
multipleReportGraph<-ggplot(backtoDF,aes(x=Year, y= countSum,group=1))+
  geom_line()+
  ggtitle("Number of providers that turned in more than 1 report")+
  xlab("Year")+
  ylab("Provider count")
hw2figure1<-ggsave(filename="multipleReports.png",multipleReportGraph,height=8,width=10)
#number 2
answer2<- length(unique(final.hcris$provider_number))

# number 3

answer3<- final.hcris.data %>%
  #ungroup()%>%
  filter(tot_charges<10000000 && tot_charges>0)%>%
  ggplot(aes(x = as.factor(year), y = tot_charges)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9)
answer3

hw2figure2<-ggsave(filename="violinPlot.png",answer3,height=8,width=10)
# start of number 4


answer4<-finalData%>%
mutate(discount_factor = 1-tot_discounts/tot_charges)%>%
mutate(price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment)%>%
mutate(price_denom = tot_discharges - mcare_discharges)%>%
mutate(price = price_num/price_denom)
answer4Graph<- answer4 %>% 
  filter(price<20000 && price >0)%>%
  group_by(year)%>%
  ggplot(aes(x = as.factor(year), y = price)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9)
answer4Graph

hw2figure3<-ggsave(filename="violinPlotPrices.png",answer4Graph,height=8,width=10)

# number 5

new2012Final<-answer4%>% filter(price>0 && price < 50000) %>%subset(year==2012)
new2012Final["hvbp_payment"][is.na(new2012Final["hvbp_payment"])] <- 0
new2012Final["hrrp_payment"][is.na(new2012Final["hrrp_payment"])] <- 0
new2012Final<-mutate(new2012Final,penalized=case_when(hvbp_payment-abs(hrrp_payment)<0~1,hvbp_payment-abs(hrrp_payment)>=0~0))
answer5Penalized<-(subset(new2012Final,penalized==1))
answer5.1<-mean(answer5Penalized$price,na.rm=TRUE)
answer5Un<-(subset(new2012Final,penalized==0))
answer5.2<-mean(answer5Un$price,na.rm=TRUE)

#number 6

buckets<-quantile(new2012Final$beds, probs = seq(0, 1, 1/4),na.rm=TRUE)


answer6<-mutate(new2012Final, quartile1=case_when(beds<=buckets[2]~1,TRUE~0))
answer6<-mutate(answer6, quartile2=case_when(beds<=buckets[3]&& beds>buckets[2]~1,TRUE~0))
answer6<-mutate(answer6, quartile3=case_when(beds<=buckets[4]&& beds>buckets[3]~1,TRUE~0))
answer6<-mutate(answer6, quartile4=case_when(beds<=buckets[5]&& beds>buckets[4]~1,TRUE~0))


# create matrix with 4 columns and 4 rows
answer6<- answer6 %>%
  mutate(group = case_when(penalized<=0~"Control",TRUE~"Treatment"))

answer6_complete <- answer6[complete.cases(answer6[, c("price", "penalized")]), ]

table6<-answer6_complete %>% group_by(group,quartile1, quartile2, quartile3, quartile4) %>% summarize(price_mean=mean(price,na.rm=TRUE))
table6

# group by penalized and bed quart before, mix quartiles into 1
answer6_complete$Quartiles<-0
answer6_complete$Quartiles[answer6_complete$quartile1==1]<-1
answer6_complete$Quartiles[answer6_complete$quartile2==1]<-2
answer6_complete$Quartiles[answer6_complete$quartile3==1]<-3
answer6_complete$Quartiles[answer6_complete$quartile4==1]<-4
readr::write_rds(answer6_complete,'data/output/answer6.rds')
table6<- answer6_complete %>%
  group_by(Quartiles, group) %>%
  summarise(mean_price = mean(price)) %>%
  pivot_wider(names_from = group, values_from = mean_price, names_prefix = "mean_price_")
# start of 7.1

answer6_complete<-ungroup(answer6_complete)
covs<-select(answer6_complete,quartile1,quartile2,quartile3,quartile4)
covs<-as_tibble(covs)


nn.est1 <- Matching::Match(Y=answer6_complete$price,
                           Tr=answer6_complete$penalized,
                           X=covs,
                           M=1,
                           Weight=1,
                           estimand="ATE")
summary(nn.est1)
#7.2

nn.est2 <- Matching::Match(Y=answer6_complete$price,
                           Tr=answer6_complete$penalized,
                           X=covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")
summary(nn.est2)

#7.3
library(modelr)
logit.reg <- glm(penalized ~ quartile1+quartile2+quartile3,
                 data = answer6_complete, family = binomial(link = 'logit'))

answer6_complete<-add_predictions(answer6_complete,logit.reg,'ps',type='response')
answer6_complete<- answer6_complete %>%filter(ps>0 & ps<1)
# Create IPW weights
answer6_complete <- answer6_complete %>%
  mutate(ipw = case_when(
    penalized == 1 ~ 1/ps,
    penalized == 0 ~ 1/(1-ps)
  ))



mean.t1 <- answer6_complete %>% filter(penalized==1) %>% summarize(mean_y=weighted.mean(price, w=ipw))
mean.t0 <- answer6_complete %>% filter(penalized==0) %>% summarize(mean_y=weighted.mean(price, w=ipw))
mean.t1$mean_y - mean.t0$mean_y
reg.ipw <- lm(price ~ penalized, data=answer6_complete, weights=ipw)
reg.ipw
#7.4

reg <-  lm(price ~ penalized+quartile1  + quartile2 + quartile3, data=answer6_complete)
summary(reg)

tidyReg<-broom::tidy(reg)
tidyIPW<-broom::tidy(reg.ipw)
nnest1Estimates<-nn.est1$est
nnest1row<-c("penalized",nnest1Estimates,nn.est1$se,NA,NA)
nnest2Estimates<-nn.est2$est
nnest2row<-c("penalized",nnest2Estimates,nn.est2$se,NA,NA)
table7<-full_join(tidyReg,tidyIPW)
table7<-rbind(table7,nnest1row)
table7<-rbind(table7,nnest2row)
table7<-table7[-c(1,3,4,5,6),]
table7<-as.data.frame(table7)
rownames(table7)<-c("Linear Regression","Inverse Propensity Weighting","Nearest Neighbor (Inverse Variance)","Nearest Neighbor (Mahalanobis)")
readr::write_rds(table7,'data/output/table7.rds')
table7

