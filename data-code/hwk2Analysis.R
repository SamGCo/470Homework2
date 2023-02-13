finalData=read_rds('data/output/HCRIS_Data.rds')
#number 1
answer1<-final.hcris %>%group_by(provider_number,fyear)%>%
summarize(count=n())
filtered1<-answer1%>%
  filter(count>1)

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
options(repr.plot.width=15, repr.plot.height=8)
answer3<- final.hcris.data %>% 
  group_by(year)%>%
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
  group_by(year)%>%
  ggplot(aes(x = as.factor(year), y = price)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9)
answer4Graph

hw2figure3<-ggsave(filename="violinPlotPrices.png",answer3,height=8,width=10)

# number 5

new2012Final<-subset(answer4,year==2012)
new2012Final["hvbp_payment"][is.na(new2012Final["hvbp_payment"])] <- 0
new2012Final["hrrp_payment"][is.na(new2012Final["hrrp_payment"])] <- 0
new2012Final<-mutate(new2012Final,penalized=case_when(hvbp_payment+hrrp_payment<0~1,hvbp_payment+hrrp_payment>=0~0))
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

write_rds(answer6,'data/output/answer6.rds')
table6<-answer6 %>% group_by(group,quartile1, quartile2, quartile3, quartile4) %>% summarize(price_mean=mean(price,na.rm=TRUE))
table6
# start of 7.1
answer6_complete <- answer6[complete.cases(answer6), ]
logit.reg <- glmnn.est1 <- Matching::Match(Y=answer6_complete$price,
                           Tr=answer6_complete$penalized,
                           X=answer6_complete$beds,
                           M=1,
                           Weight=1,
                           estimand="ATE")
summary(nn.est1)
#7.2

nn.est2 <- Matching::Match(Y=answer6_complete$price,
                           Tr=answer6_complete$penalized,
                           X=answer6_complete$beds,
                           M=1,
                           Weight=2,
                           estimand="ATE")
summary(nn.est2)

#7.3
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

tidyReg<-tidy(reg)
tidyIPW<-tidy(reg.ipw)
nnest1Estimates<-nn.est1$est
nnest1row<-c("penalized",nnest1Estimates,nn.est1$se,NA,NA)
nnest2Estimates<-nn.est2$est
nnest2row<-c("penalized",nnest2Estimates,nn.est2$se,NA,NA)
table7<-full_join(tidyReg,tidyIPW)
table7<-rbind(table7,nnest1row)
table7<-rbind(table7,nnest2row)
write_rds(table7,'data/output/table7.rds')
