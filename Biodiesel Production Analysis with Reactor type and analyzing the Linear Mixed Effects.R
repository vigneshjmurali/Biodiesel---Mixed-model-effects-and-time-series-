attach(bio)
str(bio)
boxplot(`biodiesel load`~temperature*`reacter type`,bio)
#model
install.packages("lme4")
library(lme4)
biodiesel.model=lmer(`biodiesel load`~temperature+(1|batchnum)+
                        (1|scenario),data=bio)
summary(biodiesel.model)

biodiesel.model1=lmer(`biodiesel load`~temperature+`reacter type`+(1|batchnum)+
                         (1|scenario),data=bio)
summary(biodiesel.model1)
#to test likelihood ratio test (NULL model)
biodiesel.nullmodel1=lmer(`biodiesel load`~`reacter type`+(1|batchnum)+
                             (1|scenario),data=bio,REML=FALSE)
summary(biodiesel.nullmodel1)
#full model
biodiesel.fullmodel2=lmer(`biodiesel load`~`reacter type`+temperature+(1|batchnum)+
                             (1|scenario),data=bio,REML=FALSE)
summary(biodiesel.fullmodel2)
anova(biodiesel.nullmodel1,biodiesel.fullmodel2)
#chisquare value
chisq_val=-2*((logLik(biodiesel.fullmodel2)[1])-(logLik(biodiesel.nullmodel1)[1]))
chisq_val
chisq_df=6-5
chisq_df
pval=pchisq(chisq_val,chisq_df,lower.tail = TRUE)
pval
#or we can use this also to get p values
biodiesel.fullmodel3=lmer(`biodiesel load`~`reacter type`*temperature+(1|batchnum)+
                             (1|scenario),data=bio,REML=FALSE)
anova(biodiesel.fullmodel2,biodiesel.fullmodel3)
summary(biodiesel.fullmodel3)
#Random slopes
coef(biodiesel.model1)
biodiesel.anothermodel=lmer(`biodiesel load`~temperature+`reacter type`+(1+temperature|batchnum)+
                               (1+temperature|scenario),data=bio,REML=FALSE)
summary(biodiesel.anothermodel)
coef(biodiesel.anothermodel)

biodiesel.anothermodel1=lmer(`biodiesel load`~temperature+`reacter type`+(1|batchnum)+
                                (1|scenario),data=bio,REML=FALSE)
summary(biodiesel.anothermodel1)
coef(biodiesel.anothermodel1)
#comparison
biodiesel.anothermodel2=lmer(`biodiesel load`~`reacter type`+temperature+(1+temperature|batchnum)+
                                (1+temperature|scenario),data=bio,REML=FALSE)
summary(biodiesel.anothermodel2)
anova(biodiesel.anothermodel1,biodiesel.anothermodel2)
biodiesel.anothermodel3=lmer(`biodiesel load`~`reacter type`+temperature+(1+`reacter type`|batchnum)+
                               (1+`reacter type`|scenario),data=bio,REML=FALSE)

#
bio$scenario = factor(bio$scenario)
ggplot(bio, aes(x=scenario, y=`biodiesel load`,colour=scenario)) +
  geom_boxplot()
#
library(ggplot2)
theme_set(theme_bw(base_size = 18))

qplot(temperature, `biodiesel load`, facets = . ~ batchnum,colour = batchnum, geom = "boxplot", data = bio)

