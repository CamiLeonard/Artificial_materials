library(readxl)
library(lme4)
library(car)
library(ade4)
library(lmtest)
library(MASS)
library(glmmTMB)
library(performance)
library(emmeans)
library(tidyr)
library(plyr)
library(dplyr)
library(multcomp)
library(multcompView)
library(DataCombine)
library(mvabund)
library(ggplot2)

data<-read_xlsx("Data/fulldata.xlsx")

##Table S1
#total recruit density
hist(data$Total,breaks=40)
mod1<-glmer(Total~Time*Material+(1|Brochette),data=data,family=nbinom2)
Anova(mod1)
plot(mod1)
qqnorm(residuals(mod1))
qqline(residuals(mod1))
check_overdispersion(mod1)

#survival
surv<-data %>%filter (Time!="6 months" & Gen1>0)
surv$survival<-surv$Surv1*100/surv$Gen1
surv$survival[surv$Time=="18 months"]<-surv$Surv1.2[surv$Time=="18 months"]*100/surv$Gen1[surv$Time=="18 months"]
hist(surv$survival,breaks=40)
mod1<-glmer(survival~Material*Time+Gen1+(1|Brochette),data=surv,family=poisson)
Anova(mod1)
plot(mod1)
qqnorm(residuals(mod1))
qqline(residuals(mod1))

#size
rec<-read_xlsx("Data/Allrecruits.xlsx")
rec$Polyps<-as.numeric(rec$Polyps)
rec$Sizetran<-sqrt(rec$Polyps)
mod1<-glmer(Sizetran~Material*time+(1|Brochette),data=rec,family=nbinom2)
Anova(mod1)
plot(mod1)
check_overdispersion(mod1)
qqnorm(residuals(mod1))
qqline(residuals(mod1))

#growth
growth<-read_xlsx("Data/growth.xlsx")
growth$gf<-growth$Polyps2/growth$Polyps1
mod1<-glmer(gf~Mat*survival+(1|Brochette),data=growth,family=nbinom2)
Anova(mod1)
plot(mod1)
qqnorm(residuals(mod1))
qqline(residuals(mod1))

##Fig 1
den<-ddply(data,c("Mat","Time"),summarise,den=mean(Total),CI=qnorm(0.975)*sd(Total)/sqrt(length(Total)))
den$Time<-factor(den$Time,c("6 months", "14 months", "18 months"))
den$Mat<-factor(den$Mat,c("Glass","Ceramic foam","Porous concrete","PLA","Aquaroche","FGP","3DS concrete","PVC","3DL concrete","Portland concrete"))
mycols<-c("#6db048","#5696ef","#fbcf43","#4bcebb","#5f3bb3","#37706f","#903d72","#dab960","#f47e7e","#e88433")

model<-data[data$Time=="6 months",]
mod1<-glmer(Total~Material+(1|Brochette),data=model,family=poisson)
emmeans(mod1,pairwise~Material)
model<-data[data$Time=="14 months",]
mod1<-glmer(Total~Material+(1|Brochette),data=model,family=poisson)
emmeans(mod1,pairwise~Material)
model<-data[data$Time=="18 months",]
mod1<-glmer(Total~Material+(1|Brochette),data=model,family=poisson)
emmeans(mod1,pairwise~Material)

den$letters<-"a"
den$letters[den$Time=="6 months"]<-c("ef","de","cd","b","cd","a","bc","bc","f","de")
den$letters[den$Time=="14 months"]<-c("de","bcd","abc","ab","abc","a","cde","abc","e","cde")
den$letters[den$Time=="18 months"]<-c("de","ef","ab","bc","cd","a","d","a","f","ef")


fig1<-ggplot(den, aes(x = Mat, y = den,fill=Mat)) +
  geom_bar(stat = "identity",position="stack") +
  facet_grid(rows = vars(Time),switch = "y")+
  theme(panel.background = element_rect(fill = "white"),axis.text.x = element_text(angle = 90))+
  xlab("Material")+
  ylab(expression(paste("Number of recruits per 100cm"^{2})))+
  geom_errorbar(aes(ymin = den-CI, ymax = den + CI), width = 0.2)+
  theme(legend.position = "none")+
  scale_fill_manual(values=alpha(mycols))+
  geom_text(aes(label = letters,y=22 ), colour="black", size=4)
fig1
ggsave("Fig1.pdf",fig1,width = 4, height = 6)
ggsave("Fig1.svg",fig1,width = 4, height = 6)

#Fig 2
surv$name<-"8-month survival"
surv$name[surv$Time=="18 months"]<-"12-month survival"
survfig<-ddply(surv,c("Mat","Time","name"),summarise,surv=mean(survival),CI=qnorm(0.975)*sd(survival)/sqrt(length(survival)))


model<-surv[surv$Time=="14 months",]
mod1<-glmer(Total~Mat+(1|Brochette),data=model,family=poisson)
emmeans(mod1,pairwise~Mat)
survfig$letters[survfig$Time=="14 months"]<-c("de","bc","cd","a","d","f","d","b","d","e")
model<-surv[surv$Time=="18 months",]
mod1<-glmer(Total~Mat+(1|Brochette),data=model,family=poisson)
emmeans(mod1,pairwise~Mat)
survfig$letters[survfig$Time=="18 months"]<-c("cd","b","bc","a","d","e","c","b","bc","c")

mycols2<-c("#5696ef","#fbcf43","#903d72","#5f3bb3","#e88433","#37706f","#4bcebb","#f47e7e","#dab960","#6db048")

survfig$name<-factor(survfig$name,c("8 months survival", "12 months survival"))
survfig$Mat<-factor(survfig$Mat,c("Ceramic foam","Porous concrete","3DS concrete","Aquaroche","Portland concrete","FGP","PLA","3DL concrete","PVC","Glass"))


fig2<-ggplot(survfig, aes(x = Mat, y = surv,fill=Mat)) +
  geom_bar(stat = "identity",position="stack") +
  facet_grid(rows = vars(name),switch = "y")+
  theme(panel.background = element_rect(fill = "white"),axis.text.x = element_text(angle = 90))+
  xlab("Material")+
  ylab("Recruit survival %")+
  geom_errorbar(aes(ymin = surv-CI, ymax = surv + CI), width = 0.2)+
  theme(legend.position = "none")+
  scale_fill_manual(values=alpha(mycols2))+
  geom_text(aes(label = letters,y=28 ), colour="black", size=4)
fig2
ggsave("Fig2.pdf",fig2,width = 4, height = 5)
ggsave("Fig2.svg",fig2,width = 4, height = 5)

##Fig 3
benthos<-data[,1:20]
benthos$OF<-rowSums(cbind(benthos$COR,benthos$BIV,benthos$OF))
benthos<-benthos[,-c(3,7)]
rowSums(cbind(benthos[,3:15]))
bentsum<-gather(benthos, "species", "points",c(3:15))
bentsum2<-ddply(bentsum,c("Time","Mat","species"),summarise,sum=mean(points))

bentcolors<-c("Bare substrate"="#3A9E97",
              "Bryozoa"="#6A74A3",
              "Dead CCA"="#E4E8F5",
              "Encrusting red algae"="#BA343DCC",
              "Foraminifera"="#EDC285",
              "Live CCA"="#F55839CC",
              "Other algae"="#8AB37C",
              "Other fauna"="#fcba03",
              "Lobophora"="#590F0DB3",
              "Sponge"="#E8DC36C3",
              "Wormtubes"="#747875",
              "Tunicate"="#f09ebc",
              "Turf algae"="#446938")

bentsum2$names<-bentsum2$species
names<-read_xlsx("Data/bennames.xlsx")
bentsum2<-FindReplace(data=bentsum2,Var="names",replaceData=names, from="species",to="new",exact=TRUE,vector=FALSE)
bentsum2$names<-factor(bentsum2$names,c("Other fauna","Tunicate","Wormtubes","Foraminifera",
                                        "Sponge","Bryozoa",
                                        "Bare substrate","Live CCA","Dead CCA",
                                        "Encrusting red algae", "Lobophora","Other algae","Turf algae"))
bentsum2$percent<-bentsum2$sum*100/49
bentsum2$Time<-factor(bentsum2$Time,c("6 months","14 months","18 months"))
bentsum2$Mat<-factor(bentsum2$Mat,c("Glass","Ceramic foam","Porous concrete","Aquaroche","PLA","FGP","PVC","3DS concrete","3DL concrete","Portland concrete"))


fig3<-ggplot(bentsum2, aes(x=Mat, y=percent, fill=names)) +
  geom_bar(stat="identity", width=0.9) +
  facet_grid(rows=vars(Time),switch="both")+
  coord_flip()+
  scale_x_discrete(position = "top")+
  scale_fill_manual(values=bentcolors)+
  labs(fill="Epibenthos classes", x=NULL,y="percent of surface")+
  theme(panel.background = element_rect(fill = "white"),axis.text.x = element_text(angle = 90))
fig3

ggsave("Fig3.pdf",fig3,width = 6, height = 7)
ggsave("Fig3.svg",fig3,width = 6, height = 7)

##Fig 4
data2<-data %>% filter(Time!="14 months")
longdata<-gather(data2,"species","points",c(3:17))
corden<-ddply(longdata,c("species","Time"),summarise,cor=cor(points,Total,method="spearman"),pval=cor.test(points,Total,method="spearman")$p.value)
corden$name<-"6-month recruitment"
corden$name[corden$Time=="18 months"]<-"18-month recruitment"
longsurv<-gather(surv,"species","points",c(3:17))
corsurv<-ddply(longsurv,c("species","Time","name"),summarise,cor=cor(points,survival,method="spearman"),pval=cor.test(points,survival,method="spearman")$p.value)
cortot<-rbind(corden,corsurv)
cortot<-FindReplace(data=cortot,Var="species",replaceData=names, from="species",to="new",exact=TRUE,vector=FALSE)
cortot$pval[cortot$pval<0.05]<-"*"
cortot$pval[cortot$pval>0.05]<-""
cortot$name<-factor(cortot$name,c("6-month recruitment","18-month recruitment","8-month survival","12-month survival"))
bentcolors2<-c("Bare substrate"="#3A9E97",
               "Bivalve"="#b780ed",
              "Bryozoa"="#6A74A3",
              "Coral recruit"="#ba8e5b",
              "Dead CCA"="#E4E8F5",
              "Encrusting red algae"="#BA343DCC",
              "Foraminifera"="#EDC285",
              "Live CCA"="#F55839CC",
              "Other algae"="#8AB37C",
              "Other fauna"="#fcba03",
              "Lobophora"="#590F0DB3",
              "Sponge"="#E8DC36C3",
              "Wormtubes"="#747875",
              "Tunicate"="#f09ebc",
              "Turf algae"="#446938")

fig4<-ggplot(na.omit(cortot))+
  geom_point(aes(x=cor,y=reorder(species, desc(species)),color=species),size=5)+
  scale_color_manual(values=bentcolors2)+
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",axis.title.y=element_blank(),
        axis.line.x = element_line(color="black"))+
  geom_vline(xintercept=0,linetype="dotted")+
  geom_text(aes(label = pval,x=cor,y=species),size=7)+
  xlab("Spearman's correlation")+
  geom_vline(xintercept=0,linetype="dotted")+
  scale_y_discrete(labels=rev(c("Bare substrate",
                                "Bivalve",
                                "Bryozoa",
                                "Coral recruit",
                                "Dead CCA",
                                "Encrusting red algae",
                                "Foraminifera",
                                "Live CCA",
                                expression(italic("Lobophora")),
                                "Other algae",
                                "Other fauna",
                                "Sponge",
                                "Tunicate",
                                "Turf algae",
                                "Wormtubes"))) +
  facet_grid(cols=vars(name))+
  xlim(-0.4,0.6)
fig4

ggsave("fig4.pdf",fig4,width = 8, height = 5)
ggsave("fig4.svg",fig4,width = 8, height = 5)

##Table S2
bentsp<-mvabund(benthos[,3:15])
mod1 <- manyglm(bentsp ~ benthos$Mat*benthos$Time, family = "negative_binomial")
anova(mod1)

##Table S3
model<-benthos %>% filter(Time=="6 months")
bentsp<-mvabund(model[,3:15])
mod1 <- manyglm(bentsp ~ model$Mat, family = "negative_binomial")
anova<-anova(mod1, pairwise.comp = model$Mat)
aov<-as.data.frame(anova$pairwise.comp.table)
aov$comparison<-row.names(aov)
aov$Time<-"6 months"
write_xlsx(aov,"diff_mat_6mo.xlsx")

model<-benthos %>% filter(Time=="14 months")
bentsp<-mvabund(model[,3:15])
mod1 <- manyglm(bentsp ~ model$Mat, family = "negative_binomial")
anova<-anova(mod1, pairwise.comp = model$Mat)
aov<-as.data.frame(anova$pairwise.comp.table)
aov$comparison<-row.names(aov)
aov$Time<-"14 months"
write_xlsx(aov,"diff_mat_14mo.xlsx")

model<-benthos %>% filter(Time=="18 months")
bentsp<-mvabund(model[,3:15])
mod1 <- manyglm(bentsp ~ model$Mat, family = "negative_binomial")
anova<-anova(mod1, pairwise.comp = model$Mat)
aov<-as.data.frame(anova$pairwise.comp.table)
aov$comparison<-row.names(aov)
aov$Time<-"18 months"
write_xlsx(aov,"diff_mat_18mo.xlsx")


##Fig S1
allsize<-ddply(rec,c("Mat","Time"),summarise,size=mean(Polyps),CI=qnorm(0.975)*sd(Polyps)/sqrt(length(Polyps)))
model<-rec %>% filter(Time=="6 months")
mod1<-glmer(Sizetran~Mat+(1|Brochette),data=model,family=nbinom2)
Anova(mod1)
emmeans(mod1,pairwise~Mat)
allsize$letters<-""
allsize$letters[allsize$Time=="6 months"]<-c("bc","ab","ab","a","cde","e","e","bcd","cde","de")
model<-rec %>% filter(Time=="14 months")
mod1<-glmer(Sizetran~Mat+(1|Brochette),data=model,family=nbinom2)
Anova(mod1)
emmeans(mod1,pairwise~Mat)
allsize$letters[allsize$Time=="14 months"]<-c("bc","bc","ab","a","c","c","c","bc","c","c")
model<-rec %>% filter(Time=="18 months")
mod1<-glmer(Sizetran~Mat+(1|Brochette),data=model,family=nbinom2)
Anova(mod1)
emmeans(mod1,pairwise~Mat)
allsize$letters[allsize$Time=="18 months"]<-c("bc","ab","bc","a","abc","bc","c","bc","c","c")

allsize$Time<-factor(allsize$Time,c("6 months", "14 months", "18 months"))
allsize$Mat<-factor(allsize$Mat,c("Ceramic foam","Aquaroche","Glass","3DS concrete","FGP","PLA","3DL concrete","Porous concrete","PVC","Portland concrete"))
mycols<-c('Porous concrete'="#fbcf43",
          'Ceramic foam'="#5696ef",
          Aquaroche="#5f3bb3",
          '3DS concrete'="#903d72",
          PLA="#4bcebb",
          FGP="#37706f",
          '3DL concrete'="#f47e7e",
          PVC="#dab960",
          'Portland concrete'="#e88433",
          Glass="#6db048")

figsize<-ggplot(allsize, aes(x = Mat, y = size,fill=Mat)) +
  geom_bar(stat = "identity",position="stack") +
  theme(panel.background = element_rect(fill = "white"),axis.text.x = element_text(angle = 90))+
  facet_grid(rows = vars(Time),switch = "y")+
  xlab("Material")+
  ylab("Recruit size (polyps)")+
  geom_errorbar(aes(ymin = size-CI, ymax = size + CI), width = 0.2)+
  theme(legend.position = "none")+
  scale_fill_manual(values=mycols)+
  geom_text(aes(label = letters,y=33), colour="black", size=4)
figsize

ggsave("Size.pdf",figsize,width = 3.5, height = 7)
ggsave("Size.svg",figsize,width = 3.5, height = 7)

##Fig S2
model<-growth %>% filter(survival=="8 months")
mod1<-glmer(gf~Mat+(1|Brochette),data=model,family=nbinom2)
emmeans(mod1,pairwise~Mat)
#no significant differences
model<-growth %>% filter(survival=="12 months")
mod1<-glmer(gf~Mat+(1|Brochette),data=model,family=nbinom2)
emmeans(mod1,pairwise~Mat)

grow<-ddply(model,c("Mat"),summarise,growth=mean(gf),CI=qnorm(0.975)*sd(gf)/sqrt(length(gf)))
grow$letters<-c("ab","b","ab","ab","ab","ab","a","ab","ab","b")

growthfig<-ggplot(grow, aes(x = reorder(Mat,growth), y = growth,fill=Mat)) +
  geom_bar(stat = "identity") +
  theme(panel.background = element_rect(fill = "white"),axis.text.x = element_text(angle = 90))+
  xlab("Material")+
  ylab("12-month growth factor \n (final/initial size)")+
  geom_errorbar(aes(ymin = growth-CI, ymax = growth + CI), width = 0.2)+
  theme(legend.position = "none")+
  scale_fill_manual(values=alpha(mycols))+
  geom_text(aes(label = letters,y=42), colour="black", size=4)
growthfig

ggsave("growthfig.pdf",growthfig,width = 3.5, height = 3.5)
ggsave("growthfig.svg",growthfig,width = 3.5, height = 3.5)

##Fig S3
surv2<-spread(surv[,c(1,21,30,31)],name,survival)
cor.test(surv2$Gen1,surv2$`8-month survival`,method="spearman")
cor.test(surv2$Gen1,surv2$`12-month survival`,method="spearman")

surv2<-surv[,c(1,21,30,31)]
cor<-ggplot(surv2,aes(x=Gen1,y=survival,color=name))+
  geom_jitter()+
  theme_bw()+
  ylab("Survival %")+
  xlab("Initial recruit density")+
  scale_color_discrete(name = "Survey")+
  theme(legend.position = c(0.7, 0.8))+
  geom_smooth(method=glm,formula=(y~log(x)),
              colour="black")
cor
ggsave("CorSurv.jpeg",cor,width = 4, height = 3)

##Fig S5
bentsum$percent<-bentsum$points*100/49
bentsum2<-ddply(bentsum,c("Time","species"),summarise,perc=mean(percent),ci=qnorm(0.975)*sd(percent)/sqrt(length(percent)))
bentsum2$names<-bentsum2$species
bentsum2<-FindReplace(data=bentsum2,Var="names",replaceData=names, from="species",to="new",exact=TRUE,vector=FALSE)
bentsum2$Time<-factor(bentsum2$Time,c("6 months","14 months","18 months"))


bentev<-ggplot(bentsum2, aes(x = Time, y = perc,
                             col=names,group=names))+
  geom_point(size = 4)+
  geom_line(lwd=0.9)+
  ylab("% benthic cover")+
  geom_errorbar(aes(ymin=perc-ci, ymax=perc+ci),width = 0.1)+
  scale_color_manual(values=bentcolors)+
  theme(panel.background = element_rect(fill = "white"),axis.title.x = element_blank())
bentev
ggsave("bentevolution.pdf",bentev,width = 5, height = 7)
ggsave("bentevolution.svg",bentev,width = 5, height = 7)
