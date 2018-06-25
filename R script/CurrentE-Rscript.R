#
library(magrittr)
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
devtools::install_github("crsh/papaja")

#ReadFile
#whole data (see how many failed)

#load and select data
data<- read.csv("Data/CurrentE-RIP6-25-18.csv", stringsAsFactors = F,
                      na.strings = '')[-c(1:5), ]%>%
  dplyr::select(LabID, Video, dplyr::contains("AF"), dplyr::contains("AM"),dplyr::contains("AT"),dplyr::contains("sex"),age, exp) %>%
  dplyr::mutate_at(dplyr::vars(-LabID,-Video), dplyr::funs(as.numeric)) %>%
  dplyr::select(-dplyr::contains("Location"))
#  dplyr::mutate(Target.Gender=ifelse())
  #6/25/18need create variables saying MALE/FEMALE, Nervous/neutral/anxious, 04/12/ 14/ 27 NEED TO CONTROL FOR PERSON EFFECTS/ GENDER. Actually see differences in emotion variable. 
names(data)

#make data long
#Code for PhotoType: Gender, Emotion, Activation, Valence

long.data <- data %>%

  tidyr::gather(PhotoType, Estimate, AF.04.AFS.valence:AM.27.NES.activation)%>%
  tidyr::separate(PhotoType, c("Target.Gender", "Target.ID","Target.Emotion", "Dimension.Eval"))%>% #Split Columnname into factors
  dplyr::mutate_at(dplyr::vars(Video, dplyr::contains("Target"), Dimension.Eval),dplyr::funs(as.factor))

#foo <- long.data(do.call('rbind', strsplit(as.character(long.data$PhotoType),'.',fixed=TRUE)))

#NOTES FOR ME
##Target.Gender: AF is female; AM is male
##Target.ID: 04, 12, 14, 27
##Target.Emotion: AFS=; ANS=; NES=;
names(long.data)
head(long.data)

str(long.data)
#SUBSET Valence
valence.long.data<-long.data %>%
  dplyr::filter(Dimension.Eval == "valence")
str(valence.long.data)


#SUBSET ACTIVATION
activation.long.data<-long.data %>%
  dplyr::filter(Dimension.Eval == "activation")

#VALENCE ANALYSIS
lm1<-glm(Estimate~Target.Gender+Target.ID+Target.Emotion,data=valence.long.data)
summary(lm1)

#FACTOR LOAD IF THERE ARE NO DIFFERENCES? Or is that butchering data? Can I collapse - don't have to average to collapse?  
  dplyr::rowwise() %>%
  dplyr::mutate(mean.own=mean(c(Own.Sad.Feel, Own.Sad.Display), na.rm = T),
                mean.T=mean(c(AT_1,AT_2,CT_1,CT_2,ATT2), na.rm = T),
                mean.Diag=mean(c(Diagnosticity_11,Diagnosticity_12,Diagnosticity_13,Diagnosticity_14), na.rm = T),
                mean.Agree=mean(c(Agreeableness.BFI_1,Agreeableness.BFI_2,Agreeableness.BFI_3), na.rm = T)) %>%
  dplyr::mutate(Conditions.PartnerSad= ifelse(is.na(Partner.Neutral) , "Partner.Sad", "Partner.Neutral"))  




#maybe do median split and match? David, is there a way to do this? E.g. lm(mean.T~ I(mean.Diag < median(mean.Diag)) +mean.Agree,data=data)
lm<-lm(mean.T~ (Partner.Sad == "Sad") + mean.own+ mean.Diag+mean.Agree,data=data) #mean own sadness is not significantly different across conditions
summary(lm)


median(data$mean.own) #5

lm1<-lm(mean.T~mean.Diag+mean.Agree+Partner.Sad,data=data) # 
summary(lm1)

#make data long first (mean.own, magnitude)
data %>% 
  tidyr::gather(DV, magnitude, c("mean.own", "mean.Diag", "mean.Agree")) %>%
  dplyr::group_by(Partner.Sad,DV) %>%
  dplyr::summarize(mean = mean(magnitude),
                   sd=sd(magnitude,na.rm = T),
                   se = sd/sqrt(n()),
                   n=n()) %>% 
  ggplot2::ggplot(., ggplot2::aes(x = Partner.Sad, y = mean,fill=Partner.Sad)) + #fill=what variable you want to vary the fill by
  ggplot2::geom_bar(position = 'dodge', stat = 'identity') +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-se, ymax=mean+se))+
  ggplot2::facet_grid(~ DV) + #positioning of the boxes
  ggplot2::geom_text(ggplot2::aes(label=round(mean,2), y = 5, x = Partner.Sad), #how do I get the labels? David
                     position=ggplot2::position_dodge(width = 0.9),
                     size= 3) +
  ggplot2::geom_text(ggplot2::aes(label = paste("n =", n), y = 2, x = Partner.Sad), #added n text to the graph
                     position = ggplot2::position_dodge(width = 0.9),
                     size = 3, color = 'white') +
  papaja::theme_apa() 
