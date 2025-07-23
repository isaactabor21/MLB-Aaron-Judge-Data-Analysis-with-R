
J<-read.csv("JudgeInplay.csv")

library(caret)
library(car)
library(RColorBrewer)

library(dplyr)
library(ggwordcloud)

Judge=J%>%select(pitch_name,release_speed,p_throws,events,bb_type,launch_speed,hit_distance_sc,launch_angle)

Judge<-Judge%>%rename(Pitch_Name=pitch_name,Pitch_Speed=release_speed,Pitcher_Handedness=p_throws,Result_of_Play=events,Quality_of_Contact=bb_type,Exit_Velocity=launch_speed,Distance=hit_distance_sc,Launch_Angle=launch_angle)

Judge%>%summarize(Avg_Pitch_Speed=mean(Pitch_Speed),Avg_Exit_Velocity=mean(Exit_Velocity),Avg_Distance=mean(Distance),Avg_Launch_Angle=mean(Launch_Angle),SD_Pitch_Speed=sd(Pitch_Speed),SD_Exit_Velocity=sd(Exit_Velocity),SD_Distance=sd(Distance),SD_Launch_Angle=sd(Launch_Angle))


table(Judge$Pitcher_Handedness)

table(Judge$Pitch_Name)
table(Judge$Quality_of_Contact)
table(Judge$Result_of_Play)

table(Judge$Pitch_Name,Judge$Result_of_Play)
table(Judge$Pitch_Name,Judge$Quality_of_Contact)

table(Judge$Pitcher_Handedness,Judge$Result_of_Play)
table(Judge$Pitcher_Handedness,Judge$Quality_of_Contact)

Judge%>%group_by(Pitch_Name)%>%summarize(Avg_Pitch_Speed=mean(Pitch_Speed),Avg_Exit_Velocity=mean(Exit_Velocity),Avg_Distance=mean(Distance),Avg_Launch_Angle=mean(Launch_Angle))
Judge%>%group_by(Pitcher_Handedness)%>%summarize(Avg_Pitch_Speed=mean(Pitch_Speed),Avg_Exit_Velocity=mean(Exit_Velocity),Avg_Distance=mean(Distance),Avg_Launch_Angle=mean(Launch_Angle))

ggplot(Judge,aes(x=Result_of_Play,fill=Result_of_Play))+geom_bar()+scale_fill_manual(values=c("green3","red3","peachpuff","red3","red3","red3","red3","green3","green3"))+labs(x="Result of Play")+theme_classic()+geom_text(aes(label=..count..),stat='count')

ggplot(Judge,aes(y=Exit_Velocity))+geom_boxplot(fill="navyblue",outlier.color="red")+coord_flip()+theme_classic()+labs(y="Exit Velocity")

ggplot(Judge,aes(x=Pitcher_Handedness,y=Exit_Velocity))+geom_boxplot()+geom_boxplot(fill="navyblue",color="black",outlier.color = "red")+labs(x="Pitcher Handedness", y="Exit Velocity", title = "Pitcher Handedness vs. Exit Velocity")+theme(plot.title = element_text(hjust = 0.5))

ggplot(Judge,aes(x=Launch_Angle,y=Distance))+geom_point(color="navyblue")+labs(x="Launch Angle (degrees)",y="Distance (ft)")+theme_classic()

ggplot(Judge,aes(x=Exit_Velocity,y= Launch_Angle ,size= Distance, fill ="blue"))+geom_point(alpha=0.3, shape = 21,color="black")+scale_size(range=c(0.1,13))+labs(x="Exit Velocity",y="Launch Angle")+theme_classic()

New<-Judge%>%group_by(Result_of_Play)%>%summarize(Freq=n())

ggplot(New,aes(label=Result_of_Play,size=Freq,color=Result_of_Play))+geom_text_wordcloud_area()+scale_fill_brewer(palette="Set1")

model<-train(Distance~Exit_Velocity+Launch_Angle,Judge,method="lm")
summary(model)

model$finalModel
vif(model$finalModel)
model2<-train(Distance~Exit_Velocity+Launch_Angle,Judge,method="lmStepAIC",trace=FALSE)
new_values<-data.frame(Exit_Velocity=100,Launch_Angle=40)
predict(model2,new_values)

Judge2=Judge%>%mutate(hit=ifelse(Exit_Velocity<90,"not hard hit","hard hit"))
JudLog=train(hit~Pitch_Speed+Launch_Angle,Judge2,method="glm")
summary(JudLog)
JudLog$finalModel
vif(JudLog$finalModel)
logit_model2<-train(hit~Pitch_Speed+Launch_Angle,Judge2,method="glmStepAIC",family="binomial",trace=FALSE)
logit_model2$finalModel

exp(coef(logit_model2$finalModel))

knn_model2<-train(Result_of_Play~Quality_of_Contact+Exit_Velocity+Distance+Launch_Angle,Judge,method="knn")

knn_model2$finalModel

ggplot(knn_model2)

New_Hit<-data.frame(Quality_of_Contact= "line_drive", Exit_Velocity=95,Distance=300,Launch_Angle=25,Judge,method="knn")

predict(knn_model2,New_Hit)