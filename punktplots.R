my_data <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)

library(tidyverse)



my_data
typeof(my_data)
is.data.frame(my_data)

str(my_data)
head(my_data)
tail(my_data)
library(ggplot2)
install.packages("remotes")
remotes::install_github("coolbutuseless/ggpattern")
1

my_data$deaths_prop_if_infected <- (my_data$tot_deaths/my_data$infected)

my_data$deaths_prop <- my_data$tot_deaths/my_data$tot_num

my_data$infected_prop <- my_data$infected/my_data$tot_num

my_data$death_comor_prob <- my_data$comorbidity/my_data$tot_deaths

my_data$positive_prob <- my_data$infected/my_data$tested

my_data$deaths_prop_if_infected_male <- my_data$male_deaths/my_data$infected_male

my_data$deaths_prop_if_infected_fem <- my_data$female_deaths/my_data$infected_fem
theme_bar_deaths <- theme_bw()+theme(panel.grid.major = element_blank(), 
                              panel.grid.major.y = element_line(linetype= "dotted", colour = "black"),
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(),
                              axis.title.y = element_text(angle = 0),
                              axis.text=element_text(size=20,colour = "black"),
                              axis.title=element_text(size=35, colour = "black")
                              )
theme_bar_gender <- theme_bw()+theme(panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     axis.title.y = element_text(angle = 0),
                                     panel.background = element_blank(),
                                     axis.text=element_text(size=20,colour = "black"),
                                     axis.title=element_text(size=35, colour = "black")
                                     )

theme_bar_gender2 <- theme_bw()+theme(panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(),
                                      axis.title.y = element_text(angle = 0, size = 35, colour = "black"),
                                      panel.background = element_blank(),
                                      axis.text=element_text(size=17,colour = "black"),
                                      axis.title.x = element_text(size = 35, colour = "black"),
                                      legend.title = element_text(size = 22),
                                      legend.text = element_text(size = 22),
                                      legend.position = "top"
                                      )



#Antallet af smittede: Barplot
p <- ggplot(data=my_data[1:10,], aes(x=age, y=infected)) +
  geom_bar(stat="identity", color="black", fill="#10244c") + xlab("Aldersinterval") + ylab("Antallet af smittede")
p
#Antallet af døde Barplot
ggplot(data=my_data[1:10,], aes(x=age, y=tot_deaths)) +
  geom_bar(stat="identity", color="black", fill="white") + xlab("Aldersinterval") + ylab("\n\n\n\n\nAntal\naf døde") + theme_bar_deaths



#Dødeligheden hvis du er smittet
ggplot(my_data[1:10,], aes(x=age, y=deaths_prop_if_infected)) +
  geom_point(size=3) + xlab("Aldersinterval") + ylab("Dødelighed hvis smittet") + geom_hline(yintercept=my_data$deaths_prop_if_infected[11], linetype="dashed", color = "black") + 
  annotate("text", min(my_data$age), 0.01, vjust = 1, hjust= -8, label = "Gennemsnit")

my_data$deaths_prop_if_infected[11]

#Dødeligheden givet man er smittet Barplot
ggplot(data=my_data[1:10,], aes(x=age, y=deaths_prop_if_infected)) +
  geom_bar(stat="identity", color="black", fill="white") + xlab("Aldersinterval") + ylab("\n\n\n\n\n\n% Døde") + scale_y_continuous(labels = scales::percent)+ theme_bar_deaths


#Sandsynligheden for at blive smittet
ggplot(my_data[1:10,], aes(x=age, y=infected_prop)) +
  geom_point(size=3) + xlab("Aldersinterval") + ylab("Sandsynligheden for at blive smittet")+geom_hline(yintercept=my_data$infected_prop[11], linetype="dashed", color = "black") + 
  annotate("text", min(my_data$age), 0.045, vjust = 9.5, hjust= -8, label = "Gennemsnit")


#Sandsynligheden for at blive testet positiv
ggplot(my_data[1:10,], aes(x=age, y=positive_prob)) +
  geom_point(size=3) + xlab("Aldersinterval") + ylab("Sandsynligheden for at blive testet positiv")+geom_hline(yintercept=my_data$positive_prob[11], linetype="dashed", color = "black") + 
  annotate("text", min(my_data$age), 0.045, vjust = -2, hjust= -8, label = "Gennemsnit")


#Sandsynligheden for komorbiditet blandt de døde.
ggplot(my_data[1:10,], aes(x=age, y=death_comor_prob)) +
  geom_point(size=3) + xlab("Aldersinterval") + ylab("Sandsynligheden for komorbiditet givet personen er død")+geom_hline(yintercept=my_data$death_comor_prob[11], linetype="dashed", color = "black") + 
  annotate("text", min(my_data$age), 0.80, vjust = -1, hjust= -8, label = "Gennemsnit")

#Søjlediagram over døde delt op i køn og alder
df2 <- data.frame(Køn=rep(c("Mand", "Kvinde"), each=10),
                  Alder=my_data$age[1:10],
                  Døde=c(my_data$male_deaths[1:10], my_data$female_deaths[1:10]))

ggplot(data=df2, aes(x=Alder, y=Døde, fill=Køn)) +
  geom_bar(stat="identity", position=position_dodge())

#Søjlediagram delt op i køn
df3 <- data.frame(Køn=rep(c("Mænd", "Kvinder"), each=1),
                  Døde=c(my_data$male_deaths[11], my_data$female_deaths[11]))

ggplot(data=df3, aes(x=Køn, y=Døde, fill=køn)) +
  geom_bar(stat="identity", position=position_dodge())

ggplot(data=df3, aes(x=Køn, y=Døde)) +
  geom_bar(stat="identity", color="black", fill="white", width = 0.4) + xlab("") + ylab("\n\n\n\n\n\nAntal\naf døde")+ scale_y_continuous(breaks = seq(0,1250,by = 250))+theme_bar_gender




#Aldersgrupper samlet søjlediagram 0-59, 60-90+
head(my_data)
df4 <- data.frame(Alder=c("0-59","60-90+"), Døde = c(sum(my_data$tot_deaths[1:6]), sum(my_data$tot_deaths[7:10])))
head(df4)
ggplot(data=df4, aes(x=Alder, y=Døde)) +
  geom_bar(stat="identity", color="black", fill="#10244c")

#Aldersgrupper samlet søjlediagram 0-69, 70-90+
head(my_data)
df5 <- data.frame(Alder=c("0-69","70-90+"), Døde = c(sum(my_data$tot_deaths[1:7]), sum(my_data$tot_deaths[8:10])))
head(df4)
ggplot(data=df5, aes(x=Alder, y=Døde)) +
  geom_bar(stat="identity", color="black", fill="#10244c")

#Søljediagram med proportionen af døde givet du er smittet delt op i køn
df6 <- data.frame(Køn=rep(c("Mænd", "Kvinder"), each=1),
                  Døde=c(my_data$deaths_prop_if_infected_male[11], my_data$deaths_prop_if_infected_fem[11]))

ggplot(data=df6, aes(x=Køn, y=Døde)) +
  geom_bar(stat="identity", color="black", fill="white", width = 0.4)+ xlab("") + ylab("\n\n\n\n\n\n% Døde")+ scale_y_continuous(labels = scales::percent) + theme_bar_gender


#Søjlediagram over smittede delt op i køn og alder
df7 <- data.frame(Køn=rep(c("Mænd", "Kvinder"), each=10),
                  Alder=my_data$age[1:10],
                  Smittede=c(my_data$infected_male[1:10], my_data$infected_fem[1:10]))

ggplot(data=df7, aes(x=Alder, y=Smittede, fill=Køn)) +
  geom_bar(stat="identity", position=position_dodge(),color = "black")+ xlab("Aldersinterval") + ylab("\n\n\n\n\n\nSmittede")+theme_bar_gender2+scale_fill_manual("Køn:", values = c("Kvinder" = "grey", "Mænd" = "white"))




















