
library(ggplot2)
library(reshape2)
por_3=read.csv("08_04_pop2.csv")

por_3$day=as.Date(por_3$Day)
library(scales)

#fig1
por_long <- melt(por_3[,c(2:4,13,52)], id="day") 
colnames(por_long)[2]="trend"
colnames(por_long)[3]="search_intensity"

sp2 <- ggplot(data=por_long, aes(x=day, y=search_intensity)) +
  geom_line(aes(linetype=trend, color = trend), size = 1) +
  geom_point(aes(shape=trend, color=trend))+
  scale_x_date(labels = date_format("%d-%m-%y"))+
  theme_bw()+
  geom_vline(xintercept = as.numeric(por_3$day[50]), col="blue")+
  theme(legend.position="top")+
  theme(legend.text=element_text(size=rel(0.55)))
sp2


#fig2
por_long <- melt(por_3[,c(34:39,52)], id="day") 
colnames(por_long)[2]="trend"
colnames(por_long)[3]="search_intensity"

sp2 <- ggplot(data=por_long, aes(x=day, y=search_intensity)) +
  geom_line(aes(linetype=trend, color = trend), size = 1) +
  geom_point(aes(shape=trend, color=trend))+
  scale_x_date(labels = date_format("%d-%m-%y"))+
  theme_bw()+
  geom_vline(xintercept = as.numeric(por_3$day[50]), col="blue")+
  theme(legend.position="top")+
  theme(legend.text=element_text(size=rel(0.4)))
sp2

#fig3
por_long <- melt(por_3[,c(20:24,52)], id="day") 
colnames(por_long)[2]="trend"
colnames(por_long)[3]="search_intensity"

sp2 <- ggplot(data=por_long, aes(x=day, y=search_intensity)) +
  geom_line(aes(linetype=trend, color = trend), size = 1) +
  geom_point(aes(shape=trend, color=trend))+
  scale_x_date(labels = date_format("%d-%m-%y"))+
  theme_bw()+
  geom_vline(xintercept = as.numeric(por_3$day[50]), col="blue")+
  theme(legend.position="top")+
  theme(legend.text=element_text(size=rel(0.55)))
sp2

#fig4
por_long <- melt(por_3[,c(26:30,52)], id="day") 
colnames(por_long)[2]="trend"
colnames(por_long)[3]="search_intensity"

sp2 <- ggplot(data=por_long, aes(x=day, y=search_intensity)) +
  geom_line(aes(linetype=trend, color = trend), size = 1) +
  geom_point(aes(shape=trend, color=trend))+
  scale_x_date(labels = date_format("%d-%m-%y"))+
  theme_bw()+
  geom_vline(xintercept = as.numeric(por_3$day[50]), col="blue")+
  theme(legend.position="top")+
  theme(legend.text=element_text(size=rel(0.55)))
sp2


#fig5
por_long <- melt(por_3[,c(14:18,52)], id="day") 
colnames(por_long)[2]="trend"
colnames(por_long)[3]="search_intensity"

sp2 <- ggplot(data=por_long, aes(x=day, y=search_intensity)) +
  geom_line(aes(linetype=trend, color = trend), size = 1) +
  geom_point(aes(shape=trend, color=trend))+
  scale_x_date(labels = date_format("%d-%m-%y"))+
  theme_bw()+
  geom_vline(xintercept = as.numeric(por_3$day[50]), col="blue")+
  theme(legend.position="top")+
  theme(legend.text=element_text(size=rel(0.55)))
sp2


#fig6
por_long <- melt(por_3[,c(47:52)], id="day") 
colnames(por_long)[2]="trend"
colnames(por_long)[3]="search_intensity"

sp2 <- ggplot(data=por_long, aes(x=day, y=search_intensity)) +
  geom_line(aes(linetype=trend, color = trend), size = 1) +
  geom_point(aes(shape=trend, color=trend))+
  scale_x_date(labels = date_format("%d-%m-%y"))+
  theme_bw()+
  geom_vline(xintercept = as.numeric(por_3$day[50]), col="blue")+
  theme(legend.position="top")+
  theme(legend.text=element_text(size=rel(0.55)))
sp2


#fig7
colnames(por_3)[8:10]=c("wiki-SARS.CoV.2", "news-Koronawirus", "twitter-Koronawirus")
por_3$`news-Koronawirus*10`=por_3$`news-Koronawirus`*10
por_long <- melt(por_3[,c(8, 10,52,54)], id="day") 

colnames(por_long)[2]="trend"
colnames(por_long)[3]="No.page view/articles/twitts"

sp2 <- ggplot(data=por_long, aes(x=day, y=`No.page view/articles/twitts`)) +
  geom_line(aes(linetype=trend, color = trend), size = 1) +
  geom_point(aes(shape=trend, color=trend))+
  scale_x_date(labels = date_format("%d-%m-%y"))+
  theme_bw()+
  geom_vline(xintercept = as.numeric(por_3$day[50]), col="blue")+
  theme(legend.position="top")+
  theme(legend.text=element_text(size=rel(0.55)))
sp2

#fig8
por_long <- melt(por_3[,c(2:7,52)], id="day") 
colnames(por_long)[2]="trend"
colnames(por_long)[3]="normalized intensity"

sp2 <- ggplot(data=por_long, aes(x=day, y=`normalized intensity`)) +
  geom_line(aes(linetype=trend, color = trend), size = 1) +
  # geom_point(aes(shape=trend, color=trend))+
  scale_x_date(labels = date_format("%d-%m-%y"))+
  theme_bw()+
  geom_vline(xintercept = as.numeric(por_3$day[50]), col="blue")+
  theme(legend.position="top")+
  theme(legend.text=element_text(size=rel(1.9)))
sp2

#fig9
por_auto=por_3[c(17:60),]

ccfvalues = ccf(por_auto$`news-Koronawirus`,por_auto$twitter_koronawirus)

por_long <- melt(por_3[,c(34:39)], id="day") 
sp2 <- ggplot(data=por_long, aes(x=day, y=value, colour=variable)) + geom_line(aes(color = variable), size = 1) +
  
  geom_vline(xintercept = as.numeric(por_3$day[50]), col="blue")+
  ggtitle("google search intensity")+
  theme(plot.title = element_text(hjust = 0.5))
sp2

#fig10
data=por_3[,c(29,21,35,20,22,2:7)]
s <- c("antiviral mask, g", "washing hands, g","quarantine, g", "protective mask, g",  
       "hand disinfection, g", "coronavirus (search), g", "coronavirus (news), g", "coronavirus, y",
       "SARS.CoV.2, w", "coronavirus, e", "coronavirus, t")
colnames(data)=s
kor=cor(data, use="pairwise.complete.obs")
pheatmap(kor, colorRampPalette(rev(brewer.pal(n = 7, name ="Greys")))(100), number_color = "black", display_numbers = T)

