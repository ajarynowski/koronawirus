
lay2 <- layout_with_fr(twi_maly1)

louvain <- cluster_louvain(as.undirected(twi_maly1),weights = E(twi_maly1)$weight)
memb <- membership(louvain)

pdf("usun6.pdf")
plot(twi_maly1,vertex.label=V(twi_maly1)$name3, vertex.size=(log(central$w_deg)+ central$w2/5)/10, vertex.label.cex=0.1,  edge.arrow.size=0, edge.width=E(twi_maly1)$weight/3, layout=lay2, vertex.color=memb, vertex.frame.color=memb, rescale=TRUE)
dev.off()
V(twi_maly1)$name3=V(twi_maly1)$name2
V(twi_maly1)$name3[which(central$w_deg<100)]=NA


central=data.frame(id=numeric(8391))
central_w_deg=strength(twi_maly1, weights = E(twi_maly1)$weight)
central_w_deg_in=strength(twi_maly1, mode="in", weights = E(twi_maly1)$weight)


central$w_deg=central_w_deg
central$id=V(twi_maly1)$name2
central$w2=central_w_deg_in
ce<-central[order(central$w_deg, decreasing = TRUE),]



hist(central_w_deg_in)

central2=data.frame(id=numeric(5778))
central_w2_deg=strength(twi, weights = E(twi)$weight)
central2$w_deg=central_w2_deg
central2$id=V(twi)$name2


#por=read.csv("all_sources_en.csv")
por$day=as.Date(por$Day)

ok <- ! is.na(por$g_kor)
plot (g_kor~day, por,  type = "l")
abline(v=as.numeric(por$day[64]), col="blue")

plot(por$day, por$Koronawirus_Google, por=ok)







sp <- ggplot(data=por, aes(x=day, y=g_kor)) + geom_line()+
  geom_vline(xintercept = as.numeric(por$day[64]), col="blue")+
  ggtitle("google search intensity")+
  theme(plot.title = element_text(hjust = 0.5))
por1=por[c(21:71),]
por2=por[c(25:71),]

por2=por[c(31:69),]

#The most dominant cross correlations 

ccfvalues = ccf(por2$event_registry,por2$t_kor)


sp_ev <- ggplot(data=por2, aes(x=day, y=event_registry)) + geom_line()+
  geom_vline(xintercept = as.numeric(por$day[64]), col="blue")+
  ggtitle("No. aricles in Event Registry")+
  theme(plot.title = element_text(hjust = 0.5))
sp_ev

por4=por[c(28:71),]

sp <- ggplot(data=por4, aes(x=day, y=t_kor)) + geom_line()+
  geom_vline(xintercept = as.numeric(por$day[64]), col="blue")+
  ggtitle("No. tweets daily with #Koronawirus")+
  theme(plot.title = element_text(hjust = 0.5))


sp_y <- ggplot(data=por, aes(x=day, y=y_kor)) + geom_line()+
  geom_vline(xintercept = as.numeric(por$day[64]), col="blue")+
  ggtitle("Youtube search intensity")+
  theme(plot.title = element_text(hjust = 0.5))
sp_y
colnames(por)[6]="google_queries"
por_long_goole <- melt(por[,c(6,14,15)], id="day") 
sp_google <- ggplot(data=por_long_goole, aes(x=day, y=value, colour=variable)) + geom_line(aes(color = variable), size = 1) +
  geom_vline(xintercept = as.numeric(por$day[64]), col="blue")+
  ggtitle("google search intensity")+
  theme(plot.title = element_text(hjust = 0.5))
sp_google

dev.off()
library("reshape2")

por_long <- melt(por[,c(1:5)], id="day") 
sp2 <- ggplot(data=por, aes(x=day)) + geom_line(aes(y=g_k, colour=0))+
  geom_line(aes(y=g_mr, colour=1))+
  geom_line(aes(y=g_m,  colour=2))+
  geom_line(aes(y=g_d, colour=3))+
  legend('topright', legend =c(1,2,3,4), col=c(1,2,3,4))+
  geom_vline(xintercept = as.numeric(por$day[64]), col="blue")+
  ggtitle("google search intensity")+
  theme(plot.title = element_text(hjust = 0.5))
colnames(por)[7]="Youtube_queries"
por_long_all <- melt(por[c(15:71),c(6:7,11,13,16:17)], id="day") 
por_long <- melt(por[,c(2:5,15)], id="day") 
colnames(por)[8:9]=c("SARS-CoV-2","Spread_of_SARS_CoV-2" )
colnames(por)[2:5]=c("hand washing", 'quarantine', "protective mask", "hand disinfection")
sp2 <- ggplot(data=por_long, aes(x=day, y=value, colour=variable)) + geom_line(aes(color = variable), size = 1) +
  
  geom_vline(xintercept = as.numeric(por$day[64]), col="blue")+
  ggtitle("google search intensity")+
  theme(plot.title = element_text(hjust = 0.5))
por_long2 <- melt(por[c(41:71),c(8:9,15)], id="day") 

ccfvalues = ccf(por2$event_registry_n,por2$y_kor)

cor_polo=rcorr(as.matrix(por[,c(2:7,11,13,15)]))

M <- cor_polo$r
p_mat <- cor_polo$P
corrplot(M, order = "hclust", p.mat = p_mat, sig.level = 0.01, insig = "blank")


por_long_all <- melt(por[c(15:71),c(6:7,11,13,16:17)], id="day") 

sp_all <- ggplot(data=por_long_all, aes(x=day, y=value, colour=variable)) + geom_line(aes(color = variable), size = 1) +
  geom_vline(xintercept = as.numeric(por$day[64]), col="blue")+
  ggtitle("normalized intensity on platforms")+
  theme(plot.title = element_text(hjust = 0.5))
sp_all
sp3 <- ggplot(data=por_long2, aes(x=day, y=value, colour=variable)) + geom_line(aes(color = variable), size = 1) +
  geom_vline(xintercept = as.numeric(por$day[64]), col="blue")+
  ggtitle("Page visits on Wikipedia")+
  theme(plot.title = element_text(hjust = 0.5))
sp3