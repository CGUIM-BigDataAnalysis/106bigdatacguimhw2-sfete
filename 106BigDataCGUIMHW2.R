library(readr)
library(dplyr)
library(ggplot2)
library(choroplethrMaps)
library(RColorBrewer)
abcCT103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv",skip = 0)
abcCT104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv",skip = 0)
abcCT105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv",skip = 0)
abcCT106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv",skip = 0)
abcCT103$Total103<-rowSums(abcCT103[3:11])
abcCT104$Total104<-rowSums(abcCT104[3:11])
abcCT105$Total105<-rowSums(abcCT105[3:11])
abcCT106$Total106<-rowSums(abcCT106[3:11])
abcCT103<-abcCT103[,c(2,12)]
abcCT104<-abcCT104[,c(2,12)]
abcCT105<-abcCT105[,c(2,12)]
abcCT106<-abcCT106[,c(2,12)]
abcCTtotal<-merge(abcCT103,abcCT104,by="國別",all=T)
abcCTtotal<-merge(abcCTtotal,abcCT105,by="國別",all=T)
abcCTtotal<-merge(abcCTtotal,abcCT106,by="國別",all=T)
abcCTtotal$Total103to106<-rowSums(abcCTtotal[2:5],na.rm = TRUE)

CountriesComparisionTable <- read_csv("D:/data/CountriesComparisionTable.csv",skip=0)
colnames(CountriesComparisionTable)<-c("iso_a3","English","國別")
CountriesComparisionTable<-CountriesComparisionTable[-267,]
abcCTtotal<-merge(abcCTtotal,CountriesComparisionTable,by="國別",all.x=T)

library(choroplethrMaps)
data(country.map)
final.plot<-merge(abcCTtotalcountry,
                  country.map,by="iso_a3",all.x=T)%>%
  group_by(group)%>%
  arrange(order)
abcCTmap<-ggplot() +
  geom_polygon(data = final.plot, 
               aes(x =long, y =lat, 
                   group = group, 
                   fill = Total103to106), 
               color = "black", 
               size = 0.25) + 
  coord_map()+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="ABC of X in Taiwan")
abcCTmap



abcCTtotal<-head(abcCTtotal[order(abcCTtotal$Total103to106,decreasing = T),],10)
abcCTtotal



library(ggplot2)
abcCTtotal<-merge(abcCT103,abcCT104,by="國別",all=T)
abcCTtotal<-merge(abcCTtotal,abcCT105,by="國別",all=T)
abcCTtotal<-merge(abcCTtotal,abcCT106,by="國別",all=T)
abcCTtotal$Total103to106<-rowSums(abcCTtotal[2:5],na.rm = TRUE)
abcCTtotal<-arrange(abcCTtotal,desc(Total103to106))
abcCTtop<-nrow(filter(abcCTtotal,Total103to106>500))
abcCTtotal<-group_by(abcCTtotal,`國別`) %>%
  tally(Total103to106, sort = TRUE) %>%
  group_by(`國別` = factor(c(`國別`[1:abcCTtop], rep("Other", n() - abcCTtop)),
                            levels = c(`國別`[1:abcCTtop], "Other"))) %>%
  tally(n) 
CTPlot<-ggplot()+geom_bar(data=abcCTtotal,
                  aes(x=國別,y=nn),
                  stat = "identity")+
        scale_y_sqrt()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1,size = 8))+
        labs(y="單位log(人)",title = "各國來台留學生長方圖")

CTPlot

library(ggplot2) 
library(rgdal)#for fortify()
library(rgeos) #for fortify()
library(maptools)

abcSCH103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv",skip = 0)
abcSCH104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv",skip = 0)
abcSCH105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv",skip = 0)
abcSCH106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv",skip = 0)
abcSCH103<-abcSCH103[-10]
abcSCH104<-abcSCH104[-10]
abcSCH105<-abcSCH105[-10]
abcSCH106<-abcSCH106[-10]
abcSCH103$Total103<-rowSums(abcSCH103[4:11])
abcSCH104$Total104<-rowSums(abcSCH104[4:11])
abcSCH105$Total105<-rowSums(abcSCH105[4:11])
abcSCH106$Total106<-rowSums(abcSCH106[4:11])
abcSCH103<-abcSCH103[,c(3,12)]
abcSCH104<-abcSCH104[,c(3,12)]
abcSCH105<-abcSCH105[,c(3,12)]
abcSCH106<-abcSCH106[,c(3,12)]
abcSCHtotal<-merge(abcSCH103,abcSCH104,by="學校名稱",all=T)
abcSCHtotal<-merge(abcSCHtotal,abcSCH105,by="學校名稱",all=T)
abcSCHtotal<-merge(abcSCHtotal,abcSCH106,by="學校名稱",all=T)
abcSCHtotal$Total103to106<-rowSums(abcSCHtotal[2:5],na.rm = TRUE)
abcSCHtotal<-head(abcSCHtotal[order(abcSCHtotal$Total103to106,decreasing = T),],10)
abcSCHtotal



Student_RPT<- read_csv("D:/data/Student_RPT_07.csv",skip =0)



library(readr)
library(dplyr)
Student_RPT_07 <- read_csv("D:/data/Student_RPT_07.csv",skip=2)
Student_RPT_07<-Student_RPT_07[,c(6,10,11,12,13)]
colnames(Student_RPT_07)<-c("學校名稱","對方學校國別","對方學校名稱","學校英文名稱","本國學生出國進修_交流人數")
Student_RPT_07$對方學校國別<-gsub("大陸地區","中國大陸",Student_RPT_07$對方學校國別)
Student_RPT_07$對方學校國別<-gsub("大韓民國(南韓)","南韓",Student_RPT_07$對方學校國別)
Student_RPT_07$對方學校國別<-gsub("德意志聯邦共和國","德國",Student_RPT_07$對方學校國別)

how_many_peopleCT<-group_by(Student_RPT_07,對方學校國別)%>% 
  summarise(TotalPeople = sum(`本國學生出國進修_交流人數`,na.rm = T))%>%
  arrange(desc(TotalPeople))

STtopgoCT<-head(how_many_peopleCT[order(how_many_peopleCT$TotalPeople,decreasing = T),],10)
STtopgoCT

how_many_peopleSCH<-group_by(Student_RPT_07,學校名稱)%>% 
  summarise(TotalPeople = sum(`本國學生出國進修_交流人數`,na.rm = T))%>%
  arrange(desc(TotalPeople))

STtopgo10SCH<-head(how_many_peopleSCH[order(how_many_peopleSCH$TotalPeople,decreasing = T),],10)
STtopgo10SCH

#how_many_peopleSCH<-how_many_peopleSCH[!grepl(NA,how_many_peopleSCH$對方學校名稱),]


library(ggplot2)

how_many_peopleCTn<-nrow(filter(how_many_peopleCT,TotalPeople>500))
how_many_peopleCT<-group_by(how_many_peopleCT,`對方學校國別`) %>%
  tally(TotalPeople, sort = TRUE) %>%
  group_by(`對方學校國別` = factor(c(`對方學校國別`[1:how_many_peopleCTn], rep("Other", n() - how_many_peopleCTn)),
                         levels = c(`對方學校國別`[1:how_many_peopleCTn], "Other"))) %>%
  tally(n) 

CTPlot<-ggplot()+geom_bar(data=how_many_peopleCT,
                          aes(x=對方學校國別,y=nn),
                          stat = "identity")+
  scale_y_sqrt()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1,size = 8))+
  labs(y="單位log(人)",title = "去各國家進修學生長方圖")

CTPlot



colnames(CountriesComparisionTable)<-c("iso_a3","English","對方學校國別")
how_many_peopleCT<-merge(CountriesComparisionTable,how_many_peopleCT,by="對方學校國別")

library(choroplethrMaps)
library(RColorBrewer)
library(ggplot2)
data(country.map)
final.plot2<-merge(how_many_peopleCT,
                   country.map,by="iso_a3",all.x=T)%>%
  group_by(group)%>%
  arrange(order)
stCTmap<-ggplot() +
  geom_polygon(data = final.plot2, 
               aes(x =long, y =lat, 
                   group = group, 
                   fill = TotalPeople), 
               color = "black", 
               size = 0.25) + 
  coord_map()+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="ABC of X in Taiwan")
stCTmap




people_out<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")
people_out<-people_out[,1:3]
people_top10_out<-head(arrange(people_out[,2:3],desc(總人數)),10)
  
colnames(CountriesComparisionTable)<-c("iso_a3","English","國別")
people_out<-merge(CountriesComparisionTable,people_out,by="國別")
data(country.map)
final.plot3<-merge(people_out,
                   country.map,by="iso_a3",all.x=T)%>%
  group_by(group)%>%
  arrange(order)
people_gomap<-ggplot() +
  geom_polygon(data = final.plot3, 
               aes(x =long, y =lat, 
                   group = group, 
                   fill = 總人數), 
               color = "black", 
               size = 0.25) + 
  coord_map()+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="ABC of X in Taiwan")
people_gomap


















