106-2 大數據分析方法 作業二
================
吳振凱 B0322037

作業完整說明[連結](https://docs.google.com/document/d/1aLGSsGXhgOVgwzSg9JdaNz2qGPQJSoupDAQownkGf_I/edit?usp=sharing)

學習再也不限定在自己出生的國家，台灣每年有許多學生選擇就讀國外的大專院校，同時也有人多國外的學生來台灣就讀，透過分析[大專校院境外學生人數統計](https://data.gov.tw/dataset/6289)、[大專校院本國學生出國進修交流數](https://data.gov.tw/dataset/24730)、[世界各主要國家之我國留學生人數統計表](https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv)可以了解103年以後各大專院校國際交流的情形。請同學分析以下議題，並以視覺化的方式呈現分析結果，呈現103年以後大專院校國際交流的情形。

來台境外生分析
--------------

### 資料匯入與處理

``` r
library(readr)
library(dplyr)
library(ggplot2)
library(choroplethrMaps)
library(RColorBrewer)
#載入來台念書外國人資料(CT代表Country)
abcCT103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv",skip = 0)
abcCT104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv",skip = 0)
abcCT105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv",skip = 0)
abcCT106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv",skip = 0)
#將每一ROW各自加總，並取出需要的數據做子集
abcCT103$Total103<-rowSums(abcCT103[3:11])
abcCT104$Total104<-rowSums(abcCT104[3:11])
abcCT105$Total105<-rowSums(abcCT105[3:11])
abcCT106$Total106<-rowSums(abcCT106[3:11])
abcCT103<-abcCT103[,c(2,12)]
abcCT104<-abcCT104[,c(2,12)]
abcCT105<-abcCT105[,c(2,12)]
abcCT106<-abcCT106[,c(2,12)]
#載入來台念書外國人資料(SCH代表School)
abcSCH103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv",skip = 0)
abcSCH104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv",skip = 0)
abcSCH105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv",skip = 0)
abcSCH106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv",skip = 0)
#整理資料並加總ROW，再取出需要的資料
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
#將每一年的人數作成同一張表
abcCTtotal<-merge(abcCT103,abcCT104,by="國別",all=T)
abcCTtotal<-merge(abcCTtotal,abcCT105,by="國別",all=T)
abcCTtotal<-merge(abcCTtotal,abcCT106,by="國別",all=T)
abcCTtotal$Total103to106<-rowSums(abcCTtotal[2:5],na.rm = TRUE)

#載入國家與國碼對照表，整理完資料並與來台外國人表格合併
CountriesComparisionTable <- read_csv("D:/data/CountriesComparisionTable.csv",skip=0)
colnames(CountriesComparisionTable)<-c("iso_a3","English","國別")
CountriesComparisionTable<-CountriesComparisionTable[-267,]
abcCTtotal<-merge(abcCTtotal,CountriesComparisionTable,by="國別",all.x=T)

#載入地圖資料，再合併資料框與排序
data(country.map)
final.plot<-merge(country.map,
                  abcCTtotal,by="iso_a3",all.x=T)%>%
  group_by(group)%>%
  arrange(order)
```

### 哪些國家來台灣唸書的學生最多呢？

``` r
abcCTtotal<-merge(abcCT103,abcCT104,by="國別",all=T)
abcCTtotal<-merge(abcCTtotal,abcCT105,by="國別",all=T)
abcCTtotal<-merge(abcCTtotal,abcCT106,by="國別",all=T)
abcCTtotal$Total103to106<-rowSums(abcCTtotal[2:5],na.rm = TRUE)
abcCTtotal<-head(abcCTtotal[order(abcCTtotal$Total103to106,decreasing = T),],10)
abcCTtotal
```

    ##         國別 Total103 Total104 Total105 Total106 Total103to106
    ## 5   中國大陸    33288    41951    41981    35304        152524
    ## 114 馬來西亞    13385    15054    16311    17281         62031
    ## 94      香港     6286     8233     8660     8761         31940
    ## 17      日本     5816     6455     7542     8387         28200
    ## 136     越南     4005     4459     5342     7864         21670
    ## 168     澳門     4723     5152     5286     5141         20302
    ## 38      印尼     3559     4454     5154     6453         19620
    ## 80      南韓     3587     4062     4575     4724         16948
    ## 90      美國     3328     4003     3701     3814         14846
    ## 101     泰國     1535     1591     1771     2138          7035

### 哪間大學的境外生最多呢？

``` r
abcSCHtotal<-merge(abcSCH103,abcSCH104,by="學校名稱",all=T)
abcSCHtotal<-merge(abcSCHtotal,abcSCH105,by="學校名稱",all=T)
abcSCHtotal<-merge(abcSCHtotal,abcSCH106,by="學校名稱",all=T)
abcSCHtotal$Total103to106<-rowSums(abcSCHtotal[2:5],na.rm = TRUE)
abcSCHtotal<-head(abcSCHtotal[order(abcSCHtotal$Total103to106,decreasing = T),],10)
abcSCHtotal
```

    ##             學校名稱 Total103 Total104 Total105 Total106 Total103to106
    ## 111 國立臺灣師範大學     4648     5328     5709     6428         22113
    ## 108     國立臺灣大學     3800     4514     4817     5068         18199
    ## 12      中國文化大學     4137     4188     4046     3703         16074
    ## 153         銘傳大學     3452     4152     4159     4294         16057
    ## 126         淡江大學     2986     3254     3727     3920         13887
    ## 81      國立政治大學     2705     2871     2989     3061         11626
    ## 74      國立成功大學     2385     2554     2864     3179         10982
    ## 150         輔仁大學     2276     2271     2518     2434          9499
    ## 127         逢甲大學     2032     2479     2440     2523          9474
    ## 11          中原大學     1353     1703     1995     2611          7662

### 各個國家來台灣唸書的學生人數條狀圖

``` r
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
```

![](InternationalStudents_files/figure-markdown_github/ToTWNCountryBar-1.png)

### 各個國家來台灣唸書的學生人數面量圖

``` r
abcCTmap<-ggplot() +
  geom_polygon(data = final.plot, 
               aes(x =long, y =lat, 
                   group = group, 
                   fill = Total103to106), 
               color = "black", 
               size = 0.25) + 
  coord_cartesian(xlim = c(-200, 200),ylim = c(-90, 90))+
  coord_fixed()+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="ABC of X in Taiwan")

abcCTmap
```

![](InternationalStudents_files/figure-markdown_github/ToTWNCountryMap-1.png)

台灣學生國際交流分析
--------------------

### 資料匯入與處理

``` r
library(readr)
library(dplyr)
library(ggplot2)
library(choroplethrMaps)
library(RColorBrewer)
#載入台灣大專院校去哪些國家資料，並取出需要的資料
Student_RPT_07 <- read_csv("D:/data/Student_RPT_07.csv",skip=2)
Student_RPT_07<-Student_RPT_07[,c(6,10,11,12,13)]
colnames(Student_RPT_07)<-c("學校名稱","對方學校國別","對方學校名稱","學校英文名稱","本國學生出國進修_交流人數")
#把一樣地區可是名子不一樣的國別改成一樣
Student_RPT_07$對方學校國別<-gsub("大陸地區","中國大陸",Student_RPT_07$對方學校國別)
Student_RPT_07$對方學校國別<-gsub("大韓民國(南韓)","南韓",Student_RPT_07$對方學校國別)
Student_RPT_07$對方學校國別<-gsub("德意志聯邦共和國","德國",Student_RPT_07$對方學校國別)

#把上面資料框做分組並計算各國總人數，再由大排到小
how_many_peopleCT<-group_by(Student_RPT_07,對方學校國別)%>% 
  summarise(TotalPeople = sum(`本國學生出國進修_交流人數`,na.rm = T))%>%
  arrange(desc(TotalPeople))
```

### 台灣大專院校的學生最喜歡去哪些國家進修交流呢？

``` r
STtop10go<-head(how_many_peopleCT[order(how_many_peopleCT$TotalPeople,decreasing = T),],10)
STtop10go
```

    ## # A tibble: 10 x 2
    ##    對方學校國別   TotalPeople
    ##    <chr>                <int>
    ##  1 中國大陸             16425
    ##  2 日本                 12430
    ##  3 美國                  8916
    ##  4 德國                  3164
    ##  5 南韓                  2498
    ##  6 法國                  2415
    ##  7 大韓民國(南韓)        2131
    ##  8 英國                  1416
    ##  9 加拿大                1180
    ## 10 香港                  1029

### 哪間大學的出國交流學生數最多呢？

``` r
how_many_peopleSCH<-group_by(Student_RPT_07,學校名稱)%>% 
  summarise(TotalPeople = sum(`本國學生出國進修_交流人數`,na.rm = T))%>%
  arrange(desc(TotalPeople))

STtopgo10SCH<-head(how_many_peopleSCH[order(how_many_peopleSCH$TotalPeople,decreasing = T),],10)
STtopgo10SCH
```

    ## # A tibble: 10 x 2
    ##    學校名稱     TotalPeople
    ##    <chr>              <int>
    ##  1 國立臺灣大學        4719
    ##  2 淡江大學            3794
    ##  3 國立政治大學        3479
    ##  4 逢甲大學            2646
    ##  5 東海大學            1881
    ##  6 元智大學            1864
    ##  7 國立交通大學        1513
    ##  8 東吳大學            1457
    ##  9 國立成功大學        1397
    ## 10 國立臺北大學        1397

### 台灣大專院校的學生最喜歡去哪些國家進修交流條狀圖

``` r
library(ggplot2)

how_many_peopleCTn<-nrow(filter(how_many_peopleCT,TotalPeople>500))
how_many_peopleCT1<-group_by(how_many_peopleCT,`對方學校國別`) %>%
  tally(TotalPeople, sort = TRUE) %>%
  group_by(`對方學校國別` = factor(c(`對方學校國別`[1:how_many_peopleCTn], rep("Other", n() - how_many_peopleCTn)),
                         levels = c(`對方學校國別`[1:how_many_peopleCTn], "Other"))) %>%
  tally(n) 

CTPlot2<-ggplot()+geom_bar(data=how_many_peopleCT1,
                          aes(x=對方學校國別,y=nn),
                          stat = "identity")+
  scale_y_sqrt()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1,size = 8))+
  labs(y="單位log(人)",title = "去各國家進修學生長方圖")

CTPlot2
```

![](InternationalStudents_files/figure-markdown_github/FromTWNCountryBar-1.png)

### 台灣大專院校的學生最喜歡去哪些國家進修交流面量圖

``` r
colnames(CountriesComparisionTable)<-c("iso_a3","English","對方學校國別")
how_many_peopleCT<-merge(CountriesComparisionTable,how_many_peopleCT,by="對方學校國別")

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
    coord_cartesian(xlim = c(-200, 200),ylim = c(-90, 90))+
  coord_fixed()+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="Taiwanese(交流) of X in the World")
stCTmap
```

![](InternationalStudents_files/figure-markdown_github/FromTWNCountryMap-1.png)

台灣學生出國留學分析
--------------------

### 資料匯入與處理

``` r
people_out<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")
people_out<-people_out[,1:3]
```

### 台灣學生最喜歡去哪些國家留學呢？

``` r
people_top10_out<-head(arrange(people_out[,2:3],desc(總人數)),10)
people_top10_out
```

    ## # A tibble: 10 x 2
    ##    國別     總人數
    ##    <chr>     <dbl>
    ##  1 美國     21127.
    ##  2 澳大利亞 13582.
    ##  3 日本      8444.
    ##  4 加拿大    4827.
    ##  5 英國      3815.
    ##  6 德國      1488.
    ##  7 紐西蘭    1106.
    ##  8 波蘭       561.
    ##  9 馬來西亞   502.
    ## 10 奧地利     419.

### 台灣學生最喜歡去哪些國家留學面量圖

``` r
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
    coord_cartesian(xlim = c(-200, 200),ylim = c(-90, 90))+
  coord_fixed()+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="Taiwanese(留學) of X in the Word")
people_gomap
```

![](InternationalStudents_files/figure-markdown_github/FromTWNAbMap-1.png)

綜合分析
--------

請問來台讀書與離台讀書的來源國與留學國趨勢是否相同(5分)？想來台灣唸書的境外生，他們的母國也有很多台籍生嗎？請圖文並茂說明你的觀察(10分)。

### 來台灣唸書的學生最多的國家

``` r
CTPlot
```

![](InternationalStudents_files/figure-markdown_github/FromTWNAb_Analysis_abcCTmap-1.png)

``` r
abcCTmap
```

![](InternationalStudents_files/figure-markdown_github/FromTWNAb_Analysis_abcCTmap-2.png)

### 台灣大專院校的學生最喜歡去進修交流的國家

``` r
CTPlot2
```

![](InternationalStudents_files/figure-markdown_github/FromTWNAb_Analysis_CTPlot-1.png)

``` r
stCTmap
```

![](InternationalStudents_files/figure-markdown_github/FromTWNAb_Analysis_CTPlot-2.png)

### 台灣學生最喜歡去留學的國家

``` r
people_gomap
```

![](InternationalStudents_files/figure-markdown_github/FromTWNAb_Analysis_people_gomap-1.png)

由上幾張圖可以發現來台讀書與離台讀書(交流)的來源國第一名都是中國，反而去留學卻沒有中國，可以發現來台讀書和離台交流多數都是地理位置近的國家。而離台交流和留學又有一點關聯性，畢竟有交流才有機會出國留學，只是要留學的人都會選擇比較遠一點的地方去留學，可能是因為他們是技術強國，學習環境也不一樣，學習到的東西相對比較多，而且對於台灣的觀念而言，除了日本的亞洲國家的文憑都沒有外國的看起來厲害，所以才會有這種發展局面。
