# 所需套件
library(sf)
library(ggplot2)
library(dplyr)
library(spData)





#######################################################################
#---2.1 以地理資料繪製簡單地圖---#
#######################################################################

### ggplot2美國地圖產出結果
# 第一種寫法
ggplot(data=us_states)+
  geom_sf()

# 第二種寫法
ggplot()+
  geom_sf(data=us_states)



### 地圖顏色與大小修正
ggplot()+
  geom_sf(data=us_states, size=2, color="red", fill="blue")





#######################################################################
#---2.2 屬性資料擷取---#
#######################################################################

### class查看資料型態
class(us_states)



### 檢查前六筆資料
head(us_states)



### 單一屬性資料擷取
us_states$NAME



### 多屬性資料擷取
us_states[, c(2,3)]



### 列（Rows）資料擷取
us_states[c(2:4),]



### 特定資料篩選與編修(filter)
filter(us_states, REGION=="West", total_pop_15>5000000)



### 特定資料篩選與編修(select)
select(us_states, NAME, REGION)



### 特定資料篩選與編修(mutate)
mutate(us_states, total_pop_20=2*total_pop_15-total_pop_10)





#######################################################################
#---2.3 地圖文字標記---#
#######################################################################

### 標記美國各州名稱
# 第一種寫法
ggplot()+
  geom_sf(data=us_states)+
  geom_sf_text(data=us_states, mapping=aes(label=NAME))

# 第二種寫法
ggplot()+
  geom_sf(data=us_states)+
  geom_sf_text(aes(label=NAME), us_states)



### 避免標記文字重疊
# 安裝套件
install.packages("devtools")
devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

# 文字標記重疊修正結果
ggplot()+
  geom_sf(data=us_states)+
  geom_sf_text_repel(data=us_states, aes(label=NAME), nudge_x=-0.1,
                     nudge_y=0.4, size=4, color="blue")





#######################################################################
#---2.4 數值型資料漸層地圖---#
#######################################################################

### 人口數量漸層地圖
ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))



### 人口數量漸層地圖（修正）
ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_continuous(low="#D2E9FF", high="#004B97")



### 人口數量漸層地圖（三段漸層）
ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_gradient2(low="red", mid="orange", high="green")



### 利用RColorBrewer套件設置顏色階層
# 安裝並載入套件
install.packages("RColorBrewer")
library(RColorBrewer)

# 人口數量漸層地圖（階層顏色）
ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_distiller(palette="YlOrRd", direction=1)





#######################################################################
#---2.5  類別型資料地圖---#
#######################################################################

### 美國區域類別地圖
ggplot()+
  geom_sf(data=us_states, aes(fill=REGION))



### 美國區域類別地圖（客製化顏色調整）
ggplot()+
  geom_sf(data=us_states, aes(fill=REGION))+
  scale_fill_manual(values=c("Norteast"="#FFC1E0", "Midwest"="#97CBFF", "South"="#A6FFA6", "West"="#FFFFE0"))



### 美國區域類別地圖（調色板）
ggplot()+
  geom_sf(data=us_states, aes(fill=REGION))+
  scale_fill_brewer(palette="Set2")





#######################################################################
#---2.6  規則標記地圖---#
#######################################################################

### 依州名開頭字母繪製規則地圖(方法一))
us_states_rule1=us_states

# 利用ifelse()巢狀結構建立判斷式
us_states_rule1$CLASS=ifelse(substr(us_states_rule1$NAME, 1, 1) %in% LETTERS[1:9], "A~I",
                             ifelse(substr(us_states_rule1$NAME, 1, 1) %in% LETTERS[10:18], "J~R", "S~Z"))

# 查看us_states_rule1資料
head(us_states_rule1)

# 繪製地圖
ggplot()+
  geom_sf(data=us_states_rule1, aes(fill=CLASS))+
  geom_sf_text_repel(data=us_states_rule1, aes(label=NAME))



### 依州名開頭字母繪製規則地圖(方法二))
us_states_rule2=us_states
us_states_rule2=mutate(us_states_rule2, CLASS=case_when(
  substr(NAME, 1, 1) %in% LETTERS[1:9] ~ "A~I",
  substr(NAME, 1, 1) %in% LETTERS[10:18] ~ "J~R",
  TRUE ~ "S~Z"
))



### 依人口與面積繪製規則標記地圖
# 計算人口與土地平均值
pop_mean=mean(us_states$total_pop_15)
area_mean=mean(us_states$AREA)

# 分類
us_states_rule3=mutate(us_states,CLASS=case_when(
  total_pop_15>pop_mean & AREA>area_mean ~ "HPHA",   #人口多、面積大
  total_pop_15>pop_mean & AREA<area_mean ~ "HPLA",   #人口多、面積小
  total_pop_15<pop_mean & AREA>area_mean ~ "LPHA",   #人口少、面積大
  total_pop_15<pop_mean & AREA<area_mean ~ "LPLA"    #人口少、面積小
))

# 查看新建資料
head(us_states_rule3)

# 繪製地圖
ggplot()+
  geom_sf(data=us_states_rule3, aes(fill=CLASS))





#######################################################################
#---2.7  圖層套疊與地圖元件設定---#
#######################################################################

### 紐西蘭地圖與高峰疊圖
ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=nz_height, color="red")



### 地圖元件：指北針與比例尺
# 安裝並載入套件
install.packages("ggspatial")
library(ggspatial)

# 紐西蘭高峰疊圖+指北針與比例尺
ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=nz_height, color="red")+
  annotation_scale(location="br", height=unit(2.5, "mm"))+
  annotation_north_arrow(location="tl", which_north="true")



### 地圖元件：地圖標題(紐西蘭高峰疊圖+指北針與比例尺+標題)
ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=nz_height, color="red")+
  annotation_scale(location="br", height=unit(2.5, "mm"))+
  annotation_north_arrow(location="tl", which_north="true")+
  ggtitle("New Zealand Map")





#######################################################################
#---2.8  地圖設計細節---#
#######################################################################

### 修正點線面圖形樣式
ggplot()+
  geom_sf(data=nz, color="blue", linetype="dashed")+
  geom_sf(data=nz_height, color="red", size=2, shape=4)



### 修正圖例名稱
ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="Population")



### 調整圖例順序
# 使用factor()編排順序
us_states_rule3$CLASS=factor(us_states_rule3$CLASS, levels=c("LPLA", "LPHA", "HPLA", "HPHA"))

# 重新繪製地圖
ggplot()+
  geom_sf(data=us_states_rule3, aes(fill=CLASS))



### 地圖主題調整
# 設定字體
windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))

# 繪製地圖
ggplot()+
  geom_sf(data=us_states, aes(fill=REGION))+
  ggtitle("美國地圖")+
  theme(panel.border=element_rect(color="black", fill=NA),
        panel.background=element_rect(fill="#A3B3C1"),
        panel.grid.major=element_line(color="#808080", linetype=2),
        axis.text=element_text(size=15, family="B"),
        axis.ticks=element_line(size=3),
        legend.background=element_rect(fill=alpha("#778899", 0.4)),
        legend.key=element_rect(fill=NA, color=NA),
        legend.text=element_text(size=15, family="B"),
        legend.title=element_text(size=15, family="B", hjust=0.5),
        legend.position=c(0.91, 0.2),
        plot.title=element_text(size=25, hjust=0.5, family="A"))



### 地圖範圍調整
# 第一種寫法
ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=filter(nz, Name=="West Coast"), fill="#B5B5B5")+
  geom_sf(data=nz_height, color="red")+
  xlim(1205019.303, 1571336.246) + ylim(5062352.347, 5485976.072)+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank())

# 第二種寫法
ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=filter(nz, Name=="West Coast"), fill="#B5B5B5",)+
  geom_sf(data=nz_height, color="red")+
  coord_sf(xlim=c(1205019.303, 1571336.246),
           ylim=c(5062352.347, 5485976.072))+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank())



### 地圖縮圖
# 安裝並載入套件
install.packages("cowplot")
library(cowplot)

# 繪製原始大地圖
p1=ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=filter(nz, Name=="West Coast"), fill="#B5B5B5")+
  geom_sf(data=nz_height, color="red")+
  coord_sf(xlim=c(1205019.303, 1571336.246),
           ylim=c(5062352.347, 5485976.072))+
  theme_void()

# 繪製全域縮圖
p2=ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=filter(nz, Name=="West Coast"), fill="#B5B5B5")+
  theme(panel.border=element_rect(color="black", fill=NA, size=2),
        panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

# 合併大地圖與縮圖
ggdraw(p1)+
  draw_plot(p2, x=-0.35, y=0.35, scale=0.27)



### 合併多張地圖(使用cowplot套件)
# 圖 2.4.1  人口數量漸層地圖
p1=ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  ggtitle("人口數量漸層地圖")+
  theme_void()+
  theme(title=element_text(size=12, family="A"),
        legend.title=element_text(size=10, family="B"),
        legend.text=element_text(size=8, family="B"))

# 圖 2.4.2  人口數量漸層地圖（修正）
p2=ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_continuous(low="#D2E9FF", high="#004B97")+
  ggtitle("人口數量漸層地圖（修正）")+
  theme_void()+
  theme(title=element_text(size=12, family="A"),
        legend.title=element_text(size=10, family="B"),
        legend.text=element_text(size=8, family="B"))

# 圖 2.4.3  人口數量漸層地圖（三段漸層）
p3=ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_gradient2(low="red", mid="orange", high="green")+
  ggtitle("人口數量漸層地圖（三段漸層）")+
  theme_void()+
  theme(title=element_text(size=12, family="A"),
        legend.title=element_text(size=10, family="B"),
        legend.text=element_text(size=8, family="B"))

# 圖 2.4.5  人口數量漸層地圖（階層顏色）
p4=ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_distiller(palette="YlOrRd", direction=1)+
  ggtitle("人口數量漸層地圖（階層顏色）")+
  theme_void()+
  theme(title=element_text(size=12, family="A"),
        legend.title=element_text(size=10, family="B"),
        legend.text=element_text(size=8, family="B"))

# 合併地圖
plot_grid(p1, p2, p3, p4, ncol=2, nrow=2)+
  draw_label("合併多張地圖", fontface='bold', fontfamily="A",
             size=20, x=0.5, y=0.97)



### 合併多張地圖(使用ggpubr套件)
# 安裝並載入套件
install.packages("ggpubr")
library(ggpubr)

nz_revised=nz

# 歸一化公式
maxmin=function(x) (x - min(x))/(max(x)-min(x))

# 將資料歸一化
nz_revised$Land_area=maxmin(nz_revised$Land_area)
nz_revised$Population=maxmin(nz_revised$Population)
nz_revised$Median_income=maxmin(nz_revised$Median_income)
nz_revised$Sex_ratio=maxmin(nz_revised$Sex_ratio)

# 土地面積漸層地圖
p1=ggplot()+
  geom_sf(data=nz_revised, aes(fill=Land_area))+
  scale_fill_distiller(palette="YlOrRd", direction=1,
                       name="歸一化數值")+
  ggtitle("土地面積")+
  theme_void()+
  theme(title=element_text(size=15, family="A"),
        legend.title=element_text(size=15, family="A"),
        legend.text=element_text(size=12, family="B"))

# 人口數漸層地圖
p2=ggplot()+
  geom_sf(data=nz_revised, aes(fill=Population))+
  scale_fill_distiller(palette="YlOrRd", direction=1,
                       name="歸一化數值")+
  ggtitle("人口數")+
  theme_void()+
  theme(title=element_text(size=15, family="A"),
        legend.title=element_text(size=15, family="A"),
        legend.text=element_text(size=12, family="B"))

# 收入中位數漸層地圖
p3=ggplot()+
  geom_sf(data=nz_revised, aes(fill=Median_income))+
  scale_fill_distiller(palette="YlOrRd", direction=1,
                       name="歸一化數值")+
  ggtitle("收入中位數")+
  theme_void()+
  theme(title=element_text(size=15, family="A"),
        legend.title=element_text(size=15, family="A"),
        legend.text=element_text(size=12, family="B"))

# 性別比漸層地圖
p4=ggplot()+
  geom_sf(data=nz_revised, aes(fill=Sex_ratio))+
  scale_fill_distiller(palette="YlOrRd", direction=1,
                       name="歸一化數值")+
  ggtitle("性別比")+
  theme_void()+
  theme(title=element_text(size=15, family="A"),
        legend.title=element_text(size=15, family="A"),
        legend.text=element_text(size=12, family="B"))

# 繪製合併地圖
ggarrange(p1, p2, p3, p4, ncol=2, nrow=2,
          common.legend=T, legend="right")



### 多重地理資料之顏色調整(錯誤範例)
ggplot()+
  geom_sf(data=nz, aes(color=Land_area))+
  scale_color_distiller(palette="YlOrRd", direction=1)+
  geom_sf(data=nz_height, aes(color=elevation))+
  scale_color_distiller(palette="YlGn", direction=1)+
  theme_void()



### 多重地理資料之顏色調整(修正)
# 安裝並載入套件
install.packages("ggnewscale")
library(ggnewscale)

# ggnewscale套件修正多重顏色設定
ggplot()+
  geom_sf(data=nz, aes(color=Land_area))+
  scale_color_distiller(palette="YlOrRd", direction=1)+
  new_scale_color()+
  geom_sf(data=nz_height, aes(color=elevation))+
  scale_color_distiller(palette="YlGn", direction=1)+
  theme_void()





#######################################################################
#---2.9  其他繪製地圖套件---#
#######################################################################

### tmap基礎地圖繪製
# 安裝並載入套件
install.packages("tmap")
library(tmap)

# 繪製地圖
tm_shape(nz)+
  tm_polygons(col="blue", border.col="red", lty="dashed")



### 依屬性繪製地圖
tm_shape(nz)+
  tm_polygons(col="Population")



### 依屬性繪製地圖（修正）
# 圖 2.9.3_A
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="pretty", palette="Reds")

# 圖 2.9.3_B
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="equal")

# 圖 2.9.3_C
tm_shape(nz)+
  tm_polygons(col="Population", n=4, style="quantile")

# 圖 2.9.3_D
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="jenks")

# 圖 2.9.3_E
tm_shape(nz)+
  tm_polygons(col="Population", style="cont")

# 圖 2.9.3_F
tm_shape(nz)+
  tm_polygons(col="Population", style="cat")



### tm_fill()繪製地圖
tm_shape(nz)+
  tm_fill(col="Population", n=5, style="jenks")



### tm_borders()繪製地圖
tm_shape(nz)+
  tm_fill(col="Population")+
  tm_borders()



### tm_symbols()繪製地圖
tm_shape(nz)+
  tm_polygons()+
  tm_shape(nz_height)+
  tm_symbols(col="elevation", size=0.1, shape=4,
             palette="Reds", style="quantile")



### 地圖文字標記
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="jenks")+
  tm_text(text="Name", size=1, fontfamily="B")




### 指北針與比例尺繪製
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="jenks")+
  tm_compass(north=0, type="rose", size=4, position=c(0.83, 0.89))+
  tm_scale_bar(breaks=c(0, 50, 100, 150, 200), text.size=1, position=c(0.7, 0.02))



### 地圖主題設定
# 字體設定
windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))

# 繪製地圖
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="jenks")+
  tm_shape(nz)+
  tm_text(text="Name", size=1, fontfamily="B")+
  tm_compass(north=0, type="rose", size=4,
             position=c(0.83, 0.89))+
  tm_scale_bar(breaks=c(0, 50, 100, 150, 200), text.size=1,
               position=c(0.7, 0.02))+
  tm_layout(title="紐西蘭人口地圖", title.fontfamily="A",
            title.size=2,
            legend.title.size=1.7, legend.title.fontfamily="B",
            legend.title.fontface="bold",
            legend.text.size=1, legend.text.fontfamily="B",
            legend.frame=T, legend.frame.lwd=1,
            legend.bg.color="#CCCCFF", legend.bg.alpha=0.4,
            bg.color="#A3B3C1")



### tm_style()經典地圖繪製
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="jenks")+
  tm_style("classic")



### 更換為動態地圖輸出模式
# 成果：https://rpubs.com/Chia-JungYEH/tmap_view
tmap_mode("view")
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="jenks")



### leaflet套件基礎地圖繪製
# 安裝與載入套件
install.packages("leaflet")
library(leaflet)

# 繪製地圖
leaflet()%>%
  addProviderTiles(providers$Esri)



### 動態地圖繪製
leaflet()%>%
  addProviderTiles(providers$CartoDB)%>%
  addPolygons(data=st_transform(nz, 4326), color="red",
              fillColor="#CCCCFF", weight=2, label=~Name)%>%
  addCircleMarkers(data=st_transform(nz_height, 4326), stroke=F,
                   radius=8, fillOpacity=0.2, label=~elevation)


