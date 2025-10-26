library(dplyr)
library(sf)
library(TWspdata)
library(ggplot2)
library(ggnewscale)

windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))

# YouBike起訖對騎乘流量(youbike_ridership)可自資料夾讀取，或直接使用TWspdata套件中的資料
# youbike_ridership=read.csv("data/youbike_ridership.csv")


# 查看youbike_ridership前六筆資料
head(youbike_ridership)

# 檢查youbike_ridership含有幾天的資料
unique(youbike_ridership$Date)

# 依各起訖對計算五天平均騎乘量，整理為OD資料
ubike_ridership_OD=group_by(youbike_ridership, RentStation, ReturnStation)%>%
  summarise(Riderships=round(mean(Riderships), 0))


# 檢查站點騎乘量資料中的站點
ubike_stcode=unique(c(ubike_ridership_OD$RentStation, ubike_ridership_OD$ReturnStation))
head(ubike_stcode)
length(ubike_stcode)

substr(taipei_youbike$SttnUID, 4, 7)%>%
  head()
as.numeric(substr(taipei_youbike$SttnUID, 4, 7))%>%
  head()

# 擷取騎乘量資料中擁有的站點
ubike_station=taipei_youbike[, c("SttnUID", "StatnNm")]
ubike_station$SttnUID=as.numeric(substr(ubike_station$SttnUID, 4, 7))
ubike_station=filter(ubike_station, SttnUID %in% ubike_stcode)

nrow(ubike_station)

# 確保兩資料中的站點完全相同
setdiff(ubike_station$SttnUID, ubike_stcode)
setdiff(ubike_stcode, ubike_station$SttnUID)

# 利用st_nearest_points函式建立各站點間的連線
ubike_station_OD=st_nearest_points(ubike_station, ubike_station)

# 繪圖檢查ubike_station_OD
ggplot(ubike_station_OD[1:398])+
  geom_sf()+
  theme_minimal()+
  theme(axis.text=element_text(size=15, family="B"))

# 建立起訖對編碼配對
temp_D=rep(ubike_station$SttnUID, times=nrow(ubike_station))
temp_O=temp_D[order(temp_D)]
stationid_temp=data.frame(RentStation=temp_O, ReturnStation=temp_D)

# 將起訖對編碼配對結果與ubike_station_OD合併
ubike_station_OD=cbind(stationid_temp, ubike_station_OD)

# 合併後變為data.frame格式
class(ubike_station_OD)

# 利用st_sf函式轉換為sf格式
ubike_station_OD=st_sf(ubike_station_OD)

# 將起訖對站點名稱予以合併
ubike_station_OD=left_join(ubike_station_OD, st_drop_geometry(ubike_station), by=c("RentStation"="SttnUID"))%>%
  rename(RentStationName=StatnNm)%>%
  left_join(st_drop_geometry(ubike_station), by=c("ReturnStation"="SttnUID"))%>%
  rename(ReturnStationName=StatnNm)

# 查看ubike_station_OD前六筆資料
head(ubike_station_OD)

# 將起訖對騎乘量資料(ubike_ridership_OD)與站點間OD連線(ubike_station_OD)予以合併
ubike_station_OD=left_join(ubike_station_OD, ubike_ridership_OD)

# 若起訖對資料中，騎乘量欄位(Riderships)為NA，表示該起訖對並無人騎乘，應直接設定為0
is.na(ubike_station_OD$Riderships)
ubike_station_OD$Riderships[is.na(ubike_station_OD$Riderships)]=0

# 繪圖檢查計算結果
temp=ubike_station_OD[ubike_station_OD$RentStation==1,]
temp=arrange(temp, Riderships)

ggplot()+
  geom_sf(data=temp, aes(color=Riderships))+
  scale_color_distiller(palette="YlOrRd", direction=1, name="騎乘量")+
  theme_minimal()+
  theme(axis.text=element_text(size=15, family="B"),
        legend.title=element_text(size=18, family="A"),
        legend.text=element_text(size=15, family="B"))


# 篩選日起訖對騎乘量超過50次者繪圖
temp=filter(ubike_station_OD, Riderships>=50)

# 將騎乘量分為四個區間
temp=mutate(temp, Riderships_class=case_when(
  Riderships<60 ~ "50-60",
  Riderships<80 ~ "60-80",
  Riderships<120 ~ "80-120",
  TRUE ~ ">=120"
))

# 將四個區間利用factor函式按照順序排列
temp$Riderships_class=factor(temp$Riderships_class, levels=c("50-60", "60-80", "80-120", ">=120"))

# 擷取臺北市行政區資料
taipei_town=filter(taiwan_town, COUNTYNAME=="臺北市")
taipei_town=st_transform(taipei_town, crs=3826)

ggplot()+
  geom_sf(data=taipei_town, color="#E0E0E0", fill="#BEBEBE")+
  geom_sf(data=st_intersection(taipei_mrt_route, taipei_town), aes(color=RAILNAME), show.legend=F)+
  scale_color_manual(values=c("淡水信義線"="#d90023", "板南線"="#0a59ae", "松山新店線"="#107547",
                              "中和新蘆線"="#f5a818", "文湖線"="#b57a25"))+
  new_scale_color()+
  geom_sf(data=temp, aes(color=Riderships_class), size=1)+
  scale_color_brewer(palette="YlOrRd", name="YouBike\n日均起訖對騎乘量")+
  geom_sf(data=st_intersection(taipei_mrt_station, taipei_town))+
  theme_void()+
  theme(legend.title=element_text(family="A", size=18),
        legend.text=element_text(family="B", size=15))



# 依站點分別統計租借與還車量
rent_bike=group_by(ubike_ridership_OD, RentStation)%>%
  summarise(Riderships=sum(Riderships))%>%
  rename(Station=RentStation)
return_bike=group_by(ubike_ridership_OD, ReturnStation)%>%
  summarise(Riderships=sum(Riderships))%>%
  rename(Station=ReturnStation)

# 將租借與還車量依站點合併統計
ubike_ridership_station=rbind(rent_bike, return_bike)%>%
  group_by(Station)%>%
  summarise(Riderships=sum(Riderships))



# 擷取臺北市境內的捷運站點
mrt_station=st_intersection(taipei_mrt_station, taipei_town)

# 先行尋找各YouBike場站最接近之捷運站的索引值
ubike_nearest_mrt=st_nearest_feature(ubike_station, mrt_station)

# 將索引值匹配原始的捷運站點資料
ubike_nearest_mrt=mrt_station[ubike_nearest_mrt, c("StationID", "Zh_tw")]

# 計算YouBike場站與其匹配最近捷運站間的直線距離
ubike_nearest_mrt_dist=as.numeric(st_distance(ubike_station, ubike_nearest_mrt, by_element=T))

# 合併YouBike站點、最接近捷運站點、兩者間距離的資料
ubike_nearest_mrt=cbind(ubike_station, st_drop_geometry(ubike_nearest_mrt), ubike_mrt_dist=ubike_nearest_mrt_dist)

# 將站點租還量統計資料與「距最近捷運站之距離」之變數合併
ubike_ridership_station=left_join(ubike_ridership_station, ubike_nearest_mrt[, c("SttnUID", "ubike_mrt_dist")], by=c("Station"="SttnUID"))


county_example=c("臺北市","新北市","宜蘭縣","新竹市","花蓮縣")
grepl("縣", county_example)
grepl(paste("縣", "北", sep="|"), county_example)


# 擷取名稱中大學或專科的學校
university=taiwan_school[grepl(paste("大學","專科","國防醫學院", sep="|"), taiwan_school$name) &
                           !grepl(paste("附小", "附中", "實小", "附屬", sep="|"), taiwan_school$name),]%>%
  select(name, lon, lat)
  
# 將文字資料轉換為地理資料
university=mutate(university, geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)

# 與臺北市行政區取交集
university=st_intersection(university, taipei_town$geometry)

# 繪製ubike_station的300公尺環域
ubike_station_buf=st_buffer(ubike_station, 300)

# 將環域區與大專院校取交集，所得結果中的YouBike場站即可定義為在大專院校方圓300公尺附近的YouBike站點
ubike_uni=st_intersection(ubike_station_buf, university)

# 將站點租還量統計資料與「是否位於大專院校附近」之變數合併
ubike_ridership_station=mutate(ubike_ridership_station, university=ifelse(Station %in% ubike_uni$SttnUID, 1, 0))


ubike_lm=lm(Riderships ~ ubike_mrt_dist+university, data=ubike_ridership_station)
summary(ubike_lm)

plot(ubike_lm)

