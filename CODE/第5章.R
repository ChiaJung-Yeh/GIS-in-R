# 所需套件
library(sf)
library(ggplot2)
library(dplyr)
library(spData)
library(TWspdata) # 本書範例資料套件
library(TDX)      # 運輸資料介接套件

#---本書範例資料套件下載方法---#

### 方法一：藉由網址連結直接下載檔案 ###
# 下載網址：https://github.com/ChiaJung-Yeh/Spatial-Analysis/raw/master/data.zip
# 請直接將以上網址貼到瀏覽器上，即可自動下載名為「data」的資料夾

### 方法一：藉由網址連結直接下載檔案 ###
# 使用install.packages()函式安裝devtools套件
install.packages(devtools)

# 在GitHub上安裝ChiaJung-Yeh用戶的TWspdata套件
devtools::install_github("ChiaJung-Yeh/TWspdata")




#---運輸資料介接套件安裝---#

# 在GitHub上安裝ChiaJung-Yeh用戶的TWspdata套件
devtools::install_github("ChiaJung-Yeh/NYCU_TDX")


#---本章節介接函式大多需要金鑰，請在以下網站中申請！---#
# https://tdx.transportdata.tw/register
# 以下程式碼以access_token表示金鑰


#---請參照 R TDX運輸資料介接指南 網站---# (含功能更新)
# https://chiajung-yeh.github.io/TDX_Guide/



#######################################################################
#---5.1  運輸資料流通服務平臺簡介---#
#######################################################################

### 獲取Access Token
# 輸入 Client Id 與 Client Secret 兩參數
access_token=get_token(client_id="YOUR-ID", client_secret="YOUR-SECRET")


#######################################################################
#---5.2  軌道運輸資料---#
#######################################################################

### 5.2.1  軌道站點資料
# 介接高鐵站點資料
THSR_station=Rail_Station(access_token, "THSR")

# 查看THSR_station前六筆資料
head(THSR_station)

# 介接臺北捷運鐵站點資料
TRTC_station=Rail_Station(access_token, "TRTC", dtype="sf")

# 查看TRTC_station資料
TRTC_station

# 繪製地圖
ggplot()+
  geom_sf(data=TRTC_station)



### 5.2.2  軌道路線站點資料
# 回傳臺鐵路線站點資料，並匯出資料為.csv檔案
TRA_stationofline=Rail_Station(access_token, "THSR")

# 查看TRA_stationofline前六筆資料
head(TRA_stationofline)



### 5.2.3  軌道路線線型資料
# 介接臺北捷運路線資料
TRTC_railshape=Rail_Shape(access_token, "TRTC", dtype="sf")

## 查看TRTC_railshape資料
TRTC_railshape

# 介接臺北捷運站點資料
TRTC_station=Rail_Station(access_token, "TRTC", dtype="sf")

# 繪製臺北捷運路網與站點地圖
ggplot()+
  geom_sf(data=TRTC_railshape, aes(color=LineName), show.legend="line")+
  scale_color_manual(values=c("淡水信義線"="#d90023", "板南線"="#0a59ae", "松山新店線"="#107547",
                              "中和新蘆線"="#f5a818", "文湖線"="#b57a25", "環狀線"="#fedb00"), name="路線")+
  geom_sf(data=TRTC_station)+
  theme_void()



### 5.2.4  軌道班表資料
# 介接臺鐵班表資料
TRA_timetable=Rail_TimeTable(access_token, "TRA", record="general")

# 查看TRA_timetable前六筆資料
head(TRA_timetable)

# 介接高雄捷運班表資料
KRTC_timetable=Rail_TimeTable(access_token, "KRTC", record="station")

# 查看KRTC_timetable前六筆資料
head(KRTC_timetable)





#######################################################################
#---5.3  公車運輸資料---#
#######################################################################

### 5.3.1  公車路線站點資料
# 介接新竹市公車路線站點資料，並匯出地理資料
Hsinchu_bus_station=Bus_StopOfRoute(access_token, "Hsinchu", dtype="sf", out="./Hsinchu_bus_station.shp")

# 查看Hsinchu_bus_station前六筆資料
head(Hsinchu_bus_station)



### 5.3.2  公車路線資料
# 介接公路客運路線資料
bus_route_intercity=Bus_Route(access_token, "Intercity")

# 查看bus_route_intercity前六筆資料
head(bus_route_intercity)



### 5.3.3  公車路線線型資料
# 介接新竹市市區公車路線資料
Hsinchu_bus_shape=Bus_Shape(access_token, "Hsinchu", dtype="sf")

# 查看Hsinchu_bus_station前六筆資料
head(Hsinchu_bus_shape)

# 繪製新竹市公車地圖
ggplot()+
  geom_sf(data=Hsinchu_bus_station, aes(color=RouteName))+
  scale_color_discrete(name="新竹市市區公車路線")+
  geom_sf(data=Hsinchu_bus_shape, aes(color=RouteName))+
  theme_void()



### 5.3.4  公車班表資料
# 介接新竹市市區公車班表資料
Hsinchu_bus_schedule=Bus_Schedule(access_token, "Hsinchu")

# 查看Hsinchu_bus_schedule前六筆資料
head(Hsinchu_bus_schedule)





#######################################################################
#---5.4  自行車運輸資料---#
#######################################################################

### 5.4.1  公共自行車站點資料
# 介接臺北市公共自行車站點
Taipei_bike_station=Bike_Station(access_token, "Taipei", dtype="sf")

# 查看Taipei_bike_station前六筆資料
head(Taipei_bike_station)

# tmap 套件繪製地圖
library(tmap)
tmap_mode("view")
tm_shape(Taipei_bike_station)+
  tm_dots(col="ServiceType")



### 5.4.2  自行車線型資料
# 介接臺北市自行車路網
Taipei_bike_shape=Bike_Shape(access_token, "Taipei", dtype="sf")

# 查看Taipei_bike_shape前六筆資料
head(Taipei_bike_shape)

# tmap 套件繪製地圖
tmap_mode("view")
tm_shape(Taipei_bike_shape)+
  tm_lines()





#######################################################################
#---5.5  其他TDX資料與功能---#
#######################################################################

### 5.5.1  航空班表資料
# 介接國內航空班表
air_schedule_domestic=Air_Schedule(access_token, domestic=TRUE)

# 查看air_schedule_domestic前六筆資料
head(air_schedule_domestic)

# 介接花蓮縣觀光景點
Hualien_scenicspot=Tourism(access_token, poi="ScenicSpot", county="HualienCounty", dtype="sf")

# 查看Hualien_scenicspot前六筆資料
head(Hualien_scenicspot)

# 為方便後續取交集，先將Hualien_scenicspot的座標系統轉換為EPSG:3826
Hualien_scenicspot=st_transform(Hualien_scenicspot, crs=3826)

# 自TWspdata套件中的taiwan_town資料擷取花蓮縣
hualien=filter(TWspdata::taiwan_town, COUNTYNAME=="花蓮縣")

# 將hualien的座標系統轉換為EPSG:3826
hualien=st_transform(hualien, crs=3826)

# 取交集並依據鄉鎮名稱，計算景點個數
Hualien_scenicspot_count=st_intersection(Hualien_scenicspot,
                                         hualien[, c("TOWNNAME")])%>%
  st_drop_geometry()%>%
  group_by(TOWNNAME)%>%
  summarise(count=n())%>%
  arrange(desc(count))

# 查看Hualien_scenicspot_count資料
Hualien_scenicspot_count



### 5.5.3  公路路網線型
# 介接臺中市所有公路
Taichung_road=Road_Network(access_token, county="Taichung", roadclass="ALL", dtype="sf")

# 查看Taichung_road前六筆資料
head(Taichung_road)



### 5.5.4  地理編碼服務
# 將房價資料中的地址進行地理編碼
# house_price為本書所提供資料
house_price_geocode=Geocoding(access_token, house_price$ADDRESS, dtype="sf")

# 擷取成功地理編碼的地址
house_price_geocode=house_price_geocode$SUCCESS

# 將房價資料與地理編碼結果予以合併
house_price_geocode=left_join(house_price_geocode, house_price, by=c("AddressOriginal"="ADDRESS"))

# 查看house_price_geocode前六筆資料
head(house_price_geocode)

