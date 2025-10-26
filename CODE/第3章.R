# 所需套件
library(sf)
library(ggplot2)
library(dplyr)
library(spData)
library(TWspdata) # 本書範例資料套件

#---本書範例資料套件下載方法---#

### 方法一：藉由網址連結直接下載檔案 ###
# 下載網址：https://github.com/ChiaJung-Yeh/Spatial-Analysis/raw/master/data.zip
# 請直接將以上網址貼到瀏覽器上，即可自動下載名為「data」的資料夾

### 方法一：藉由網址連結直接下載檔案 ###
# 使用install.packages()函式安裝devtools套件
install.packages(devtools)

# 在GitHub上安裝ChiaJung-Yeh用戶的TWspdata套件
devtools::install_github("ChiaJung-Yeh/TWspdata")





#######################################################################
#---3.1  簡單圖徵之建構---#
#######################################################################

### 點（POINT）
# 設定一組向量c(x, y)
point_eg=st_point(c(2, 3))

# 檢視point_eg
point_eg

# 檢查point_eg的資料型態
class(point_eg)

# 繪圖
ggplot()+
  geom_sf(data=point_eg)



### 線（LINESTRING）
# 設定一組陣列rbind()
linestring_eg=st_linestring(rbind(c(2,3), c(4,4), c(3,5), c(1,4)))

# 檢視linestring_eg
linestring_eg

# 檢查linestring_eg的資料型態
class(linestring_eg)



### 面（POLYGON）
# 設定一組列表list()
polygon_eg=st_polygon(list(rbind(c(2,3), c(4,4), c(3,5), c(1,4), c(2,3))))

# 檢視polygon_eg
polygon_eg


# 檢查polygon_eg的資料型態
class(polygon_eg)



### 多點（MULTIPOINT）
# 設定一組陣列rbind()
mpoint_eg=st_multipoint(rbind(c(2,3), c(4,4), c(3,5), c(1,4)))

# 檢視mpoint_eg
mpoint_eg

# 檢查mpoint_eg的資料型態
class(mpoint_eg)



### 多線（MULTILINESTRING）
# 設定一組列表list()
mlinestring_eg=st_multilinestring(list(rbind(c(2,3), c(4,4), c(3,5)), rbind(c(2,5), c(1,2))))

# 檢視mlinestring_eg
mlinestring_eg

# 檢查mlinestring_eg的資料型態
class(mlinestring_eg)



### 多面（MULTIPOLYGON）
# 設定一組列表list()
mpolygon_eg=st_multipolygon(list(list(rbind(c(2,3), c(4,4), c(3,5), c(1,4), c(2,3))),
                                 list(rbind(c(1,5), c(2,5), c(3,6), c(1,5)))))

# 檢視mpolygon_eg
mpolygon_eg

# 檢查mmpolygon_eg的資料型態
class(mpolygon_eg)



### 簡單圖徵向量
# 建立簡單圖徵幾何元素
point1=st_point(c(3, 5))
point2=st_point(c(2, 6))
point3=st_point(c(1, 4))

# 建立簡單圖徵向量
point123=st_sfc(point1, point2, point3)

# 查看point123資料型態
class(point123)

# 查看point123細目
point123

# 建立簡單圖徵向量（附加CRS）
point123=st_sfc(point1, point2, point3, crs=4326)

# 查看point123細目
point123

# 查看CRS
st_crs(point123)$epsg



### 簡單圖徵
# 依序建立空間資料
office_geom=st_sfc(
  st_point(c(120.973255, 24.805162)),   # 東區區公所經緯度
  st_point(c(120.970314, 24.816374)),   # 北區區公所經緯度
  st_point(c(120.942268, 24.794044)),   # 香山區區公所經緯度
  crs=4326)                               # 設定座標參考系統

# 檢查office_geom資料型態
class(office_geom)

# 依序建立屬性資料，並將空間資料合併於後
office=data.frame(
  name=c("東區","北區","香山區"),                          # 區域名稱
  address=c("民族路40號","國華街69號","育德街188號"),  # 區公所地址
  phone=c("03-5218231","03-5152525","03-5307105"),      # 區公所電話
  office_geom                                              # 放入空間資料
)

# 檢查office資料型態
class(office)

# 轉換office的資料格式
office=st_sf(office)

# 再次檢查office資料型態
class(office)

# 輸出office資料
office



### 簡單圖徵(第二種方法)
# 依序建立空間資料 (暫且不設定CRS)
office_geom=st_sfc(
  st_point(c(120.973255, 24.805162)),   # 東區區公所經緯度
  st_point(c(120.970314, 24.816374)),   # 北區區公所經緯度
  st_point(c(120.942268, 24.794044)))   # 香山區區公所經緯度

# 依序建立屬性資料，並將空間資料合併於後
office=data.frame(
  name=c("東區","北區","香山區"),                          # 區域名稱
  address=c("民族路40號","國華街69號","育德街188號"),  # 區公所地址
  phone=c("03-5218231","03-5152525","03-5307105"),      # 區公所電話
  office_geom                                              # 放入空間資料
)

# 轉換office的資料格式 (在此設定CRS)
office=st_sf(office, crs=4326)





#######################################################################
#---3.2  讀取地理資料---#
#######################################################################

### 匯入Shapefile格式
# 讀取資料（相對路徑）
taipei_village_map1=
  read_sf("./data/taipei_map/taipei_village_map.shp")

# 查看taipei_village_map前六筆資料
head(taipei_village_map1)

# 使用st_read()匯入的資料型態
class(taipei_village_map2)



### 匯入文字格式（.csv、.txt）與建構地理資料
# 將WKT文字資料放入st_as_sfc函式中
point1=st_as_sfc("POINT (2 3)")

# 查看point1資料
point1

# 檢查point1的資料類型
class(point1)

# 建立文字向量
point2=c("POINT (2 3)", "POINT (4 5)", "POINT (6 7)")

# 將WKT文字向量放入st_as_sfc函式中
point2=st_as_sfc(point2)

# 查看point2資料
point2

# 檢查point2的資料類型
class(point2)



### 匯入文字格式與建構地理資料(範例：新竹公車路線)
# 讀取csv檔案
hsinchu_bus_route=read.csv("./data/csv_files/hsinchu_bus_route.csv")

# 檢查hsinchu_bus_route資料型態
class(hsinchu_bus_route)

# 將WKT文字向量放入st_as_sfc函式中
hsinchu_bus_route$Geometry=st_as_sfc(hsinchu_bus_route$Geometry)

# 檢查hsinchu_bus_route資料型態
class(hsinchu_bus_route)

# 檢查hsinchu_bus_route中Geometry的資料型態
class(hsinchu_bus_route$Geometry)

# 將WKT文字向量放入st_as_sfc函式中
hsinchu_bus_route=st_sf(hsinchu_bus_route, crs=4326)

# 檢查資料型態
class(hsinchu_bus_route)

# 查看hsinchu_bus_route資料
hsinchu_bus_route



### 匯入文字格式與建構地理資料(範例：新竹觀光景點)
# 讀取csv檔案
hsinchu_scenicSpot=read.csv("./data/csv_files/hsinchu_scenicSpot.csv")

# 新增WKT文字向量
hsinchu_scenicSpot=mutate(hsinchu_scenicSpot,
                          Geometry=paste("POINT(", PositionLon, " ", PositionLat, ")"))

# 將Geometry欄位轉變為sfc
hsinchu_scenicSpot$Geometry=st_as_sfc(hsinchu_scenicSpot$Geometry)

# 建立sf資料
hsinchu_scenicSpot=st_sf(hsinchu_scenicSpot, crs=4326)

# 檢查hsinchu_scenicSpot資料型態
class(hsinchu_scenicSpot)

# 查看hsinchu_scenicSpot前六筆資料
head(hsinchu_scenicSpot)

# 逕讀取文字格式為地理資料
bus_route_read=read_sf("./data/csv_files/hsinchu_bus_route.csv", options="GEOM_POSSIBLE_NAMES=Geometry")

scenicSpot_read=read_sf("./data/csv_files/hsinchu_scenicSpot.csv",
                        options=c("X_POSSIBLE_NAMES=PositionLon",
                                  "Y_POSSIBLE_NAMES=PositionLat"))





#######################################################################
#---3.3  座標參考系統與地理資料邊界---#
#######################################################################

### leaflet中若CRS非4326無法繪製地圖(會出現錯誤訊息)
leaflet()%>%
  addProviderTiles(providers$CartoDB)%>%
  addPolygons(data=nz)%>%
  addCircleMarkers(nz_height)



### 查詢座標參考系統
# 查看CRS
st_crs(nz)

# 查看EPSG代碼
st_crs(nz)$epsg



### 轉換座標參考系統
# 先查看nz_height資料
head(nz_height)

# 轉換座標參考系統
nz_height_4326=st_transform(nz_height, crs=4326)

# 查看nz_height_4326資料
head(nz_height_4326)



### 地理資料邊界
# 回傳nz_height的座標極值
st_bbox(nz_height)

# 查看st_bbox()回傳資料型態
class(st_bbox(nz_height))

# 轉變為簡單圖徵向量格式
st_as_sfc(st_bbox(nz_height))



### 藉由地理資料邊界裁切地圖
# 擷取紐西蘭West Coast的地理資料
nz_wc=filter(nz, Name=="West Coast")

# 記錄West Coast的地理邊界
nz_wc_box=st_bbox(nz_wc)

# 繪製地圖
ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=nz_wc, fill="#B5B5B5")+
  geom_sf(data=nz_height, color="red")+
  coord_sf(xlim=c(nz_wc_box[1], nz_wc_box[3]),
           ylim=c(nz_wc_box[2], nz_wc_box[4]))+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank())



### 地理資料格式輸出
# 輸出us_states資料
write_sf(us_states, "./us_states_export.shp")

# 讀取us_states_export.shp資料
us_states_export=read_sf("./us_states_export.shp")

# 檢查原資料與匯出資料的名稱差異
colnames(us_states)
colnames(us_states_export)

# 輸出hsinchu_scenicSpot資料
write_sf(hsinchu_scenicSpot, "./hsinchu_scenicSpot.shp",
         layer_options="ENCODING=UTF-8")

# 移除地理資料中的空間欄位
us_states_att=st_drop_geometry(us_states)

# 查看us_states_att資料
head(us_states_att)

# 擷取空間資料WKT格式
us_states_WKT=st_as_text(us_states$geometry)

# 查看第一筆資料的空間WKT格式
us_states_WKT[1]

# 查看us_states_WKT的資料型態
class(us_states_WKT)

# 合併屬性及空間的文字資料
us_states_text=data.frame(us_states_att, geometry=us_states_WKT)

# 查看us_states_text的資料型態
class(us_states_text)

# 擷取點資料的座標值
nz_height_WKT=st_coordinates(nz_height)

# 查看輸出結果
head(nz_height_WKT)

# 移除地理資料中的空間欄位
nz_height_att=st_drop_geometry(nz_height)

# 合併屬性及空間的文字資料
nz_height_text=data.frame(nz_height_att, nz_height_WKT)

# 匯出地理資料文字格式
write.csv(nz_height_text, "./nz_height_text.csv", row.names=F)





#######################################################################
#---3.5  屬性資料合併---#
#######################################################################

# 讀取美國COVID 19統計資料
us_covid=read.csv("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2Fnytimes%2Fcovid-19-data%2Fmaster%2Fus-states.csv&filename=us-states.csv")

# 查看us_covid資料
head(us_covid)

# 轉換日期格式
us_covid$date=as.Date(us_covid$date)

# 擷取指定日期的累積統計數據
us_covid=filter(us_covid, date==as.Date("2021/9/30"))

# 查看整理後的us_covid資料
head(us_covid)

# 利用setdiff()檢查兩份資料內的名稱是否一致
setdiff(us_states$NAME, us_covid$state)

# 合併原地理資料與新蒐集的屬性資料
us_states_covid=left_join(us_states, us_covid, by=c("NAME"="state"))

# 新增一欄位記錄確診死亡率
us_states_covid$death_rate=us_states_covid$deaths/us_states_covid$cases

# 查看合併後的us_states_covid資料
head(us_states_covid)

# 地圖繪製
ggplot()+
  geom_sf(data=us_states_covid, aes(fill=death_rate))+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="確診死亡率 (%)")+
  theme_void()

