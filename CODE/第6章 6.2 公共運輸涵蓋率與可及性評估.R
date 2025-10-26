library(dplyr)
library(sf)
library(TWspdata)
library(ggplot2)
library(TDX)
library(ggsflabel)

#---TDX套件中的函式需要金鑰，請在以下網站中申請！---#
# https://tdx.transportdata.tw/register
# 申請後將可取得一組client_id與client_secret
# access_token=get_token(client_id, client_secret)
# 以下程式碼中的access_token即表示存取權杖

windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))

# 擷取苗栗縣圖資
miaoli=filter(taiwan_village, COUNTYNAME=="苗栗縣")%>%
  select(VILLCODE, COUNTYNAME, TOWNNAME, VILLNAME)%>%
  st_transform(crs=3826)

# 介接苗栗縣市區公車站牌站序資料
bus_stop_city=Bus_StopOfRoute(access_token, "MiaoliCounty", dtype="sf")
bus_stop_city=st_transform(bus_stop_city, crs=3826)

# 介接苗栗縣市區公車路線資料
bus_shape_city=Bus_Shape(access_token, "MiaoliCounty", dtype="sf")
bus_shape_city=st_transform(bus_shape_city, crs=3826)

# 介接一般公路客運站牌站序資料
bus_stop_intercity=Bus_StopOfRoute(access_token, "Intercity", dtype="sf")

# 預設為EPSG:4326，需統一為EPSG3826
bus_stop_intercity=st_transform(bus_stop_intercity, crs=3826)

# 擷取位於苗栗縣的公路客運站牌(預設會下載包含國道的資料)
bus_stop_intercity=st_intersection(bus_stop_intercity, miaoli$geometry)

# 介接一般公路客運路線資料
bus_shape_intercity=Bus_Shape(access_token, "Intercity", dtype="sf")
bus_shape_intercity=st_transform(bus_shape_intercity, crs=3826)


# 篩選行經苗栗縣的一般公路客運
bus_route=Bus_Route(access_token, "Intercity")
bus_route=filter(bus_route, BusRouteType==12)$SubRouteUID

bus_stop_intercity=filter(bus_stop_intercity, SubRouteUID %in% bus_route)
bus_shape_intercity=filter(bus_shape_intercity, SubRouteUID %in% bus_stop_intercity$SubRouteUID)


# 繪製公車路線與站點地圖
ggplot()+
  geom_sf(data=miaoli, color="#E0E0E0", fill="#BEBEBE")+
  geom_sf(data=st_boundary(filter(taiwan_town, COUNTYNAME=="苗栗縣")), color="#7B7B7B")+
  geom_sf(data=bus_shape_intercity, color="#004B97")+
  geom_sf(data=bus_stop_intercity, size=1, color="#004B97")+
  geom_sf(data=bus_shape_city, color="red")+
  geom_sf(data=bus_stop_city, size=1, color="red")+
  geom_sf_text_repel(data=filter(taiwan_town, COUNTYNAME=="苗栗縣"), aes(label=TOWNNAME), family="A", size=4, color="#6C6C6C", fontface="bold")+
  theme_void()


# 建立苗栗縣網格單元
miaoli_grid=st_make_grid(miaoli, cellsize=200)%>%
  st_sf()


ggplot()+
  geom_sf(data=miaoli, color="#E0E0E0", fill="#BEBEBE")+
  geom_sf(data=miaoli_grid, color="#75AADB", fill=alpha("#75AADB", 0.1), size=0.001)+
  theme_void()


# 裁切網格
miaoli_grid=st_intersection(miaoli_grid, miaoli)


# 取道路環域500公尺
road_buf=st_buffer(Miaoli_Road_Network, 500)

# 將環域結果取聯集
road_buf=st_union(road_buf)

# 與道路路網500公尺範圍取交集，具交集者尚為分析對象
miaoli_grid_road=st_intersects(miaoli_grid, road_buf)

# 檢查網格是否與道路有交集，若無交集將顯示empty，長度為0；若有交集則長度為1
road_inter=lengths(miaoli_grid_road)>0

# 查看road_inter第100至110筆資料
road_inter[100:110]

# 篩選有交集者
miaoli_grid=miaoli_grid[road_inter,]


# 給予待分析的每一網格唯一編號
miaoli_grid=data.frame(GridID=c(1:nrow(miaoli_grid)), miaoli_grid)%>%
  st_sf()


ggplot()+
  geom_sf(data=miaoli, color="#E0E0E0", fill="#BEBEBE")+
  geom_sf(data=miaoli_grid, color="#75AADB", fill=alpha("#75AADB", 0.1), size=0.001)+
  theme_void()


# 以網格的中心點進行涵蓋率分析
miaoli_grid_center=st_centroid(miaoli_grid)

# 將市區客運與一般公路客運站牌資料合併
miaoli_stop=rbind(st_buffer(bus_stop_city, 500), st_buffer(bus_stop_intercity, 500))

# 取所有公車站牌之500公尺環域
miaoli_stop_buf=st_buffer(miaoli_stop, 500)

# 將網格中心與環域結果取交集
miaoli_grid_bus=st_intersection(miaoli_grid_center, miaoli_stop_buf[, c("SubRouteUID", "SubRouteName", "StopUID", "StopName", "Direction", "StopSequence")])

# 增加是否有涵蓋之欄位
miaoli_grid=mutate(miaoli_grid, COVER=ifelse(GridID %in% miaoli_grid_bus$GridID, 1, 0))

ggplot()+
  geom_sf(data=miaoli, color=NA, fill="#BEBEBE")+
  geom_sf(data=miaoli_grid, aes(fill=as.character(COVER)), size=0.001)+
  scale_fill_manual(values=c("0"="#FF2D2D", "1"="#00EC00"), labels=c("未涵蓋", "涵蓋"), name="")+
  theme_void()+
  theme(legend.text=element_text(family="A", size=18))


# 統計每一村里的網格總數與涵蓋總數
miaoli_grid_summary=group_by(st_drop_geometry(miaoli_grid), VILLCODE, COUNTYNAME, TOWNNAME, VILLNAME)%>%
  summarise(ALL=n(),
            COVERAGE=sum(COVER)/n())


# 將miaoli與miaoli_grid_summary兩圖資合併
miaoli_grid_summary=left_join(miaoli, miaoli_grid_summary)

ggplot()+
  geom_sf(data=miaoli_grid_summary, aes(fill=COVERAGE), color=NA)+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="涵蓋率")+
  geom_sf(data=filter(taiwan_town, COUNTYNAME=="苗栗縣"), fill=NA)+
  geom_sf_text_repel(data=filter(taiwan_town, COUNTYNAME=="苗栗縣"), aes(label=TOWNNAME), family="A", size=4, color="#BEBEBE", fontface="bold")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=20),
        legend.text=element_text(family="B", size=18))



#---可及性分析---#
# 介接所有臺鐵站點資料，並篩選位於苗栗縣者
# 座標格式轉換為EPSG:3826，便於後續分析
miaoli_TRA=Rail_Station(access_token, operator="TRA", dtype="sf")%>%
  filter(LocationCity=="苗栗縣")%>%
  st_transform(crs=3826)%>%
  select(StationName, StationUID)

# 介接所有高鐵站點資料，並篩選位於苗栗縣者
# 座標格式轉換為EPSG:3826，便於後續分析
miaoli_HSR=Rail_Station(access_token, operator="THSR", dtype="sf")%>%
  filter(LocationCity=="苗栗縣")%>%
  st_transform(crs=3826)%>%
  select(StationName, StationUID)

# 將臺鐵與高鐵站點資料予以合併
miaoli_train=rbind(miaoli_TRA, miaoli_HSR)

# 將苗栗軌道運輸資料與公車站牌500公尺環域取交集
miaoli_train_bus=st_intersection(miaoli_train, miaoli_stop_buf)

# 整理網格中心點與公車站牌500公尺環域範圍取交集之結果
temp_grid=st_drop_geometry(miaoli_grid_bus)%>%
  select(GridID, SubRouteUID, SubRouteName, StopUID, StopName, Direction, StopSequence)%>%
  rename(StopUID_O=StopUID, StopName_O=StopName, StopSequence_O=StopSequence)

# 整理軌道運輸場站與公車站牌500公尺環域範圍取交集之結果
temp_train=st_drop_geometry(miaoli_train_bus)%>%
  select(StationName, SubRouteUID, SubRouteName, StopUID, StopName, Direction, StopSequence)%>%
  rename(StopUID_D=StopUID, StopName_D=StopName, StopSequence_D=StopSequence)

# 將兩者依據路線代碼合併
grid2train=left_join(temp_grid, temp_train)

# 篩選起始點站序小於到達站點站序者
grid2train=filter(grid2train, StopSequence_O<=StopSequence_D)

# 去除重複資料
grid2train=distinct(grid2train, GridID, StationName)

# 在網格資料中增加一欄位，記錄該網格是否與軌道運輸場站可及
miaoli_grid=mutate(miaoli_grid, ACCESS=ifelse(GridID %in% grid2train$GridID, 1, 0))

# 統計每一村里的網格總數與涵蓋總數
miaoli_grid_summary2=group_by(st_drop_geometry(miaoli_grid), VILLCODE, COUNTYNAME, TOWNNAME, VILLNAME)%>%
  summarise(ALL=n(),
            ACCESSIBILITY=sum(ACCESS)/n())

miaoli_grid_summary2=left_join(miaoli, miaoli_grid_summary2)

ggplot()+
  geom_sf(data=miaoli_grid_summary2, aes(fill=ACCESSIBILITY))+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="至軌道運輸場之可及性")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=20),
        legend.text=element_text(family="B", size=18))



