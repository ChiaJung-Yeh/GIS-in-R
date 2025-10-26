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
#---4.1  空間資料合併（Spatial Join）---#
#######################################################################

### 利用left_join()合併兩地理資料(錯誤訊息)
left_join(nz, nz_height)



### 空間資料合併(紐西蘭資料範例)
# 空間資料合併
nz_height_join1=st_join(nz_height, nz)

# 檢視前6筆資料
head(nz_height_join1)



### 將兩資料的參數設定順序顛倒
# 空間資料合併
nz_height_join2=st_join(nz, nz_height)

# 檢視前6筆資料
head(nz_height_join2)





#######################################################################
#---4.2  空間與屬性聚合（Spatial & Attribute Aggregation）---#
#######################################################################

### 屬性資料聚合
# 聚合美國地圖與人口資料
us_states_df=aggregate(total_pop_15 ~ REGION, FUN=sum, data=us_states)

# 確認us_states_df資料屬性
class(us_states_df)

# 查看us_states_df
us_states_df

### 屬性與空間資料聚合
us_states_sf=aggregate(x=us_states["total_pop_15"], by=list(us_states$REGION), FUN=sum)

# 確認us_states_sf資料屬性
class(us_states_sf)

# 查看us_states_sf
us_states_sf



### 兩地理資料的屬性與空間資料聚合
nz_ave_ele=aggregate(x=nz_height["elevation"], by=nz, FUN=mean)

# 查看nz_ave_ele前六筆資料
head(nz_ave_ele)



### group_by() %>% summarise()
# 屬性與空間資料聚合
us_states_sf2=group_by(us_states, REGION)%>%
  summarise(total_pop_15=sum(total_pop_15))

# 查看us_states_sf2前六筆資料
head(us_states_sf2)



### 世界地理資料範例
# 屬性與空間資料聚合
subregion_info=group_by(world, subregion)%>%
  summarise(
    pop=sum(pop, na.rm=T),               # 利用sum()計算人口總數
    lifeExp=mean(lifeExp, na.rm=T),     # 利用mean()計算平均壽命
    num_cou=n()                           # 利用n()計算國家總數
  )%>%
  arrange(desc(lifeExp))

# 查看subregion_info前六筆資料
head(subregion_info)



### 結合st_join()函式與group_by() %>% summarise()函式
# 屬性與空間資料聚合
ave_height=st_join(nz, nz_height)%>%
  group_by(Name)%>%
  summarise(elevation=mean(elevation),
            count=n())%>%
  mutate(count=ifelse(is.na(elevation), 0, count))%>%
  arrange(desc(count))

# 查看ave_height前六筆資料
head(ave_height)





#######################################################################
#---4.3  空間插值計算（Interpolation）---#
#######################################################################

### 計算各捷運站200公尺環域區域的總人數
# 計算特定區域總人口數
PP_interpo=st_interpolate_aw(taipei_village_map["PP"], taipei_mrt_station_buf, extensive=T)

# 查看PP_interpo前六筆資料
head(PP_interpo)

# 插值資料筆數
nrow(PP_interpo)

# 特定區域資料筆數
nrow(taipei_mrt_station_buf)



### 計算捷運站200公尺環域的人口總數與密度
# 計算特定區域總人口數
PP_interpo2=st_interpolate_aw(taipei_village_map["PP"], taipei_mrt_station_buf, extensive=T, keep_NA=T)
PP_interpo2=cbind(Station=taipei_mrt_station_buf$Zh_tw, PP_interpo2)

# 查看PP_interpo2前六筆資料
head(PP_interpo2)

# 計算特定區域人口密度
PPDENS_interpo=st_interpolate_aw(taipei_village_map["PPDENS"], taipei_mrt_station_buf, extensive=F, keep_NA=T)
PPDENS_interpo=cbind(Station=taipei_mrt_station_buf$Zh_tw,
                     PPDENS_interpo)

# 查看PPDENS_interpo前六筆資料
head(PPDENS_interpo)

# 計算每一特定區域面積(檢驗是否為環域的面積)
head(PP_interpo2$PP/PPDENS_interpo$PPDENS)





#######################################################################
#---4.4  聯集（Unions）---#
#######################################################################

### 兩地理資料聯集
# 取捷運站（點）和村里（面）兩地理資料的聯集
mrt_vil=st_union(taipei_mrt_station[,c("Zh_tw")], 
                 taipei_village_map[, c("VILLNAME")])

# 查看mrt_vil前六筆資料
head(mrt_vil)



### 將所有村里的空間資料取聯集，以消除村里間的界線
# 取所有村里地理資料之聯集
vil_uni=st_union(taipei_village_map)

# 查看vil_uni資料
vil_uni

# 查看vil_uni資料型態
class(vil_uni)



### 各別單一地理資料的聯集後再取聯集
# 取捷運站（點）和村里（面）兩地理資料聯集後的聯集
vil_mrt_uni=st_union(st_union(taipei_village_map), st_union(taipei_mrt_station))

# 查看vil_mrt_uni資料
vil_uni

# 查看vil_mrt_uni資料型態
class(vil_mrt_uni)





#######################################################################
#---4.5  交集（Intersection）---#
#######################################################################

### 回傳地理資料交集之索引值
# 回傳YouBike及村里交集的索引值
ubike_vil_index=st_intersects(taipei_youbike, taipei_village_map)

# 查看ubike_vil_index資料
ubike_vil_index

# 檢查ubike_vil_index的資料型態
class(ubike_vil_index)

# 將儲存索引值的陣列轉換為數值型態
ubike_vil_index=as.numeric(ubike_vil_index)

# 擷取相對應的村里屬性
ubike_village=st_drop_geometry(taipei_village_map)[ubike_vil_index, c("VILLCODE","COUNTYNAME","TOWNNAME","VILLNAME")]

# 合併YouBike資料與所在村里之屬性資料
taipei_youbike_village=cbind(taipei_youbike, ubike_village)

# 查看taipei_youbike_village前六筆資料
head(taipei_youbike_village)



### 瞭解每一捷運站所在的村里為何
# 回傳捷運站及村里交集的索引值
vil_mrt_index=st_intersects(taipei_mrt_station, taipei_village_map)

# 將儲存索引值的陣列轉換為數值型態
vil_mrt_index=as.numeric(vil_mrt_index)

# 查看vil_mrt_index資料
vil_mrt_index



### 將兩地理資料的擺放順序顛倒(各村里所含有的YouBike站點)
# 回傳村里及YouBike交集的索引值
vil_ubike_index=st_intersects(taipei_village_map, taipei_youbike)

# 查看vil_ubike_index資料
vil_ubike_index

# 計算陣列中每一陣列的元素個數
vil_ubike_count=lengths(vil_ubike_index)

# 查看vil_ubike_count資料
head(vil_ubike_count)



### st_intersectuon()函式
# 將兩地理資料取交集
ubike_vill_intersection=st_intersection(taipei_youbike, taipei_village_map)

# 查看ubike_vill_intersection前六筆資料
head(ubike_vill_intersection)



### 剪裁方法
# 先檢查原臺北捷運站點數
nrow(taipei_mrt_station)

# 將臺北捷運資料利用臺北市村里圖層進行剪裁
mrt_clip=st_intersection(taipei_mrt_station, 
                         taipei_village_map$geometry)

# 查看剪裁後的資料筆數 (在臺北市內的捷運站點數)
nrow(mrt_clip)



### 地理操作之集合操作元件
# 擷取名為「竹科新竹園區」的工業區
sipa=filter(taiwan_factory, FNAME=="竹科新竹園區")

# 擷取寶山鄉
baoshan=filter(taiwan_town, TOWNNAME=="寶山鄉")

# 檢查座標參考系統
st_crs(sipa)$epsg
st_crs(baoshan)$epsg

# 轉換座標參考系統（轉換為EPSG:3826）
sipa=st_transform(sipa, crs=3826)
baoshan=st_transform(baoshan, crs=3826)

# 科學園區與寶山鄉之差集
st_difference(sipa, baoshan)

# 寶山鄉與科學園區之差集
st_difference(baoshan, sipa)

# 科學園區與寶山鄉之交集
st_intersection(sipa, baoshan)

# 非位於科學園區與寶山鄉之交集
st_sym_difference(sipa, baoshan)





#######################################################################
#---4.6 環域（Buffer）---#
#######################################################################

### 倫敦地區的共享自行車站點
# 查看cycle_hire的EPSG代碼與單位
st_crs(cycle_hire)$epsg
st_crs(cycle_hire)$units

# 查看cycle_hire的EPSG代碼與單位
cycle_hire_buf1=st_buffer(cycle_hire, 0.0009)

# 轉換座標參考系統為27700
cycle_hire_27700=st_transform(cycle_hire, crs=27700)

# 查看cycle_hire_27700的單位
st_crs(cycle_hire_27700)$units

# 環域分析
cycle_hire_buf2=st_buffer(cycle_hire_27700, 100)

# 查看cycle_hire_buf2前六筆資料
head(cycle_hire_buf2)



### 計算各捷運站300公尺環域範圍內的YouBike站點個數
# 確認座標參考系統 (若為EPSG:3826，表示為投影座標，無須轉換)
st_crs(taipei_youbike)$epsg

# 查看cycle_hire_buf2前六筆資料
st_crs(taipei_mrt_station)$epsg

# 部分捷運站位於臺北市外，先予以去除
mrt_station=st_intersection(taipei_mrt_station, taipei_village_map$geometry)

# 去除重複捷運站點
mrt_station=mrt_station[!(duplicated(mrt_station$Zh_tw)),]

# 繪製300公尺環域範圍
mrt_station_buf=st_buffer(mrt_station, 300)

# 取交集，將YouBike場站映射至所在捷運站之環域範圍
ubike_mrt_buf=st_intersection(taipei_youbike[, c("SttnUID","StatnNm")], mrt_station_buf[, c("StationID","Zh_tw")])

# 針對同一捷運站站名，統計其資料總筆數
# (每一筆代表特定YouBike場站位於該捷運站的環域中)
# 先將ubike_mrt_buf的空間欄位去除，以避免
# group_by() %>% summarise()函式將空間資料予以合併
ubike_mrt_buf=st_drop_geometry(ubike_mrt_buf)
ubike_mrt_buf_count=group_by(ubike_mrt_buf, Zh_tw)%>%
  summarise(ubike_count=n())%>%
  arrange(desc(ubike_count))

# 查看ubike_mrt_buf_count資料
ubike_mrt_buf_count
 




#######################################################################
#---4.7  中心（Centroid）---#
#######################################################################

### 地理資料中心
# 臺北市各村里中心
tp_vill_center=st_centroid(taipei_village_map)

# 臺北捷運各路段中心
mrt_center=st_centroid(taipei_mrt_route)

# 查看tp_vill_center前六筆資料
head(tp_vill_center[, c("TOWNNAME","VILLNAME")])

# 查看mrt_center前六筆資料
head(mrt_center[, c("RAILNAME","RAILID")])



### 設定參數of_largest_polygon=T
# 擷取旗津區的面圖層
cijin=filter(taiwan_town, TOWNNAME=="旗津區")

# 未設定「最大面積之中心點」之結果
st_centroid(cijin)%>%
  st_coordinates()

# 設定「最大面積之中心點」之結果
st_centroid(cijin, of_largest_polygon=T)%>%
  st_coordinates()



### st_point_on_surface()函式求取面上其中一點
# 擷取澎湖縣的面圖層
penghu=filter(taiwan_county, COUNTYNAME=="澎湖縣")

# 面上點
st_point_on_surface(penghu)%>%
  st_coordinates()





#######################################################################
#---4.8  邊界（Boundary））---#
#######################################################################

### 美國各州邊界
st_boundary(us_states)



### 美國全國邊界
# 美國全國聯集
us_states_uni=st_union(us_states) 

# 全國邊界
st_boundary(us_states_uni)

# 繪製地圖
ggplot()+
  geom_sf(data=us_states)+
  geom_sf(data=st_boundary(us_states_uni), size=1)+
  theme_void()





#######################################################################
#---4.9  空間資料簡化（Simplification）---#
#######################################################################

### 塞納河簡化
st_simplify(seine, dTolerance=2000)

# 繪製地圖
ggplot()+
  geom_sf(data=st_simplify(seine, dTolerance=2000))+
  theme_void()



### 美國地圖簡化
us_states_2163=st_transform(us_states, crs=2163)
st_simplify(us_states_2163, dTolerance=50000)

# 繪製地圖
ggplot()+
  geom_sf(data=st_simplify(us_states_2163, dTolerance=50000))+
  theme_void()





#######################################################################
#---4.10  移動、縮放與旋轉（Shifting, Scaling and Rotation）---#
#######################################################################

### 移動美國地圖
# 轉換為投影座標系統
us_states_2163=st_transform(us_states, crs=2163)

# 查看移動前的空間欄位
us_states_2163$geometry

# 向東移動500公里，且南移動300公里
us_states_shift=us_states_2163
us_states_shift$geometry=us_states_shift$geometry+c(500000,-300000)

# 移動後，原有CRS消失，故須重新設定
us_states_shift=st_sf(us_states_shift, crs=2163)

# 查看移動後的空間欄位
us_states_shift$geometry

# 繪製地圖
ggplot()+
  geom_sf(data=us_states)+
  geom_sf(data=us_states_shift, fill=alpha("blue", 0.3))+
  theme_void()



### 縮放美國地圖
# 轉換為投影座標系統
us_states_2163=st_transform(us_states, crs=2163)

#  全域縮放(縮小兩倍，縮放常數=0.5) 
us_states_global=us_states_2163
us_states_global$geometry=us_states_global$geometry*0.5

# 區域縮放
us_states_local=us_states_2163
us_states_center=st_centroid(us_states_local)$geometry
us_states_local$geometry=(us_states_local$geometry-us_states_center)    *0.5+ us_states_center

# 縮放後原有CRS消失，故須重新設定
us_states_global=st_sf(us_states_global, crs=2163)
us_states_local=st_sf(us_states_local, crs=2163)

# 繪製地圖(us_states_global)
ggplot()+
  geom_sf(data=us_states)+
  geom_sf(data=us_states_global, fill=alpha("blue", 0.3))+
  theme_void()

# 繪製地圖(us_states_local)
ggplot()+
  geom_sf(data=us_states)+
  geom_sf(data=us_states_local, fill=alpha("blue", 0.3))+
  theme_void()



### 旋轉美國地圖
# 旋轉公式
rotation=function(x){
  r=x*pi/180
  ro=matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow=2, ncol=2)
  return(ro)
}

#  轉換為投影座標系統
us_states_rotation=st_transform(us_states, crs=2163)

# 旋轉
us_states_center=st_centroid(us_states_rotation)$geometry
us_states_rotation$geometry=(us_states_rotation$geometry-us_states_center)*rotation(30)+us_states_center

# 旋轉後原有CRS消失，故須重新設定
us_states_rotation=st_sf(us_states_rotation, crs=2163)

# 繪製地圖
ggplot()+
  geom_sf(data=us_states)+
  geom_sf(data=us_states_rotation, fill=alpha("blue", 0.3))+
  theme_void()





#######################################################################
#---4.11  空間計算（Geometric Measurement）---#
#######################################################################

### 面積計算
# 計算各村里面積
tp_vil_area=st_area(taipei_village_map)

# 查看tp_vil_area前六筆資料
head(tp_vil_area)

# 轉換數值型態與單位換算 (平方公尺轉平方公里)
tp_vil_area=as.numeric(tp_vil_area)/10^6

# 合併原始地理資料與面積計算結果
tp_vil_area=cbind(taipei_village_map, AREA=tp_vil_area)

# 查看tp_vil_area前六筆資料
head(tp_vil_area[, c("VILLCODE","COUNTYNAME","TOWNNAME",
                     "VILLNAME","AREA")])



### 長度計算
# 計算各捷運路段之長度
mrt_length=st_length(taipei_mrt_route)

# 轉換數值型態
mrt_length=as.numeric(mrt_length)

# 合併長度資料
mrt_length=cbind(taipei_mrt_route, mrt_length)

# 查看mrt_length前六筆資料
head(mrt_length[, c("RAILNAME","RAILID","mrt_length")])

# 去除mrt_length的空間資料後再依據RAILNAME計算長度之總和
group_by(st_drop_geometry(mrt_length), RAILNAME)%>%
  summarise(mrt_length=sum(mrt_length)/1000)



### 周長計算
# 擷取臺北市各村里的邊框
vill_border=st_boundary(taipei_village_map)

# 計算各邊框的長度（周長）
vil_peri1=st_length(vill_border)

# 轉換數值型態
vil_peri1=as.numeric(vil_peri1)

# 合併周長資料
tp_vil_peri=cbind(taipei_village_map, vil_peri1)

# 查看mrt_length前六筆資料
head(tp_vil_peri[, c("VILLCODE","COUNTYNAME",
                     "TOWNNAME","VILLNAME","vil_peri1")])



### 周長計算 (另一種方法)
# 安裝並載入套件
install.packages("lwgeom")
library(lwgeom)

# 計算周長
vil_peri2=st_perimeter(taipei_village_map)

# 查看vil_peri2前六筆資料
head(vil_peri2)



### 直線距離計算
# 臺北捷運站點（前五筆資料）
mrt_station=taipei_mrt_station[1:5,]
mrt_station$Zh_tw   ## 查看捷運站名

# YouBike站點（前五筆資料）
ubike_station=taipei_youbike[1:5,]
ubike_station$StatnNm   ## 查看YouBike站名

# 臺北捷運各站點間直線距離
st_distance(mrt_station)

# 臺北捷運站點與YouBike站點間直線距離
st_distance(mrt_station, ubike_station)

# 臺北捷運站點與YouBike站點間直線距離（逐欄配對）
st_distance(mrt_station, ubike_station, by_element=T)





#######################################################################
#---4.12  鄰近分析（Nearest Feature Analysis）---#
#######################################################################

### 利用st_distance()函式計算距離，挑選出最短者
# 擷取YouBike場站最接近捷運站之索引值
ubike_nearest_mrt1=apply(st_distance(taipei_youbike, taipei_mrt_station), 1, which.min)

# 查看ubike_nearest_mrt1元素個數
length(ubike_nearest_mrt1)

# 查看ubike_nearest_mrt1前六筆資料
head(ubike_nearest_mrt1)



### 利用st_nearest_feature()函式回傳最近站點之索引值
# 擷取YouBike場站最接近捷運站之索引值
ubike_nearest_mrt2=st_nearest_feature(taipei_youbike, taipei_mrt_station)

# 查看ubike_nearest_mrt2前六筆資料
head(ubike_nearest_mrt2)



### 貼附捷運站相關屬性資料
# 依照索引值擷取相對應的捷運站代碼及名稱，並去除空間資料
nearest_mrt=taipei_mrt_station[ubike_nearest_mrt2, 
                               c("StationID","Zh_tw")]%>%
  st_drop_geometry()
# 合併taipei_youbike與nearest_mrt兩資料，並去除空間資料
ubike_nearest_mrt=cbind(taipei_youbike[, c("SttnUID","StatnNm")], nearest_mrt)%>%
  st_drop_geometry()

# 查看ubike_nearest_mrt前六筆資料
head(ubike_nearest_mrt)



### 查詢多個鄰近點
# 安裝與載入套件
install.packages("nngeo")
library(nngeo)

# 擷取YouBike場站前三近捷運站之索引值
near_mrt_k3=st_nn(taipei_youbike, taipei_mrt_station, k=3)

# 將回傳結果轉換為矩陣
head(near_mrt_k3)

# 將near_mrt_k3轉換為矩陣
near_mrt_k3=matrix(unlist(near_mrt_k3), ncol=3, byrow=T)

# 擷取各別行數的資料（第一行表示最接近站點、第二行為次接近站點...）
near_mrt_k3_1=taipei_mrt_station$Zh_tw[near_mrt_k3[,1]]
near_mrt_k3_2=taipei_mrt_station$Zh_tw[near_mrt_k3[,2]]
near_mrt_k3_3=taipei_mrt_station$Zh_tw[near_mrt_k3[,3]]

# 合併YouBike原始資料與各別接近站點資料
near_mrt_k3=cbind(st_drop_geometry(taipei_youbike)[, c("StatnNm")],                         near_mrt_k3_1, near_mrt_k3_2, near_mrt_k3_3)

# 查看near_mrt_k3前六筆資料
head(near_mrt_k3)



### 擷取最近點之連線段
# 擷取捷運善導寺與忠孝敦化兩站
mrt1=filter(taipei_mrt_station, Zh_tw %in% c("善導寺", "忠孝敦化"))

# 將mrt1取環域
mrt1=st_buffer(mrt1, 300)

# mrt2擷取另外五個站點資料
mrt2=filter(taipei_mrt_station, Zh_tw %in% c("龍山寺","圓山","台電大                                                      樓","六張犁","松山機場"))

# 將mrt1(面)與mrt2(線)兩地理資料，取任兩幾何元素間最近點的線段
mrt12_near=st_nearest_points(mrt1, mrt2)

# 查看mrt12_near資料
mrt12_near

# 計算mrt1與mrt2任兩幾何元素之最近點連線的長度
st_length(mrt12_near)

# 計算mrt1與mrt2任兩幾何元素間的最短距離
st_distance(mrt1, mrt2)



### 配對最近點連線
# 各YouBike站點最接近的捷運站點（索引值）
nearest_mrt=st_nearest_feature(taipei_youbike, taipei_mrt_station)

# 回傳各個捷運站點的地理資料（透過索引值匹配地理資料）
nearest_mrt=taipei_mrt_station[nearest_mrt,]

# 利用pairwise=T逐欄擷取兩地理資料的最近點
mrt_ubike_line=st_nearest_points(nearest_mrt, taipei_youbike,
                                 pairwise=T)

# 查看mrt_ubike_line資料
mrt_ubike_line

# 繪製地圖
ggplot()+
  geom_sf(data=filter(taiwan_county, COUNTYNAME=="臺北市"))+
  geom_sf(data=mrt_ubike_line)+
  theme_void()





#######################################################################
#---4.13  凸包（Convex Hull）---#
#######################################################################

### 聯集後取凸包
# 將所有YouBike站點資料取聯集
ubike_union=st_union(taipei_youbike)

# 擷取站點聯集的凸包
ubike_union_convex=st_convex_hull(ubike_union)

# 查看ubike_union_convex資料
ubike_union_convex



### 凸包實務範例
# 先擷取所有位於臺北市的捷運站點
mrt_station=st_intersection(taipei_mrt_station, 
                            taipei_village_map$geometry)

# 尋找臺北市YouBike站點鄰近的捷運站點（索引值）
nearest_mrt=st_nearest_feature(taipei_youbike, mrt_station)

# 擷取最近捷運站點的屬性資料
nearest_mrt=mrt_station[nearest_mrt,]

# 將YouBike站點與最近捷運站屬性合併
mrt_ubike_nearest=cbind(taipei_youbike[, c("SttnUID", "StatnNm")],
                        nearest_mrt[, c("Zh_tw")])

# 將同一最近捷運站點之YouBike場站合併為多點
mrt_ubike_nearest=group_by(mrt_ubike_nearest, Zh_tw)%>%
  summarise()

# 取多點資料的凸包
mrt_ubike_nearest_convex=st_convex_hull(mrt_ubike_nearest)

# 查看mrt_ubike_nearest_convex資料
mrt_ubike_nearest_convex

# 繪製地圖
ggplot()+
  geom_sf(data=filter(taiwan_county, COUNTYNAME=="臺北市"))+
  geom_sf(data=mrt_ubike_nearest_convex, fill=alpha("red", 0.4))+
  theme_void()

# 查看mrt_ubike_nearest_convex各資料之幾何型態
st_geometry_type(mrt_ubike_nearest_convex)

# 檢驗各資料是否為LINESTRING，若是回傳TRUE；若否則為FALSE
temp=st_geometry_type(mrt_ubike_nearest_convex)=="LINESTRING"

# 回傳TRUE之站點名稱
mrt_ubike_nearest_convex$Zh_tw[temp]





#######################################################################
#---4.14  沃羅諾伊圖（Voronoi Polygon）---#
#######################################################################

### 繪製沃羅諾伊圖
# 將臺北市村里資料取聯集
tp_union=st_union(taipei_village_map)

# 擷取臺北市內的捷運站點
mrt_station=st_intersection(taipei_mrt_station, tp_union)

# 將捷運站點取聯集後，再繪製沃羅諾伊圖
mrt_vor=st_voronoi(st_union(mrt_station))

# 檢查mrt_vor的幾何型態（此時為GEOMETRYCOLLECTION）
class(mrt_vor)

# 將GEOMETRYCOLLECTION轉換為POLYGON
mrt_vor=st_collection_extract(mrt_vor, "POLYGON")

# 再次檢查mrt_vor的幾何型態（已更為POLYGON）
class(mrt_vor)

# 利用臺北市地圖裁切邊框，避免沃羅諾伊圖無限延伸
mrt_vor=st_intersection(mrt_vor, tp_union)

# 繪製地圖
ggplot()+
  geom_sf(data=mrt_vor)+
  theme_void()



### 結合kmeans與沃羅諾伊圖
# 先擷取捷運站點資料的座標值，再利用kmeans分為5群
mrt_kmeans=kmeans(st_coordinates(mrt_station), 5)

# 將分群結果與原始站點資料予以合併
taipei_mrt_kmeans=cbind(mrt_station, cluster=mrt_kmeans$cluster)

# 將中心點資料轉換為地理資料
mrt_center=data.frame(mrt_kmeans$centers)%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", X, " ", Y, ")")))%>%
  st_sf(crs=3826)

# 利用中心點繪製沃羅諾伊圖（以下程式碼與上一範例雷同）
mrt_center_vor=st_voronoi(st_union(mrt_center))
mrt_center_vor=st_collection_extract(mrt_center_vor)
mrt_center_vor=st_intersection(mrt_center_vor, tp_union)

# 繪製地圖
# 注意：由於kmeans演算法中第一步驟為「隨機指派群集中心」，每次分群的結果不相同，故本結果及書中的範例亦可能不同
ggplot()+
  geom_sf(data=mrt_center_vor)+
  geom_sf(data=taipei_mrt_kmeans, aes(color=as.character(cluster)))+
  scale_color_brewer(palette="Set1", name="群集")+
  theme_void()





#######################################################################
#---4.15  網格（Grid）---#
#######################################################################

### 建立網格
# 建立10*10的正方形區域
poly10=st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0))))

# 網格面
st_make_grid(poly10, cellsize=2, what="polygons")

# 中心點
st_make_grid(poly10, cellsize=2, what="centers")

# 邊角
st_make_grid(poly10, cellsize=2, what="corners")

# 設定參數n=，繪製5*2個網格
st_make_grid(poly10, n=c(2,5))



### 將臺北市依200*200公尺之網格進行劃分，計算每一網格的YouBike場站個數
# 將臺北市村里地圖取聯集
tp_union=st_union(taipei_village_map)

# 繪製500*500公尺之網格
tp_grid=st_make_grid(tp_union, cellsize=500)

# 繪製地圖
ggplot()+
  geom_sf(data=filter(taiwan_county, COUNTYNAME=="臺北市"), fill=alpha("blue", 0.4))+
  geom_sf(data=tp_grid, fill=NA)+
  theme_void()

# 網格結果與臺北地圖取交集（刪除超出臺北市邊界之網格）
tp_grid=st_intersection(tp_grid, tp_union)

# tp_grid的資料格式原為sfc，先轉換為sf
tp_grid=st_sf(tp_grid, crs=3826)

# 賦予每一網格唯一編碼
tp_grid=cbind(GridID=c(1:nrow(tp_grid)), tp_grid)

# 將YouBike場站與tp_grid取交集
grid_youbike=st_intersects(tp_grid, taipei_youbike)

# 計算每一交集陣列中的元素個數，即為該網格的場站數
tp_grid=cbind(tp_grid, youbike=lengths(grid_youbike))

# 查看tp_grid第351至360筆資料
tp_grid[351:360,]

# 繪製地圖
ggplot()+
  geom_sf(data=filter(taiwan_county, COUNTYNAME=="臺北市"), fill=alpha("blue", 0.4))+
  geom_sf(data=tp_grid, aes(fill=youbike))+
  scale_fill_distiller(palette="YlOrRd", direction=1)+
  theme_void()


