library(dplyr)
library(sf)
library(TWspdata)
library(ggplot2)
library(TDX)
library(tmap)
library(ggsflabel)

#---TDX套件中的函式需要金鑰，請在以下網站中申請！---#
# https://tdx.transportdata.tw/register
# 申請後將可取得一組client_id與client_secret
# access_token=get_token(client_id, client_secret)
# 以下程式碼中的access_token即表示存取權杖


tmap_mode("view")

windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))



# 擷取雙和地區的村里地圖
shuangho_village=filter(taiwan_village, TOWNNAME %in% c("中和區","永和區"))%>%
  st_transform(crs=3826)

# 將雙和地區房價資料中的地址進行地理編碼
address_geocoding=Geocoding(access_token, shuangho_house_price$address, dtype="sf")

# 擷取成功地理編碼的地址
address_geocoding=address_geocoding$SUCCESS

# 將房價資料與地理編碼結果合併
shuangho_hp=left_join(shuangho_house_price, address_geocoding, by=c("address"="AddressOriginal"))

# 將房價資料轉換為sf格式，並將座標系統轉換為EPSG:3826
shuangho_hp=select(shuangho_hp, -AddressNew)%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)


ggplot()+
  geom_sf(data=shuangho_village, fill="#BEBEBE", color="#E0E0E0")+
  geom_sf(data=shuangho_hp, aes(size=total_price/1000, color=state))+
  scale_color_brewer(palette="Set2", name="建物類型")+
  scale_size_continuous(range=c(0,6), name="房價(萬)")+
  geom_sf_text_repel(data=filter(taiwan_town, TOWNNAME %in% c("中和區","永和區")), aes(label=TOWNNAME), family="A", size=6)+
  geom_sf(data=filter(taiwan_town, TOWNNAME %in% c("中和區","永和區")), fill=NA, size=1)+
  theme_void()+
  theme(legend.title=element_text(family="A", face="bold", size=18),
        legend.text=element_text(family="A", size=15))


# 擷取臺北捷運站點資料
mrt_station=Rail_Station(access_token, "TRTC", dtype="sf")

# 將捷運站點資料的座標系統轉換為EPSG:3826
mrt_station=st_transform(mrt_station, crs=3826)

# 尋找每一家戶最鄰近的捷運站點
near_mrt=mrt_station[st_nearest_feature(shuangho_hp, mrt_station),]

# 將匹配最接近捷運站貼附於房價資料中
shuangho_hp$nearest_mrt=near_mrt$StationName

# 計算每一家戶相對應鄰近站點間的距離
shuangho_hp$mrt_dist=as.numeric(st_distance(shuangho_hp, near_mrt, by_element=T))

# 整理全臺灣各村里人口資料
village_pop=taiwan_village_pop[, c("VILLCODE", "people_total")]
village_pop$VILLCODE=as.character(village_pop$VILLCODE)

# 將雙和地區村里與人口資料進行合併
shuangho_village=left_join(shuangho_village, village_pop)

# 計算人口密度 (村里人口數/村里面積)
shuangho_village$pop_density=shuangho_village$people_total/(as.numeric(st_area(shuangho_village))/1000000)

# 將房價資料與雙和地區村里資料進行交集，貼附人口相關屬性
shuangho_hp=st_intersection(shuangho_hp, shuangho_village[, c("TOWNNAME", "VILLNAME", "VILLCODE", "people_total", "pop_density")])

# 將convenience_store資料轉換為sf格式
convenience_store=mutate(convenience_store, geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)

# 取每一家戶的300公尺環域
hp_buffer=st_buffer(shuangho_hp, 300)

# 將每一家戶300公尺環域範圍與便利商店圖資取交集
hp_conv=st_intersection(hp_buffer, convenience_store)

# 計算每一家戶300公尺範圍內的便利商店總數
hp_conv=st_drop_geometry(hp_conv)%>%
  group_by(houseid)%>%
  summarise(conv_store=n())

# 將雙和地區房價資料與便利商店統計數值予以合併
shuangho_hp=left_join(shuangho_hp, hp_conv)

# 將便利商店統計欄位中為NA者，強制轉換為0 (否則後續無法建構迴歸模型)
shuangho_hp$conv_store=ifelse(is.na(shuangho_hp$conv_store), 0, shuangho_hp$conv_store)

# 建立房價迴歸模型
lm_hp=lm(total_price/10000 ~ building_area+room+hall+bath+state+with_1st_floor+only_4th_floor+pop_density+mrt_dist+conv_store, data=shuangho_hp)

# 迴歸模型報表
summary(lm_hp)


