library(sf)
library(here)

#读取数据
st_layers('/Users/johnnie/Documents/CASA/05/WEEK3/homework/data/gadm41_CHN.gpkg')


#将数据新建在一个文件中
CHNline <- st_read('/Users/johnnie/Documents/CASA/05/WEEK3/homework/data/gadm41_CHN.gpkg')


#读坐标系
st_crs(CHNline)$proj4string



#读tif文件
library(raster)
library(terra)

ssp1<-terra::rast('/Users/johnnie/Documents/CASA/05/WEEK3/homework/data/wc2.1_2.5m_tmax_ACCESS-CM2_ssp126_2081-2100.tif')
ssp5<-terra::rast('/Users/johnnie/Documents/CASA/05/WEEK3/homework/data/wc2.1_2.5m_tmax_ACCESS-CM2_ssp585_2081-2100.tif')

#求差
difference<-(ssp5-ssp1)

difference

#
#plot(difference)

#重命名
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(difference) <- month


##
##
#读CSV
library(readr)
Worldcity <- read_csv("/Users/johnnie/Documents/CASA/05/WEEK3/homework/data/World_Cities.csv")


#选择行，找出中国
library(stringr)
library(dplyr)
CHNcities<- Worldcity %>% 
  filter(str_detect(CNTRY_NAME, "China"))


#选择列，找经纬度
CHNXY<-CHNcities %>% 
  dplyr::select("X","Y","CNTRY_NAME","CITY_NAME")

class(CHNXY)



library(sf)

# 将表格转换为空间对象（假设原坐标系是 UTM Zone 50N，即 EPSG:32650）
CHNcities_sf <- st_as_sf(CHNXY, coords = c("X", "Y"), crs = 3857)

# 转换为经纬度坐标系（WGS84）
CHNcities2 <- st_transform(CHNcities_sf, crs = 4326)


#提取经纬度
CHNcities2 <- CHNcities2 %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )


CHNcities_final <- CHNcities2 %>%
  st_drop_geometry() %>%    # 删除 geometry 列
  #select(lon, lat, CNTRY_NAME, CITY_NAME)
  select(lon, lat)

class(CHNcities_final)


#tif文件和差值链接
CHNcitytemp<- terra::extract(difference, CHNcities_final)


#绘制中国轮廓
CHNSIMPLE <- CHNline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()


#合并图层前，检查是否在同一坐标系
print(CHNline)
crs(difference)


#绘图1
CHNtemp <- CHNline %>%
  # now crop our temp data to the extent
  terra::crop(difference,.)

# plot the output
plot(CHNtemp)


#绘图2
library(ggplot2)

exactCHN<-terra::mask(CHNtemp, CHNline)

#中国三月
hist(exactCHN[[3]], col="red", main="mar")

#将空间数据变成数据框
exactCHNdf <- exactCHN %>%
  as.data.frame()


library(tidyr)
squishdata<-exactCHNdf%>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "Month",
    values_to = "Temp"
  )


CHN_complete <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

CHN_complete <- CHN_complete %>%
  filter(Temp > -50 & Temp < 60)


# Plot faceted histogram
ggplot(CHN_complete, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black",fill = "skyblue", binwidth = 0.5)+
  labs(title="Ggplot2 faceted histogram of the difference for CHINA in 2081-2100", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))




summary(CHN_complete$Temp)
hist(CHN_complete$Temp)

names(exactCHNdf)


library(usethis)
edit_git_config()


library(gitcreds)
