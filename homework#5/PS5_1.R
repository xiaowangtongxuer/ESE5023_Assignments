setwd('D:/ESEHW#5') 
getwd()
library(sp)
library(rgdal)
library(raster)
library(viridis)
#1.1 加载数据,并且分别求取月平均太阳辐射，月平均风速以及年总降水
# 定义一个参数为路径，可以读取该路径下所有tif文件的函数
calculate_total <- function(filepath){
        filenames <- dir(filepath)
        for (i in 1:12){
                filename <- paste(filepath,filenames[i],sep='/')
                if (i ==1 ){
                        totaldata <- raster(filename)
                        next
                }
                totaldata = totaldata + raster(filename) 
                #raster()读取后可以直接加减运算
        }
        return (totaldata)
}

path_prec <- "D:/ESEHW#5/wc2.1_2.5m_prec"
prec <- calculate_total(path_prec)  #求得年总降水
plot(prec,  main="Precipitation(mm/year)", col = terrain.colors(10), 
     xlab = "Long", ylab="Lat",ylim = c(-90,90))
path_srad <- "D:/ESEHW#5/wc2.1_2.5m_srad" 
srad <- calculate_total(path_srad) /12 * 365 #求得年总太阳辐射
plot(srad,  main="Srad(kJ/(m^2·year))", col = terrain.colors(10), 
     xlab = "Long", ylab="Lat")
path_wind <- "D:/ESEHW#5/wc2.1_2.5m_wind"
wind <- calculate_total(path_wind) / 12 #求得年平均风速
plot(wind,  main="Wind(m/s)", col = terrain.colors(10), 
     xlab = "Long", ylab="Lat")


#-----------------------------------------------------------------------
#1.2 绘制图表
China_map <- readOGR("D:/ESEHW#5", "bou2_4p") #加载中国地图
plot(China_map)

# Crop the raster
prec_crop <- crop(prec, China_map) #用crop粗略裁剪区域
prec_mask <- mask(prec_crop,China_map) #用mask精确裁剪
srad_crop <- crop(srad, China_map)
srad_mask <- mask(srad_crop,China_map)
wind_crop <- crop(wind, China_map)
wind_mask <- mask(wind_crop,China_map)

plot(prec_mask,  main="Precipitation(mm/year)", col = rainbow(20), 
     xlab = "Long", ylab="Lat")
plot(srad_mask,  main="Srad(kJ/(m^2·year))", col = terrain.colors(20), 
     xlab = "Long", ylab="Lat")
plot(wind_mask,  main="Wind(m/s)", col = terrain.colors(16), 
     xlab = "Long", ylab="Lat")

#-----------------------------------------------------------------------
#1.3
image(wind_mask,col = terrain.colors(10))
image(wind_mask, zlim =c(4 , 7),add = T ,col = 'red')

w <- wind_mask > 3.5 #筛选风速大于4的区域，得到布尔类型

plot(w,col = c("skyblue","orange"),   
     xlab = "Long", ylab="Lat",legend=FALSE)
legend("top", legend=c("Unselected","Selected"),   #图例内容
       col=c("skyblue","orange"), lty=2,lwd=4)          #图例颜色

#---------------------------------------------------------------------
#1.4
#安装report 要求进行筛选
k <-  (srad_mask > 5852000 & prec_mask < 200)
plot(k,col = c("skyblue","orange"),xlab = "Long", ylab="Lat",legend=FALSE)
legend("top", legend=c("Unselected","Selected"),   #图例内容
       col=c("skyblue","orange"), lty=2,lwd=4)



# good work



