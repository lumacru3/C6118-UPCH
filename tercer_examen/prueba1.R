library(ncdf4)
library(fields)
library(kali)

data_sst = nc_open("datos/sst.nc4") #abrir archivo de temperatura

#Tamaño time= 552 desde 1959 en meses hasta el año 2005
#hallar 12 meses del año 1990
#30 años desde 1959 hasta 1989. sumar 360 meses pero inicia de 0 así que 359
#360=enero, 371=diciembre de 1990

sst = ncvar_get(data_sst, "to")
lat = ncvar_get(data_sst, "latitude")
lon = ncvar_get(data_sst, "longitude") - 360
time = ncvar_get(data_sst, "time")
solo1990 = time[361:372]
prof = ncvar_get(data_sst, "zt_ocean") # solo tiene un valor. Profundidad = 5 metros

#Crear arreglo para meses
meses = rep(1:12, length=length(solo1990))

sst[,, meses==1]
apply(sst[,, meses==1], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
clim= array(dim=c(dim(sst)[1:2], 12)) 

for (i in 1:12){
  clim[,,i] = apply(sst[,, meses==i], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
}

image.map(lon, lat, clim[,,1]) #promedio mes de enero
title("Promedio T° Enero")

#indexando directamente

sst_promedio = apply(sst[,,1:552], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
image.map(lon, lat, sst_promedio)

prom_sst_1990 = apply(sst[,,361:372], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
anom_sst = sst_promedio - prom_sst_1990
image.map(lon, lat, anom_sst)
title("Anomalías T° 1990")

##posible hov

par(mfrow=c(1,1))
hov1 = apply(sst[,,361:372], MARGIN = c(2,3), FUN = mean, na.rm = TRUE) #promedio SST en todo el tiempo

Hovmuller1=image.plot(1:12, lat, t(hov1), las=1,ylim=c(-20,0), xlab="Tiempo", ylab="Latitud", axes=FALSE)

title("Variación de temperatura a lo largo de 1990")
ejey = paste(abs(axTicks(2)), ifelse(axTicks(2)>0, "°N", "°S"), sep="") 
axis(2, at=axTicks(2), labels=ejey, las=1) 

points(-5,1) #truco para retomar el foco en el eje, se buscará una mejor solución
axis(1, at=1:12, labels=month.abb) #agregar meses en eje x


###anomalias salinidad

data_sss = nc_open("datos/sss.nc4")
sss = ncvar_get(data_sss, "so")

sss_promedio = apply(sss[,,1:552], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
#image.map(lon, lat, sss_promedio)

prom_sss_1990 = apply(sss[,,360:372], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
anom_sss = sss_promedio - prom_sss_1990
image.map(lon, lat, anom_sss)

### anomalias pH

data_ph = nc_open("datos/ph.nc4")
ph = ncvar_get(data_ph, "ph_total")

ph_promedio = apply(ph[,,1:552], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
#image.map(lon, lat, ph_promedio)

prom_ph_1990 = apply(ph[,,360:372], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
anom_ph = ph_promedio - prom_ph_1990
image.map(lon, lat, anom_ph)

## promedio? fito L

data_lphy = nc_open("datos/lphy.nc4") #large phytoplankton
lphy = ncvar_get(data_lphy, "intpp")

lphy_promedio = apply(lphy[,,1:552], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
#image.map(lon, lat, lphy_promedio)

prom_lphy_1990 = apply(lphy[,,360:372], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
#anom_lphy = lphy_promedio - prom_lphy_1990
#image.map(lon, lat, anom_lphy)
image.map(lon, lat, prom_lphy_1990) #creo que para fito se deberia usar este

##### fito S promedio anual?

data_sphy = nc_open("datos/sphy.nc4") #small phytoplankton
sphy = ncvar_get(data_sphy, "intpp")

sphy_promedio = apply(sphy[,,1:552], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
#image.map(lon, lat, lphy_promedio)

prom_sphy_1990 = apply(sphy[,,360:372], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)

prom_sphy_verano = apply(sphy[,,360:362], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
prom_sphy_invierno = apply(sphy[,,366:368], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
#anom_sphy = sphy_promedio - prom_sphy_1990
#image.map(lon, lat, anom_sphy)
image.map(lon, lat, prom_sphy_1990) 

image.map(lon, lat, prom_sphy_verano, main="Promedio Fitoplancton verano")

image.map(lon, lat, prom_sphy_invierno,  main="Promedio Fitoplancton invierno") 

par(mfrow=c(2,1), mar=c(0,0,0,0), oma=c(5,5,3,3))

image.map(lon, lat, prom_sphy_verano) 
image.map(lon, lat, prom_sphy_invierno) 

image.plot(lat, sphy[,,366:368]) 
