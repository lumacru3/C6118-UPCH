library(ncdf4)
library(fields)
library(kali)

data_sst = nc_open("datos/sst.nc4")

#Tamaño time= 552 desde 1959 en meses hasta el año 2005
#hallar 12 meses del año 1990
#30 años desde 1959 hasta 1989. sumar 360 meses pero inicia de 0 así que 359
#360=enero, 372=diciembre de 1990

###anomalias temperatura

sst = ncvar_get(data_sst, "to")
lat = ncvar_get(data_sst, "latitude")
lon = ncvar_get(data_sst, "longitude") - 360

sst_promedio = apply(sst[,,1:552], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
#image.map(lon, lat, sst_promedio)

prom_sst_1990 = apply(sst[,,360:372], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
anom_sst = sst_promedio - prom_sst_1990
image.map(lon, lat, anom_sst)

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

image.map(lon, lat, prom_sphy_verano) 
image.map(lon, lat, prom_sphy_invierno) 

par(mfrow=c(2,1), mar=c(0,0,0,0), oma=c(5,5,3,3))

image.map(lon, lat, prom_sphy_verano) 
image.map(lon, lat, prom_sphy_invierno) 
