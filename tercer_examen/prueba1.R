
##SCRIPTS UTILIZADO PARA CREACIÓN DE GRÁFICOS A MODO BORRADOR

library(ncdf4)
library(fields)
library(kali)

# Abrir todos los archivos  ----------------------------------------------------------

data_sst = nc_open("datos/sst.nc4")
data_sss = nc_open("datos/sss.nc4")
data_lphy = nc_open("datos/lphy.nc4")
data_sphy = nc_open("datos/sphy.nc4")

#Tamaño time= 552 desde 1959 en meses hasta el año 2005
#hallar 12 meses del año 1990
#30 años desde 1959 hasta 1989. sumar 360 meses pero inicia de 0 así que 359
#360=enero, 371=diciembre de 1990

# Definir variables generales -------------------------------------------------------

sst = ncvar_get(data_sst, "to")
sss = ncvar_get(data_sss, "so")
lat = ncvar_get(data_sst, "latitude")
lon = ncvar_get(data_sst, "longitude") - 360
time = ncvar_get(data_sst, "time")
time2 = ncvar_get(data_lphy, "time")
time2 = time
lphy = ncvar_get(data_lphy, "intpp")
sphy = ncvar_get(data_sphy, "intpp")
sfito = sphy * 10^6
lfito = lphy * 10^6
totalfito = lphy + sphy

# SOLO DATOS 1990 ---------------------------------------------------------------

t1990 = time[361:372]
sst1990 = sst[,,361:372]
sss1990 = sss[,,361:372]
lphy1990 = lfito[,,361:372]
sphy1990 = sfito[,,361:372]
fito1990 = totalfito[,,361:372]

#PROMEDIO DATOS 1990

promsst90 = apply(sst1990, MARGIN = c(1,2), FUN=mean)
promsss90 = apply(sss1990, MARGIN = c(1,2), FUN=mean)
promlphy90 = apply(lphy1990, MARGIN = c(1,2), FUN=mean)
promsphy90 = apply(sphy1990, MARGIN = c(1,2), FUN=mean)

#PROMEDIO DATOS TOTAL 1959 - 2005

promsst = apply(sst, MARGIN = c(1,2), FUN=mean)
promsss = apply(sss, MARGIN = c(1,2), FUN=mean)
promlphy = apply(lfito, MARGIN = c(1,2), FUN=mean)
promsphy = apply(sfito, MARGIN = c(1,2), FUN=mean)

#ANOMALIAS 1990 CON RESPECTO A PROMEDIO TOTAL

anomsst90 = promsst - promsst90
anomsss90 = promsss - promsss90
anomlphy90 = promlphy - promlphy90
anomsphy90 = promsphy - promsphy90


# INICIO GRÁFICOS ---------------------------------------------------------

par(oma=c(0,0,0,1))

image.map(lon, lat, promsst90, legend.width = 0.6, legend.lab = "°C")
title("Distribución de temperaturas en el año 1990")

image.map(lon, lat, anomsst90)
title("Anomalías T° 1990")

# PROMEDIO MESES Y ANOMALÍA CON RESPECTO A 1990 ------------------------------------

meses = rep(1:12, length=length(t1990))
#apply(sst[,, meses==1], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
sst_mes= array(dim=c(dim(sst)[1:2], 12)) 

for (i in 1:12){
  sst_mes[,,i] = apply(sst[,, meses==i], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
}

image.map(lon, lat, sst_mes[,,1],legend.width = 0.6, legend.lab = "°C") #promedio mes 
title("Promedio T° enero" )

par(mfrow=c(3,4), mar=c(4,4,1,1)) #se coloca partición del gráfico antes de realizar el for

for (i in 1:12){
  sst_mes[,,i] = apply(sst[,, meses==i], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
  image.plot(lon, lat, sst_mes[,,i], main=paste(month.name[i]))
  
}

par(mfrow=c(1,3), mar=c(2,2,1,1)) #se coloca partición del gráfico antes de realizar el for

for (i in 1:3){
  sst_mes[,,i] = apply(sst[,, meses==i], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
  image.map(lon, lat, sst_mes[,,i], main=paste(month.name[i]))
}

###
sss_mes= array(dim=c(dim(sss)[1:2], 12)) 

par(mfrow=c(3,4), mar=c(4,4,1,1))
for (i in 1:12){
  sss_mes[,,i] = apply(sss[,, meses==i], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
  image.map(lon, lat, sss_mes[,,i], main=paste(month.name[i]))
  
}

par(mfrow=c(1,4), mar=c(2,2,1,1)) #se coloca partición del gráfico antes de realizar el for

for (i in 1:4){
  sss_mes[,,i] = apply(sss[,, meses==i], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
  image.map(lon, lat, sss_mes[,,i], main=paste(month.name[i]))
}

par(mfrow=c(2,2), mar=c(4,4,1,1))
image.map(lon, lat, sst_mes[,,1], main=paste(month.name[1]))
image.map(lon, lat, sss_mes[,,1], main=paste(month.name[1]))
image.map(lon, lat, sst_mes[,,4], main=paste(month.name[4]))
image.map(lon, lat, sss_mes[,,4], main=paste(month.name[4]))

###
anom_enero = promsst90 - sst_mes[,,1]

image.map(lon, lat, anom_enero, legend.width = 0.6, legend.lab = "°C")
title("anomalia enero")

# ESTACIONES --------------------------------------------------------------

#VERANO = 361:363
#OTOÑO = 364:366
#INVIERNO = 367:369
#PRIMAVERA = 370:372


# VERANO ------------------------------------------------------------------

prom_verano = apply(sst[,,361:363], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
image.map(lon, lat, prom_verano)
title("Distribución de temperaturas Verano 1990")

anom_ver = promsst90 - prom_verano
image.map(lon, lat, anom_ver)
title("Anomalías de temperatura en verano")

# INVIERNO ----------------------------------------------------------------

prom_invierno = apply(sst[,,367:369], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
image.map(lon, lat, prom_invierno)
title("Distribución de temperaturas Invierno 1990")

anom_inv = promsst90 - prom_invierno
image.map(lon, lat, anom_inv)
title("Anomalías de temperatura en invierno")

##HOVMULLER

par(mfrow=c(1,1))
hov1 = apply(sst[,,361:372], MARGIN = c(2,3), FUN = mean, na.rm = TRUE) #promedio SST en todo el tiempo

Hovmuller1=image.plot(1:12, lat, t(hov1), las=1,ylim=c(-20,0), xlab="Tiempo", ylab="Latitud", axes=FALSE)

title("Variación de temperatura a lo largo de 1990")
ejey = paste(abs(axTicks(2)), ifelse(axTicks(2)>0, "°N", "°S"), sep="") 
axis(2, at=axTicks(2), labels=ejey, las=1) 
points(-5,1) #truco para retomar el foco en el eje, se buscará una mejor solución
axis(1, at=1:12, labels=month.abb) #agregar meses en eje x

##########################


RES=sweep(sst1990, c(1,2), promsst, "-") 

par(mfrow=c(1,1))
hov1 = apply(RES, MARGIN = c(2,3), FUN = mean, na.rm = TRUE) #promedio SST en todo el tiempo

Hovmuller1=image.plot(1:12, lat, t(hov1), las=1,ylim=c(-20,0), xlab="Tiempo", ylab="Latitud", axes=FALSE)

title("Anomalías de temperatura a lo largo de 1990")
ejey = paste(abs(axTicks(2)), ifelse(axTicks(2)>0, "°N", "°S"), sep="") 
axis(2, at=axTicks(2), labels=ejey, las=1) 
points(-5,1) #truco para retomar el foco en el eje, se buscará una mejor solución
axis(1, at=1:12, labels=month.abb) #agregar meses en eje x


#

prom=apply(sst1990, MARGIN=3,FUN=mean, na.rm=TRUE)
GRAF6=plot(1:12, prom,type="l", col="purple",las=1, xlab="Tiempo",ylab="Temperatura promedio", axes = FALSE)
ejey = paste(abs(axTicks(2)), ifelse(axTicks(2)>0, "°c"), sep="") 
axis(2, at=axTicks(2), labels=ejey, las=1) 
points(-5,1) #truco para retomar el foco en el eje, se buscará una mejor solución
axis(1, at=1:12, labels=month.abb) #agregar meses en eje x
box()
title("Variación de la T° a lo largo del año 1990")

# SALINIDAD ---------------------------------------------------------------


data_sss = nc_open("datos/sss.nc4")
sss = ncvar_get(data_sss, "so")

sss_promedio = apply(sss[,,1:552], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
#image.map(lon, lat, sss_promedio)

prom_sss_1990 = apply(sss[,,361:372], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
anom_sss = sss_promedio - prom_sss_1990
image.map(lon, lat, anom_sss)

image.map(lon, lat, anomsss90, legend.lab = "UPS", main="Anomalías de salinidad 1990")



par(mfrow=c(2,1), mar=c(0,0,0,0), oma=c(5,5,3,3))

image.map(lon, lat, promsss90, legend.lab = "UPS", main="hola")

image.map(lon, lat, anomsss90, legend.lab = "UPS")



# FITOPLANCTON ------------------------------------------------------------

meses = rep(1:12, length=length(t1990))
sphy_mes= array(dim=c(dim(sfito)[1:2], 12)) 

for (i in 1:12){
  sphy_mes[,,i] = apply(sfito[,, meses==i], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
}

image.map(lon, lat, sphy_mes[,,1],legend.width = 0.6, legend.lab = "Fito pequeño") #promedio mes 
title("Promedio  enero" )

#

image.map(lon, lat, sphy_mes[,,3],legend.width = 0.6, legend.lab = "Fito pequeño") #promedio mes 
title("Promedio  marzo" )

#

par(mfrow=c(3,4), mar=c(4,4,1,1)) #se coloca partición del gráfico antes de realizar el for

for (i in 1:12){
  sst_mes[,,i] = apply(sst[,, meses==i], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
  image.plot(lon, lat, sst_mes[,,i], main=paste(month.name[i]))
  
}

par(mfrow=c(1,3), mar=c(2,2,1,1)) #se coloca partición del gráfico antes de realizar el for

for (i in 1:3){
  sst_mes[,,i] = apply(sst[,, meses==i], MARGIN=c(1,2), FUN=mean, na.rm=TRUE)
  image.map(lon, lat, sst_mes[,,i], main=paste(month.name[i]))
}










data_lphy = nc_open("datos/lphy.nc4") #large phytoplankton
lphy = ncvar_get(data_lphy, "intpp")

lphy_promedio = apply(lphy[,,1:552], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
#image.map(lon, lat, lphy_promedio)

prom_lphy_1990 = apply(lfito[,,360:372], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
#anom_lphy = lphy_promedio - prom_lphy_1990
#image.map(lon, lat, anom_lphy)
image.map(lon, lat, promlphy90) #creo que para fito se deberia usar este

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

