library(ncdf4)
library(fields)
library(kali)
nc = nc_open("datos/sst.nc4")
sst = ncvar_get(nc, "to")
lat = ncvar_get(nc, "latitude")
lon = ncvar_get(nc, "longitude") - 360
image.map(lon, lat, sst[,,1])