###-----------------------------------------------------------------------------------------###
# 1. clear the memory and load the packages
# clear workspace and increase memory
rm(list = ls())
memory.limit(size = 1.75e13) 

###-----------------------------------------------------------------------------------------###

# diretório temporario de processamento do R 

tempdir <- function() "D:\\temps"
unlockBinding("tempdir", baseenv())
assignInNamespace("tempdir", tempdir, ns="base", envir=baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())
tempdir()

CRS.new=CRSargs(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

###-----------------------------------------------------------------------------------------###

# Pacotes

if(!require(pacman)) install.packages("pacman")
pacman::p_load(raster,sp, maptools, rgdal, corrplot, RStoolbox, vegan, psych, data.table,rasterVis)

# check loaded packets
search()

###-----------------------------------------------------------------------------------------###

# 2. import data
# directory

col.bioclim <- c(
  'Annual.Mean.Temperature', #bio1
  'Mean.Monthly.Temperature.Range', #bio1
  'Isothermality', #bio3
  'Temperature.Seasonality.STD.x.100',#bio4
  'Max.Temperature.of.Warmest.Month',#bio5
  'Min.Temperature.of.Coldest.Month',#bio6
  'Temperature.Annual.Range',#bio7
  'Mean.Temperature.of.Wettest.Quarter',#bio8
  'Mean.Temperature.of.Driest.Quarter',#bio9
  'Mean.Temperature.of.Warmest.Quarter',#bio10
  'Mean.Temperature.of.Coldest.Quarter',#bio11
  'Annual.Precipitation',#bio12
  'Precipitation.of.Wettest.Month',#bio13
  'Precipitation.of.Driest.Month',#bio14
  'Precipitation.Seasonality.CV',#bio15
  'Precipitation.of.Wettest.Quarter',#bio16
  'Precipitation.of.Driest.Quarter',#bio17
  'Precipitation.of.Warmest.Quarter',#bio18
  'Precipitation.of.Coldest.Quarter') #bio19

#--- Mundo ---#

#--- bioclim ---#

bi.names.order=c(paste0("bio0", 1:9), paste0("bio", 10:19))
bi.names.native=c("bio01", paste0("bio", 10:19), paste0("bio0", 2:9))
pr.names.month.native=(c('pr01',paste0('pr',10:12),paste0('pr0',2:9)))
tn.names.month.native=(c('tn01',paste0('tn',10:12),paste0('tn0',2:9)))
tx.names.month.native=(c('tx01',paste0('tx',10:12),paste0('tx0',2:9)))
ta.names.month.native=(c('ta01',paste0('ta',10:12),paste0('ta0',2:9)))


#--- mundo
pre.path.bi <- 'E:/environmental_data/bioclim/bioclim_v01/current/bil/2_5m/bio_2-5m_bil'
pre.path.pr <- 'E:/environmental_data/bioclim/bioclim_v01/current/bil/2_5m/prec_2-5m_bil'
pre.path.tn <- 'E:/environmental_data/bioclim/bioclim_v01/current/bil/2_5m/tmin_2-5m_bil'
pre.path.tx <- 'E:/environmental_data/bioclim/bioclim_v01/current/bil/2_5m/tmax_2-5m_bil'
pre.path.ta <- 'E:/environmental_data/bioclim/bioclim_v01/current/bil/2_5m/tmean_2-5m_bil'

mid.path.bi <- 'E:/environmental_data/bioclim/bioclim_v01/past/mid/2_5m/ccmid/ccmidbi_2-5m'
mid.path.pr <- 'E:/environmental_data/bioclim/bioclim_v01/past/mid/2_5m/ccmid/ccmidpr_2-5m'
mid.path.tn <- 'E:/environmental_data/bioclim/bioclim_v01/past/mid/2_5m/ccmid/ccmidtn_2-5m'
mid.path.tx <- 'E:/environmental_data/bioclim/bioclim_v01/past/mid/2_5m/ccmid/ccmidtx_2-5m'

lgm.path.bi <- 'E:/environmental_data/bioclim/bioclim_v01/past/lgm/2_5m/cclgm/cclgmbi_2-5m'
lgm.path.pr <- 'E:/environmental_data/bioclim/bioclim_v01/past/lgm/2_5m/cclgm/cclgmpr_2-5m'
lgm.path.tn <- 'E:/environmental_data/bioclim/bioclim_v01/past/lgm/2_5m/cclgm/cclgmtn_2-5m'
lgm.path.tx <- 'E:/environmental_data/bioclim/bioclim_v01/past/lgm/2_5m/cclgm/cclgmtx_2-5m'


lig.path.bi <- 'E:/environmental_data/bioclim/bioclim_v01/past/lig/bio'
lig.path.pr <- 'E:/environmental_data/bioclim/bioclim_v01/past/lig/prec'
lig.path.tn <- 'E:/environmental_data/bioclim/bioclim_v01/past/lig/tmin'
lig.path.tx <- 'E:/environmental_data/bioclim/bioclim_v01/past/lig/tmax'

adm.path <- 'E:/environmental_data/gadm/gadm28.shp'
veg.lgm.path <- 'E:/environmental_data/VegetationMapWorldLGM/world_cut.shp'


#------------------------------------------------------------------------#
# mascara

mask.path <- 'E:/environmental_data/mask'; setwd(mask.path)
mascara=readShapePoly("Lowenberg_Neto_Neotropics.shp")

#------------------------------------------------------------------------#
# adm
setwd(adm.path)
adm=readShapePoly("gadm28.shp")

# cortar
adm.neo <- crop(adm, extent(mascara))
dir.create('neo'); setwd('neo'); getwd()
writePolyShape( adm.neo, "neo_gadm28.shp")   

#------------------------------------------------------------------------#
# veg.lgm
setwd(veg.lgm.path)
veg.lgm=readShapePoly("world_cut.shp")
# cortar
veg.lgm.neo <- crop(veg.lgm, extent(mascara))
dir.create('neo'); setwd('neo'); getwd()
writePolyShape( veg.lgm.neo, "neo_world_cut.shp")   

#------------------------------------------------------------------------#
# http://dev.maxmind.com/geoip/legacy/geolite/

city.path <- 'E:/environmental_data/GeoLite/GeoLiteCity-latest'; setwd(city.path)
city.pto <- fread('GeoLiteCity-Location.csv')
city.wld <- SpatialPointsDataFrame(cbind(city.pto$longitude,city.pto$latitude),city.pto)
proj4string(city.wld) <- CRS.new 
# cortar
city.wld.neo <- crop(city.wld, extent(mascara))
dir.create('neo'); setwd('neo'); getwd()
writePointsShape(city.wld.neo, "neo_GeoLiteCity-Location.shp")   

#------------------------------------------------------------------------#
#NGA GEOnet Names Server

city.path <- 'E:/environmental_data/NGA GEOnet Names Server/geonames_20171002'; setwd(city.path)
city.GEOnet.pto <- fread('Countries.txt')

names.col = colnames(city.GEOnet.pto)
colnames(city.GEOnet.pto) = c("RC", "UFI",  "UNI", "LAT", "LONG", "DMS_LAT", "DMS_LONG",
                              "MGRS", "JOG", "FC", "DSG", "PC", "CC1", "ADM1", "POP", "ELEV",
                              "CC2", "NT", "LC", "SHORT_FORM", "GENERIC", 
                              "SN_RO", "FN_RO", "FN_ND_RO", "SN_RG", "FN_RG", "FN_ND_RG",
                              "NOTE", "MOD_DT", "DISPLAY", "N_RANK", "N_LINK", "TRANSL_CD",
                              "NM_MOD_DT", "F_EFCTV_DT", "F_TERM_DT")  
                            
Encoding(city.GEOnet.pto) = "UTF-8"
city.GEOnet <- SpatialPointsDataFrame(cbind(city.GEOnet.pto$LONG,city.GEOnet.pto$LAT),city.GEOnet.pto)
# proj4string(city.GEOnet) <- CRS.new 
# cortar
city.GEOnet.neo <- crop(city.GEOnet, extent(mascara))
proj4string(city.GEOnet.neo) <- CRS.new 
dir.create('neo'); setwd('neo'); getwd()
writePointsShape( city.GEOnet.neo, "neo_Countries.shp") #; plot(city.GEOnet.neo)   

#------------------------------------------------------------------------#
# karst

path.ev <- 'E:/environmental_data/WMCROv3'; setwd(path.ev)
karst_south_america <- readOGR(dsn=path.ev, layer="South_American_Karst")
karst_south_america.df = data.frame(karst_south_america)
karst_south_america = SpatialPolygonsDataFrame(karst_south_america,karst_south_america.df)
# cortar
karst_south_america.neo <- crop(karst_south_america, extent(mascara))
plot(karst_south_america)
proj4string(karst_south_america.neo) <- CRS.new 

dir.create('neo'); setwd('neo'); getwd()
writePolyShape( karst_south_america.neo, "neo_world_cut.shp")   

#------------------------------------------------------------------------#
#bacias
path.ana <- 'E:/environmental_data/ana'; setwd(path.ana)
ana.bacias = readShapePoly("baciashidrobr.shp"); proj4string(ana.bacias) <- CRS.new
# cortar
ana.bacias2 <- crop(ana.bacias, extent(mascara))
writePolyShape( ana.bacias2, "baciashidrobr2.shp")   
#------------------------------------------------------------------------#

# #KoppenBrazil2013
# path.koppen <- 'E:/environmental_data/KoppenBrazil2013/Raster'; setwd(path.koppen)
# tif <- list.files(patt = "ovr")
# dtc <- stack(tif); names(dtc)
# #names(dtc) <- bi.names.native; names(dtc)
# 
# e <- extent( dtc )
# p <- as(e, 'SpatialPolygons')
# crs(dtc) <- CRS.new 
#   "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# 
#   proj4string(dtc) <- CRS.new 
#   
#   shapefile(dtc, 'file.shp',overwrite=T)
# 
# 
# dtc.neo <- mask(dtc, ana.bacias)
# dtc.neo <- crop(dtc.neo, extent(ana.bacias))
# 
# proj4string(dtc.neo) <- CRS.new 
# shapefile(dtc.neo, 'file.shp',overwrite=T)
# 
# koppen.neo <- polygonizer(dtc,'koppenshp', 'C:\\Program Files\\GRASS GIS 7.2.2\\extrabin' )
# 
# 
# koppen.neo <- polygonizer(x=dtc,outshape='koppenshp', pypath='C:\\Program Files\\GRASS GIS 7.2.2\\extrabin',
#                           readpoly=F, fillholes=FALSE, aggregate=T,  quietish=F
#                           )
# 
# 
# 
# dir.create('neo'); setwd('neo'); getwd()
# writeRaster(dtc, filename=c('koppen_paper'), format="EHdr", bylayer=T,overwrite=TRUE)   # bil
# writeRaster(dtc, filename=c('koppen_paper'), format="raster", bylayer=T,overwrite=TRUE)   # grd

#------------------------------------------------------------------------#


# current

# bio
setwd(pre.path.bi)
tif <- list.files(patt = "bil")
dtc <- stack(tif); names(dtc)
names(dtc) <- bi.names.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=bi.names.native, format="GTiff", bylayer=T,overwrite=TRUE)   

# pr
setwd(pre.path.pr)
names.month.native <- pr.names.month.native
tif <- list.files(patt = "bil")
dtc <- stack(tif); names(dtc)
names(dtc) <- pr.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=names.month.native, format="GTiff", bylayer=T,overwrite=TRUE)   

# tn
setwd(pre.path.tn)
names.month.native <- tn.names.month.native
tif <- list.files(patt = "bil")
dtc <- stack(tif); names(dtc)
names(dtc) <- tn.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
# dtc.neo <- dtc.neo/10
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=names.month.native, format="GTiff", bylayer=T,overwrite=TRUE)   

# tx
setwd(pre.path.tx)
names.month.native <- tx.names.month.native
tif <- list.files(patt = "bil")
dtc <- stack(tif); names(dtc)
names(dtc) <- tx.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
# dtc.neo <- dtc.neo/10
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=names.month.native, format="GTiff", bylayer=T, overwrite=TRUE)   


# ta
setwd(pre.path.ta)
tif <- list.files(patt = "bil")
dtc <- stack(tif); names(dtc)
names(dtc) <- ta.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
# dtc.neo <- dtc.neo/10
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=names.month.native, format="GTiff", bylayer=T, overwrite=TRUE)   

#------------------------------------------------------------------------#

# mid

# bio
setwd(mid.path.bi)
tif <- list.files(patt = ".tif")
dtc <- stack(tif); names(dtc)
names(dtc) <- bi.names.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=bi.names.native, format="GTiff", bylayer=T,overwrite=TRUE)   

# pr
setwd(mid.path.pr)
tif <- list.files(patt = ".tif")
dtc <- stack(tif); names(dtc)
names(dtc) <- pr.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=pr.names.month.native, format="GTiff", bylayer=T,overwrite=TRUE)   

# tn
setwd(mid.path.tn)
tif <- list.files(patt = "tif")
dtc <- stack(tif); names(dtc)
names(dtc) <- tn.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
# dtc.neo <- dtc.neo/10
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=tn.names.month.native, format="GTiff", bylayer=T,overwrite=TRUE)   

# tx
setwd(mid.path.tx)
tif <- list.files(patt = ".tif")
dtc <- stack(tif); names(dtc)
names(dtc) <- tx.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
# dtc.neo <- dtc.neo/10
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=tx.names.month.native, format="GTiff", bylayer=T, overwrite=TRUE)   

#------------------------------------------------------------------------#

# lmg

# bio
setwd(lgm.path.bi)
tif <- list.files(patt = ".tif")
dtc <- stack(tif); names(dtc)
names(dtc) <- bi.names.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=bi.names.native, format="GTiff", bylayer=T,overwrite=TRUE)   
# neo.tmp.bi <- dtc.neo[[1]]


# pr
setwd(lgm.path.pr)
tif <- list.files(patt = ".tif")
dtc <- stack(tif); names(dtc)
names(dtc) <- pr.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=pr.names.month.native, format="GTiff", bylayer=T,overwrite=TRUE)   
# neo.tmp.pr <- dtc.neo[[1]]

# tn
setwd(lgm.path.tn)
tif <- list.files(patt = "tif")
dtc <- stack(tif); names(dtc)
names(dtc) <- tn.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
# dtc.neo <- dtc.neo/10
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=tn.names.month.native, format="GTiff", bylayer=T,overwrite=TRUE)   
# neo.tmp.tn <- dtc.neo[[1]]


# tx
setwd(lgm.path.tx)
tif <- list.files(patt = ".tif")
dtc <- stack(tif); names(dtc)
names(dtc) <- tx.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))
# dtc.neo <- dtc.neo/10
dir.create('neo'); setwd('neo'); getwd()
writeRaster(dtc.neo, filename=tx.names.month.native, format="GTiff", bylayer=T, overwrite=TRUE)   
# neo.tmp.tx <- dtc.neo[[1]]

#------------------------------------------------------------------------#

# lig 
# está em 30s


# carregar estrutura de lgm
path.ev <- 'C:/Dados/GitHub/environmental_data'

lgm.path.bi.neo <- paste0(path.ev,'/lgm/2_5m/bio/')
lgm.path.pr.neo <- paste0(path.ev,'/lgm/2_5m/pre/')
lgm.path.tn.neo <- paste0(path.ev,'/lgm/2_5m/tmin/')
lgm.path.tx.neo <- paste0(path.ev,'/lgm/2_5m/tmax/')

setwd(lgm.path.bi.neo)
tif <- list.files(patt = ".tif")
lgm.bi.neo <- stack(tif); lgm.bi.neo <- na.omit(lgm.bi.neo)
neo.tmp.bi <- lgm.bi.neo[[1]]

setwd(lgm.path.pr.neo)
tif <- list.files(patt = ".tif")
lgm.pr.neo <- stack(tif); lgm.pr.neo <- na.omit(lgm.pr.neo)
neo.tmp.pr <- lgm.pr.neo[[1]]

setwd(lgm.path.tn.neo)
tif <- list.files(patt = ".tif")
lgm.tn.neo <- stack(tif); lgm.tn.neo <- na.omit(lgm.tn.neo)
neo.tmp.tn <- lgm.tn.neo[[1]]
# plot(lgm.tn.neo[[1]])

setwd(lgm.path.tx.neo)
tif <- list.files(patt = ".tif")
lgm.tx.neo <- stack(tif); lgm.tx.neo <- na.omit(lgm.tx.neo)
neo.tmp.tx <- lgm.tx.neo[[1]]
# plot(lgm.tx.neo[[]])


# bio
setwd(lig.path.bi)
tif <- list.files(patt = ".bil")
dtc <- stack(tif); names(dtc)
names(dtc) <- bi.names.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))

mostra=neo.tmp.bi
lig_2_5m=neo.tmp.bi

for(i in 1:nlayers(dtc.neo)){
  lig_1=dtc.neo[[i]]
  lig_1.5=aggregate(lig_1,fact=c(4,4) , fun=mean)
  rlig1.5=resample(lig_1.5, mostra, method="ngb")
  lig_2_5m=addLayer(lig_2_5m,rlig1.5)
}
lig_2_5m=lig_2_5m[[-1]]

dir.create('neo'); setwd('neo'); getwd()
writeRaster(lig_2_5m, filename=bi.names.native, format="GTiff", bylayer=T,overwrite=TRUE)   

# pr
setwd(lig.path.pr)
tif <- list.files(patt = ".bil")
dtc <- stack(tif); names(dtc)
names(dtc) <- pr.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))

mostra=neo.tmp.pr 
lig_2_5m=neo.tmp.pr

for(i in 1:nlayers(dtc.neo)){
  lig_1=dtc.neo[[i]]
  lig_1.5=aggregate(lig_1,fact=c(4,4) , fun=mean)
  rlig1.5=resample(lig_1.5, mostra, method="ngb")
  lig_2_5m=addLayer(lig_2_5m,rlig1.5)
}
lig_2_5m=lig_2_5m[[-1]]

dir.create('neo'); setwd('neo'); getwd()
writeRaster(lig_2_5m, filename=pr.names.month.native, format="GTiff", bylayer=T,overwrite=TRUE)   


# tn
setwd(lig.path.tn)
tif <- list.files(patt = ".bil")
dtc <- stack(tif); names(dtc)
names(dtc) <- tn.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))

mostra=neo.tmp.tn 
lig_2_5m=neo.tmp.tn

for(i in 1:nlayers(dtc.neo)){
  lig_1=dtc.neo[[i]]
  lig_1.5=aggregate(lig_1,fact=c(4,4) , fun=mean)
  rlig1.5=resample(lig_1.5, mostra, method="ngb")
  lig_2_5m=addLayer(lig_2_5m,rlig1.5)
}
# lig_2_5m=lig_2_5m[[-1]]/10
lig_2_5m=lig_2_5m[[-1]]

dir.create('neo'); setwd('neo'); getwd()
writeRaster(lig_2_5m, filename=tn.names.month.native, format="GTiff", bylayer=T,overwrite=TRUE)   


# tx
setwd(lig.path.tx)
tif <- list.files(patt = ".bil")
dtc <- stack(tif); names(dtc)
names(dtc) <- tx.names.month.native; names(dtc)
dtc.neo <- mask(dtc, mascara)
dtc.neo <- crop(dtc.neo, extent(mascara))

## aqui
mostra = neo.tmp.tx
lig_2_5m = neo.tmp.tx

for(i in 1:nlayers(dtc.neo)){
  lig_1=dtc.neo[[i]]
  lig_1.5=aggregate(lig_1,fact=c(4,4) , fun=mean)
  rlig1.5=resample(lig_1.5, mostra, method="ngb")
  lig_2_5m=addLayer(lig_2_5m,rlig1.5)
}
# lig_2_5m=lig_2_5m[[-1]]/10
lig_2_5m=lig_2_5m[[-1]]

dir.create('neo'); setwd('neo'); getwd()
writeRaster(lig_2_5m, filename=tx.names.month.native, format="GTiff", bylayer=T,overwrite=TRUE)   


#------------------------------------------------------------------------#

# dividir variáveis de temperatura por 10

biodiv10.name = c('bio01','bio02','bio04','bio05','bio06','bio07','bio08','bio09','bio10','bio11')
biodiv10.txt = 'bio01|bio02|bio04|bio05|bio06|bio07|bio08|bio09|bio10|bio11'

col.bioclim <- c(                                     # dividir por 10 ?
  'Annual.Mean.Temperature', #bio1                    # sim
  'Mean.Monthly.Temperature.Range', #bio2             # sim 
  'Isothermality', #bio3                              # nao
  'Temperature.Seasonality.STD.x.100',#bio4           # sim ? é expressa em %
  'Max.Temperature.of.Warmest.Month',#bio5            # sim 
  'Min.Temperature.of.Coldest.Month',#bio6            # sim 
  'Temperature.Annual.Range',#bio7                    # sim 
  'Mean.Temperature.of.Wettest.Quarter',#bio8         # sim 
  'Mean.Temperature.of.Driest.Quarter',#bio9          # sim 
  'Mean.Temperature.of.Warmest.Quarter',#bio10        # sim 
  'Mean.Temperature.of.Coldest.Quarter',#bio11        # sim 
  'Annual.Precipitation',#bio12                       # nao
  'Precipitation.of.Wettest.Month',#bio13             # nao
  'Precipitation.of.Driest.Month',#bio14              # nao  
  'Precipitation.Seasonality.CV',#bio15               # nao
  'Precipitation.of.Wettest.Quarter',#bio16           # nao  
  'Precipitation.of.Driest.Quarter',#bio17            # nao
  'Precipitation.of.Warmest.Quarter',#bio18           # nao
  'Precipitation.of.Coldest.Quarter') #bio19          # nao  



# cur
setwd(pre.path.bi.neo)
tif <- list.files(patt = biodiv10.txt)
dtc.neo <- stack(tif); names(dtc.neo)
dtc10.neo <- dtc.neo/10
writeRaster(dtc10.neo, filename=biodiv10.name, format="GTiff", bylayer=T, overwrite=TRUE)   

# mid
setwd(mid.path.bi.neo)
tif <- list.files(patt = biodiv10.txt)
dtc.neo <- stack(tif); names(dtc.neo)
dtc10.neo <- dtc.neo/10
writeRaster(dtc10.neo, filename=biodiv10.name, format="GTiff", bylayer=T, overwrite=TRUE)   

# lgm
setwd(lgm.path.bi.neo)
tif <- list.files(patt = biodiv10.txt)
dtc.neo <- stack(tif); names(dtc.neo)
dtc10.neo <- dtc.neo/10
writeRaster(dtc10.neo, filename=biodiv10.name, format="GTiff", bylayer=T, overwrite=TRUE)   

# lig
setwd(lig.path.bi.neo)
tif <- list.files(patt = biodiv10.txt)
dtc.neo <- stack(tif); names(dtc.neo)
dtc10.neo <- dtc.neo*10
writeRaster(dtc10.neo, filename=biodiv10.name, format="GTiff", bylayer=T, overwrite=TRUE)   

#tn
setwd(lig.path.tn.neo)
tif <- list.files(patt = '.tif')
dtc.neo <- stack(tif); names(dtc.neo)
dtc10.neo <- dtc.neo*10
writeRaster(dtc10.neo, filename=names(dtc.neo), format="GTiff", bylayer=T, overwrite=TRUE)   


#tx
setwd(lig.path.tx.neo)
tif <- list.files(patt = '.tif')
dtc.neo <- stack(tif); names(dtc.neo)
dtc10.neo <- dtc.neo*10
writeRaster(dtc10.neo, filename=names(dtc.neo), format="GTiff", bylayer=T, overwrite=TRUE)   





# https://gist.github.com/johnbaums/26e8091f082f2b3dd279

polygonizer <- function(x, outshape=NULL, pypath=NULL, readpoly=TRUE, 
                        fillholes=FALSE, aggregate=FALSE, 
                        quietish=TRUE) {
  # x: an R Raster layer, or the file path to a raster file recognised by GDAL 
  # outshape: the path to the output shapefile (if NULL, a temporary file will 
  #           be created) 
  # pypath: the path to gdal_polygonize.py or OSGeo4W.bat (if NULL, the function 
  #         will attempt to determine the location)
  # readpoly: should the polygon shapefile be read back into R, and returned by
  #           this function? (logical) 
  # fillholes: should holes be deleted (i.e., their area added to the containing
  #            polygon)
  # aggregate: should polygons be aggregated by their associated raster value?
  # quietish: should (some) messages be suppressed? (logical)
  if (isTRUE(readpoly) || isTRUE(fillholes)) require(rgdal)
  if (is.null(pypath)) {
    cmd <- Sys.which('OSGeo4W.bat')
    pypath <- 'gdal_polygonize'
    if(cmd=='') {
      cmd <- 'python'
      pypath <- Sys.which('gdal_polygonize.py')
      if (!file.exists(pypath)) 
        stop("Could not find gdal_polygonize.py or OSGeo4W on your system.") 
    }
  }
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists)) 
      stop(sprintf('File already exists: %s', 
                   toString(paste(outshape, c('shp', 'shx', 'dbf'), 
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  
  system2(cmd, args=(
    sprintf('"%s" "%s" %s -f "ESRI Shapefile" "%s.shp"', 
            pypath, rastpath, ifelse(quietish, '-q ', ''), outshape)))
  
  if(isTRUE(aggregate)||isTRUE(readpoly)||isTRUE(fillholes)) {
    shp <- readOGR(dirname(outshape), layer=basename(outshape), 
                   verbose=!quietish)    
  } else return(NULL)
  
  if (isTRUE(fillholes)) {
    poly_noholes <- lapply(shp@polygons, function(x) {
      Filter(function(p) p@ringDir==1, x@Polygons)[[1]]
    })
    pp <- SpatialPolygons(mapply(function(x, id) {
      list(Polygons(list(x), ID=id))
    }, poly_noholes, row.names(shp)), proj4string=CRS(proj4string(shp)))
    shp <- SpatialPolygonsDataFrame(pp, shp@data)
    if(isTRUE(aggregate)) shp <- aggregate(shp, names(shp))
    writeOGR(shp, dirname(outshape), basename(outshape), 
             'ESRI Shapefile', overwrite=TRUE)
  }
  if(isTRUE(aggregate) & !isTRUE(fillholes)) {
    shp <- aggregate(shp, names(shp))
    writeOGR(shp, dirname(outshape), basename(outshape), 
             'ESRI Shapefile', overwrite=TRUE)
  }
  ifelse(isTRUE(readpoly), return(shp), return(NULL))
}
