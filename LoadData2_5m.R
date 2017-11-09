###-----------------------------------------------------------------------------------------###
# Pacotes

if(!require(pacman)) install.packages("pacman")
pacman::p_load(raster,sp, maptools, rgdal, corrplot, RStoolbox, vegan, psych, data.table
               , climatol, knitr, rmarkdown,jpeg, sqldf,
               maps, mapdata, RgoogleMaps)

# check loaded packets
search()

#
###-----------------------------------------------------------------------------------------###
# mascaras
maptype = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")

path.ev <- 'C:/Dados/GitHub/environmental_data'
mask.path <- paste0(path.ev,'/mask/'); setwd(mask.path)


# 
neotropico <- readShapePoly(fn="Lowenberg_Neto_Neotropics.shp")

# 
br <- readShapePoly("BR.shp")
br.uf <- readShapePoly("BR_UF.shp")
bacias <- readShapePoly("bacias.shp")
biomas <- readShapePoly("biomas.shp")

#
karst.br.wmcro3 <- readShapePoly("Karst_Brazil2.shp")
#karst.sa.wmcro3 <- readShapePoly("Karst_South_America.shp")
karst.sa.all.wmcro3 <- readShapePoly("South_American_Karst.shp")
karst.br.icmbio.cecav <- readShapePoly("Karst_Carbo_CECAV.shp")

# biomas
bioma.names <- c('AM','CE','CA','MA','PP','PT')
bioma <-list({})
setwd(mask.path);setwd('biomas')

bioma[[1]] <- readShapePoly("amazonia.shp") 
bioma[[2]] <- readShapePoly("cerrado.shp")
bioma[[3]] <- readShapePoly("caatinga.shp")
bioma[[4]] <- readShapePoly("mataatlantica.shp")
bioma[[5]] <- readShapePoly("pampa.shp")
bioma[[6]] <- readShapePoly("pantanal.shp")
names(bioma) <- bioma.names


checa.bioma <- function(occ.points){
  Amazonia <- Caatinga <- Cerrado <- MataAtlantica <- Pampa <- Pantanal <- {}
  for(p in 1:NROW(occ.points) )
  {
    Amazonia <- rbind(Amazonia, endemismo2(occ.points[p,], bioma[[1]]))
    Caatinga <- rbind(Caatinga, endemismo2(occ.points[p,],  bioma[[3]]))
    Cerrado <- rbind(Cerrado, endemismo2(occ.points[p,],  bioma[[2]]))
    MataAtlantica <- rbind(MataAtlantica, endemismo2(occ.points[p,], bioma[[4]]))
    Pampa <- rbind(Pampa, endemismo2(occ.points[p,], bioma[[5]]))
    Pantanal <- rbind(Pantanal, endemismo2(occ.points[p,], bioma[[6]]))
  }
  
  biomas<-cbind.data.frame(Amazonia, Caatinga, Cerrado, MataAtlantica, Pampa, Pantanal)
  
  return(biomas)
}

# ufs BR
uf.names <-c('AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MG', 'MS', 'MT', 'PA', 'PB', 'PE','PI', 'PR', 'RJ', 'RN', 'RO', 'RR', 'RS', 'SC', 'SE', 'SP', 'TO')
uf<-list({})
setwd(mask.path);setwd('uf')

uf[[1]] <- readShapePoly("AC.shp")
uf[[2]] <- readShapePoly("AL.shp")
uf[[3]] <- readShapePoly("AM.shp")
uf[[4]] <- readShapePoly("AP.shp")
uf[[5]] <- readShapePoly("BA.shp")
uf[[6]] <- readShapePoly("CE.shp")
uf[[7]] <- readShapePoly("DF.shp")
uf[[8]] <- readShapePoly("ES.shp")
uf[[9]] <- readShapePoly("GO.shp")
uf[[10]] <- readShapePoly("MA.shp")
uf[[11]] <- readShapePoly("MG.shp")
uf[[12]] <- readShapePoly("MS.shp")
uf[[13]] <- readShapePoly("MT.shp")
uf[[14]] <- readShapePoly("PA.shp")
uf[[15]] <- readShapePoly("PB.shp")
uf[[16]] <- readShapePoly("PE.shp")
uf[[17]] <- readShapePoly("PI.shp")
uf[[18]] <- readShapePoly("PR.shp")
uf[[19]] <- readShapePoly("RJ.shp")
uf[[20]] <- readShapePoly("RN.shp")
uf[[21]] <- readShapePoly("RO.shp")
uf[[22]] <- readShapePoly("RR.shp")
uf[[23]] <- readShapePoly("RS.shp")
uf[[24]] <- readShapePoly("SC.shp")
uf[[25]] <- readShapePoly("SE.shp")
uf[[26]] <- readShapePoly("SP.shp")
uf[[27]] <- readShapePoly("TO.shp")
names(uf) <- uf.names


# bacias
bacia.names <-c('AM', 'AL', 'AN', 'AS', 'PR', 'SF', 'TO')
bacia.legenda <-c('Amazonas', 'Atlântico Leste', 'Atlântico Nordeste', 'Atlântico Sudeste', 'Paraná', 'São Francisco', 'Tocantins')
bacia<-list({})
setwd(mask.path); setwd('bacias')

bacia[[1]] <- readShapePoly(fn="amazonas.shp")
bacia[[2]] <- readShapePoly(fn="atlanticoleste.shp")
bacia[[3]] <- readShapePoly(fn="atlanticonortenordeste.shp")
bacia[[4]] <- readShapePoly(fn="atlanticosudeste.shp")
bacia[[5]] <- readShapePoly(fn="parana.shp")
bacia[[6]] <- readShapePoly(fn="saofrancisco.shp")
bacia[[7]] <- readShapePoly(fn="tocantins.shp")
names(bacia) <- bacia.names

checa.bacia <- function(occ.points){

  Amazonas <- AtlanticoLeste <- AtlanticoNordeste <- AtlanticoSudeste <- Parana <- SaoFrancisco <- Tocantins <- {}
  
  for(p in 1:NROW(occ.points) )
  {
    Amazonas <- rbind(Amazonas, endemismo2(occ.points[p,], bacia[[1]]))
    AtlanticoLeste <- rbind(AtlanticoLeste, endemismo2(occ.points[p,], bacia[[2]]))
    AtlanticoNordeste <- rbind(AtlanticoNordeste, endemismo2(occ.points[p,], bacia[[3]]))
    AtlanticoSudeste <- rbind(AtlanticoSudeste, endemismo2(occ.points[p,], bacia[[4]]))
    Parana <- rbind(Parana, endemismo2(occ.points[p,], bacia[[5]]))
    SaoFrancisco <- rbind(SaoFrancisco, endemismo2(occ.points[p,], bacia[[6]]))
    Tocantins <- rbind(Tocantins, endemismo2(occ.points[p,], bacia[[7]]))
  }
  
  bacias<-cbind.data.frame(Amazonas, AtlanticoLeste, AtlanticoNordeste, AtlanticoSudeste, Parana, SaoFrancisco, Tocantins)
  
  return(bacias)
}

endemismo2 <- function(occ.points, limit.pol) # migrada de endemismo 
{
  occ.tmp.points <- occ.tmp.points_in <- occ.tmp.points_out <- {}
  
  if (!NROW(occ.points)>0){return(NULL)}
  
  occ.tmp.points <- SpatialPointsDataFrame(cbind(occ.points$DecLong,occ.points$DecLat),occ.points)
  occ.tmp.points_in <- occ.tmp.points[!is.na(over(occ.tmp.points ,geometry(limit.pol))),] # dentro do poligono
  #occ.tmp.points_out <- occ.tmp.points[is.na(over(occ.tmp.points ,geometry(limit.pol))),] # fora do poligono
  
  return(NROW(occ.tmp.points_in@data))
}

###-----------------------------------------------------------------------------------------###
### Carregar dados variávei
###-----------------------------------------------------------------------------------------###

# bioclim

# path neotropico
pre.path.bi.neo <- paste0(path.ev,'/cur/2_5m/bio/')
pre.path.pr.neo <- paste0(path.ev,'/cur/2_5m/pre/')
pre.path.tn.neo <- paste0(path.ev,'/cur/2_5m/tmin/')
pre.path.tx.neo <- paste0(path.ev,'/cur/2_5m/tmax/')
pre.path.ta.neo <- paste0(path.ev,'/cur/2_5m/tmean/')

mid.path.bi.neo <- paste0(path.ev,'/mid/2_5m/bio/')
mid.path.pr.neo <- paste0(path.ev,'/mid/2_5m/pre/')
mid.path.tn.neo <- paste0(path.ev,'/mid/2_5m/tmin/')
mid.path.tx.neo <- paste0(path.ev,'/mid/2_5m/tmax/')

lgm.path.bi.neo <- paste0(path.ev,'/lgm/2_5m/bio/')
lgm.path.pr.neo <- paste0(path.ev,'/lgm/2_5m/pre/')
lgm.path.tn.neo <- paste0(path.ev,'/lgm/2_5m/tmin/')
lgm.path.tx.neo <- paste0(path.ev,'/lgm/2_5m/tmax/')

lig.path.bi.neo <- paste0(path.ev,'/lig/2_5m/bio/')
lig.path.pr.neo <- paste0(path.ev,'/lig/2_5m/pre/')
lig.path.tn.neo <- paste0(path.ev,'/lig/2_5m/tmin/')
lig.path.tx.neo <- paste0(path.ev,'/lig/2_5m/tmax/')

###-----------------------------------------------------------------------------------------###

biodiv10.name = c('bio01','bio02','bio04','bio05','bio06','bio07','bio08','bio09','bio10','bio11')

# pre
setwd(pre.path.bi.neo)
tif <- list.files(patt = ".tif")
pre.bi.neo <- stack(tif); pre.bi.neo <- na.omit(pre.bi.neo)
pre.bi.neo[[biodiv10.name]] <- pre.bi.neo[[biodiv10.name]]/10
# plot(pre.bi.neo)

setwd(pre.path.pr.neo)
tif <- list.files(patt = ".tif")
pre.pr.neo <- stack(tif); pre.pr.neo <- na.omit(pre.pr.neo)
# plot(pre.pr.neo)

setwd(pre.path.tn.neo)
tif <- list.files(patt = ".tif")
pre.tn.neo <- stack(tif); pre.tn.neo <- na.omit(pre.tn.neo)
pre.tn.neo <- pre.tn.neo/10
# plot(pre.tn.neo)

setwd(pre.path.tx.neo)
tif <- list.files(patt = ".tif")
pre.tx.neo <- stack(tif); pre.tx.neo <- na.omit(pre.tx.neo)
pre.tx.neo <- pre.tx.neo/10
# plot(pre.tx.neo)

setwd(pre.path.ta.neo)
tif <- list.files(patt = ".tif")
pre.ta.neo <- stack(tif); pre.ta.neo <- na.omit(pre.ta.neo)
pre.ta.neo <- pre.ta.neo/10
# plot(pre.ta.neo)

#mid
setwd(mid.path.bi.neo)
tif <- list.files(patt = ".tif")
mid.bi.neo <- stack(tif); mid.bi.neo <- na.omit(mid.bi.neo)
mid.bi.neo[[biodiv10.name]] <- mid.bi.neo[[biodiv10.name]]/10

# plot(mid.bi.neo[[]])

setwd(mid.path.pr.neo)
tif <- list.files(patt = ".tif")
mid.pr.neo <- stack(tif); mid.pr.neo <- na.omit(mid.pr.neo)
# plot(mid.pr.neo[[]])

setwd(mid.path.tn.neo)
tif <- list.files(patt = ".tif")
mid.tn.neo <- stack(tif); mid.tn.neo <- na.omit(mid.tn.neo)
mid.tn.neo <- mid.tn.neo/10
# plot(mid.tn.neo[[]])

setwd(mid.path.tx.neo)
tif <- list.files(patt = ".tif")
mid.tx.neo <- stack(tif); pre.tx.neo <- na.omit(pre.tx.neo)
mid.tx.neo <- mid.tx.neo/10
# plot(pre.tx.neo[[]])

# lgm
setwd(lgm.path.bi.neo)
tif <- list.files(patt = ".tif")
lgm.bi.neo <- stack(tif); lgm.bi.neo <- na.omit(lgm.bi.neo)
lgm.bi.neo[[biodiv10.name]] <- lgm.bi.neo[[biodiv10.name]]/10
# plot(lgm.bi.neo[[]])

setwd(lgm.path.pr.neo)
tif <- list.files(patt = ".tif")
lgm.pr.neo <- stack(tif); lgm.pr.neo <- na.omit(lgm.pr.neo)
# plot(lgm.pr.neo[[]])

setwd(lgm.path.tn.neo)
tif <- list.files(patt = ".tif")
lgm.tn.neo <- stack(tif); lgm.tn.neo <- na.omit(lgm.tn.neo)
lgm.tn.neo <- lgm.tn.neo/10
# plot(lgm.tn.neo[[1]])

setwd(lgm.path.tx.neo)
tif <- list.files(patt = ".tif")
lgm.tx.neo <- stack(tif); lgm.tx.neo <- na.omit(lgm.tx.neo)
lgm.tx.neo <- lgm.tx.neo/10
# plot(lgm.tx.neo[[]])

#lig
path.ev <- 'C:/Dados/GitHub/environmental_data'
setwd(lig.path.bi.neo)
tif <- list.files(patt = ".tif")
lig.bi.neo <- stack(tif); lig.bi.neo <- na.omit(lig.bi.neo)
lig.bi.neo[[biodiv10.name]] <- lig.bi.neo[[biodiv10.name]]/10
#plot(lig.bi.neo[[11]])

setwd(lig.path.pr.neo)
tif <- list.files(patt = ".tif")
lig.pr.neo <- stack(tif); lig.pr.neo <- na.omit(lig.pr.neo)
#plot(lig.pr.neo[[1]])

setwd(lig.path.tn.neo)
tif <- list.files(patt = ".tif")
lig.tn.neo <- stack(tif); lig.tn.neo <- na.omit(lig.tn.neo)
lig.tn.neo <- lig.tn.neo/10
#plot(lig.tn.neo[[12]])

setwd(lig.path.tx.neo)
tif <- list.files(patt = ".tif")
lig.tx.neo <- stack(tif); pre.tx.neo <- na.omit(pre.tx.neo)
lig.tx.neo <- lig.tx.neo/10
# plot(pre.tx.neo[[]])


# 
# biodiv10.name = c('bio01','bio02','bio04','bio05','bio06','bio07','bio08','bio09','bio10','bio11')
# biodiv10.txt = 'bio01|bio02|bio04|bio05|bio06|bio07|bio08|bio09|bio10|bio11'
# 
# col.bioclim <- c(                                     # dividir por 10 ?
#   'Annual.Mean.Temperature', #bio1                    # sim
#   'Mean.Monthly.Temperature.Range', #bio2             # sim 
#   'Isothermality', #bio3                              # nao
#   'Temperature.Seasonality.STD.x.100',#bio4           # sim ? é expressa em %
#   'Max.Temperature.of.Warmest.Month',#bio5            # sim 
#   'Min.Temperature.of.Coldest.Month',#bio6            # sim 
#   'Temperature.Annual.Range',#bio7                    # sim 
#   'Mean.Temperature.of.Wettest.Quarter',#bio8         # sim 
#   'Mean.Temperature.of.Driest.Quarter',#bio9          # sim 
#   'Mean.Temperature.of.Warmest.Quarter',#bio10        # sim 
#   'Mean.Temperature.of.Coldest.Quarter',#bio11        # sim 
#   'Annual.Precipitation',#bio12                       # nao
#   'Precipitation.of.Wettest.Month',#bio13             # nao
#   'Precipitation.of.Driest.Month',#bio14              # nao  
#   'Precipitation.Seasonality.CV',#bio15               # nao
#   'Precipitation.of.Wettest.Quarter',#bio16           # nao  
#   'Precipitation.of.Driest.Quarter',#bio17            # nao
#   'Precipitation.of.Warmest.Quarter',#bio18           # nao
#   'Precipitation.of.Coldest.Quarter') #bio19          # nao  