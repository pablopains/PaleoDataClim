###-----------------------------------------------------------------------------------------###

# Pacotes

if(!require(pacman)) install.packages("pacman")
pacman::p_load(raster,sp, maptools, rgdal, corrplot, RStoolbox, vegan, psych, data.table
               , climatol, knitr, rmarkdown,
               maps, mapdata, RgoogleMaps)

# check loaded packets
search()

#
###-----------------------------------------------------------------------------------------###

# mascaras

maptype = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")

mask.path <- 'E:/environmental_data/mask'
setwd(mask.path)

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
bioma<-list({})
setwd(mask.path);setwd('biomas')

bioma[[1]] <- readShapePoly("amazonia.shp") 
bioma[[2]] <- readShapePoly("cerrado.shp")
bioma[[3]] <- readShapePoly("caatinga.shp")
bioma[[4]] <- readShapePoly("mataatlantica.shp")
bioma[[5]] <- readShapePoly("pampa.shp")
bioma[[6]] <- readShapePoly("pantanal.shp")
names(bioma) <- bioma.names

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

###-----------------------------------------------------------------------------------------###
### Carregar dados variávei
###-----------------------------------------------------------------------------------------###

# bioclim
path.to <- 'C:/Dados/GitHub'

mask.path.to <- paste0(path.to,'/environmental_data/mask/')

# path neotropico
pre.path.bi.neo <- 'E:/environmental_data/bioclim/bioclim_v01/current/bil/2_5m/bio_2-5m_bil/neo'
pre.path.pr.neo <- 'E:/environmental_data/bioclim/bioclim_v01/current/bil/2_5m/prec_2-5m_bil/neo'
pre.path.tn.neo <- 'E:/environmental_data/bioclim/bioclim_v01/current/bil/2_5m/tmin_2-5m_bil/neo'
pre.path.tx.neo <- 'E:/environmental_data/bioclim/bioclim_v01/current/bil/2_5m/tmax_2-5m_bil/neo'
pre.path.ta.neo <- 'E:/environmental_data/bioclim/bioclim_v01/current/bil/2_5m/tmean_2-5m_bil/neo'

pre.path.bi.neo.to <- paste0(path.to,'/environmental_data/cur/2_5m/bio/')
pre.path.pr.neo.to <- paste0(path.to,'/environmental_data/cur/2_5m/pre/')
pre.path.tn.neo.to <- paste0(path.to,'/environmental_data/cur/2_5m/tmin/')
pre.path.tx.neo.to <- paste0(path.to,'/environmental_data/cur/2_5m/tmax/')
pre.path.ta.neo.to <- paste0(path.to,'/environmental_data/cur/2_5m/tmean/')

mid.path.bi.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/mid/2_5m/ccmid/ccmidbi_2-5m/neo'
mid.path.pr.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/mid/2_5m/ccmid/ccmidpr_2-5m/neo'
mid.path.tn.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/mid/2_5m/ccmid/ccmidtn_2-5m/neo'
mid.path.tx.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/mid/2_5m/ccmid/ccmidtx_2-5m/neo'

mid.path.bi.neo.to <- paste0(path.to,'/environmental_data/mid/2_5m/bio/')
mid.path.pr.neo.to <- paste0(path.to,'/environmental_data/mid/2_5m/pre/')
mid.path.tn.neo.to <- paste0(path.to,'/environmental_data/mid/2_5m/tmin/')
mid.path.tx.neo.to <- paste0(path.to,'/environmental_data/mid/2_5m/tmax/')

lgm.path.bi.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/lgm/2_5m/cclgm/cclgmbi_2-5m/neo'
lgm.path.pr.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/lgm/2_5m/cclgm/cclgmpr_2-5m/neo'
lgm.path.tn.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/lgm/2_5m/cclgm/cclgmtn_2-5m/neo'
lgm.path.tx.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/lgm/2_5m/cclgm/cclgmtx_2-5m/neo'

lgm.path.bi.neo.to <- paste0(path.to,'/environmental_data/lgm/2_5m/bio/')
lgm.path.pr.neo.to <- paste0(path.to,'/environmental_data/lgm/2_5m/pre/')
lgm.path.tn.neo.to <- paste0(path.to,'/environmental_data/lgm/2_5m/tmin/')
lgm.path.tx.neo.to <- paste0(path.to,'/environmental_data/lgm/2_5m/tmax/')

lig.path.bi.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/lig/bio/neo'
lig.path.pr.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/lig/prec/neo'
lig.path.tn.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/lig/tmin/neo'
lig.path.tx.neo <- 'E:/environmental_data/bioclim/bioclim_v01/past/lig/tmax/neo'

lig.path.bi.neo.to <- paste0(path.to,'/environmental_data/lig/2_5m/bio/')
lig.path.pr.neo.to <- paste0(path.to,'/environmental_data/lig/2_5m/pre/')
lig.path.tn.neo.to <- paste0(path.to,'/environmental_data/lig/2_5m/tmin/')
lig.path.tx.neo.to <- paste0(path.to,'/environmental_data/lig/2_5m/tmax/')

setwd(path.to)
dir.create('environmental_data')
setwd('environmental_data')
dir.create('mask')
dir.create('cur')
dir.create('mid')
dir.create('lgm')
dir.create('lig')

setwd('mask')
dir.create('uf');
dir.create('biomas');
dir.create('bacias');

setwd('..')
setwd('cur')
dir.create('2_5m');
setwd('2_5m')
dir.create('bio');
dir.create('pre')
dir.create('tmin')
dir.create('tmax')
dir.create('tmean')
setwd('..')

setwd('..')
setwd('mid')
dir.create('2_5m');
setwd('2_5m')
dir.create('bio')
dir.create('pre')
dir.create('tmin')
dir.create('tmax')
setwd('..')

setwd('..')
setwd('lgm')
dir.create('2_5m');
setwd('2_5m')
dir.create('bio')
dir.create('pre')
dir.create('tmin')
dir.create('tmax')
setwd('..')

setwd('..')
setwd('lig')
dir.create('2_5m');
setwd('2_5m')
dir.create('bio')
dir.create('pre')
dir.create('tmin')
dir.create('tmax')


###-----------------------------------------------------------------------------------------###

# pre
setwd(pre.path.bi.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,pre.path.bi.neo.to)

setwd(pre.path.pr.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,pre.path.pr.neo.to)

setwd(pre.path.tn.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,pre.path.tn.neo.to)

setwd(pre.path.tx.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,pre.path.tx.neo.to)

setwd(pre.path.ta.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,pre.path.ta.neo.to)

#mid
setwd(mid.path.bi.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,mid.path.bi.neo.to)

setwd(mid.path.pr.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,mid.path.pr.neo.to)

setwd(mid.path.tn.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,mid.path.tn.neo.to)

setwd(mid.path.tx.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,mid.path.tx.neo.to)

# lgm
setwd(lgm.path.bi.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,lgm.path.bi.neo.to)

setwd(lgm.path.pr.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,lgm.path.pr.neo.to)

setwd(lgm.path.tn.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,lgm.path.tn.neo.to)

setwd(lgm.path.tx.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,lgm.path.tx.neo.to)

#lig
setwd(lig.path.bi.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,lig.path.bi.neo.to)

setwd(lig.path.pr.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,lig.path.pr.neo.to)

setwd(lig.path.tn.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,lig.path.tn.neo.to)

setwd(lig.path.tx.neo)
tif <- list.files(patt = ".tif")
file.copy(tif,lig.path.tx.neo.to)

# mask
setwd(mask.path)
tif <- list.files(all.files = T)
file.copy(tif,mask.path.to)

setwd(mask.path); setwd('uf')
tif <- list.files(all.files = T)
file.copy(tif,paste0(mask.path.to,'uf'))

setwd(mask.path); setwd('biomas')
tif <- list.files(all.files = T)
file.copy(tif,paste0(mask.path.to,'biomas'))

setwd(mask.path); setwd('bacias')
tif <- list.files(all.files = T)
file.copy(tif,paste0(mask.path.to,'bacias'))
