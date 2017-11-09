###-----------------------------------------------------------------------------------------###
# 1. clear the memory and load the packages
# clear workspace and increase memory
rm(list = ls())
memory.limit(size = 1.75e13) 

###-----------------------------------------------------------------------------------------###

# diretório temporario de processamento do R 

tempdir <- function() "D:\\temps2"
unlockBinding("tempdir", baseenv())
assignInNamespace("tempdir", tempdir, ns="base", envir=baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())
tempdir()


###-----------------------------------------------------------------------------------------###
#setwd("C:/Dados/GitHub/Endemismo"); source("FuncoesEndemismo.R")
root.dir <- 'C:/Dados/GitHub/DataClim'
setwd(root.dir); source('LoadData2_5m.R') 
setwd(root.dir); source('FuncoesPaleoClima2_5m.R')

###-----------------------------------------------------------------------------------------###

# carrgar pontos

points.path <- 'C:\\Users\\pablo\\Dropbox\\Artigos em Preparacao\\Diversidade fitofisionomica Calcarios\\Dados'
points.file <- 'localidades_insitu.txt'
points.file2 <- 'localidades_datapeper.txt'
points.file3 <- 'TotasAreasArtigo.txt'


file <- paste0(points.path,'/',points.file)
file2 <- paste0(points.path,'/',points.file2)
file3 <- paste0(points.path,'/',points.file3)

occ.points <- data.frame(fread(file), stringsAsFactors = F)
occ.points2 <- data.frame(fread(file2), stringsAsFactors = F)
occ.points3 <- data.frame(fread(file3), stringsAsFactors = F)

###-----------------------------------------------------------------------------------------###

# pega dados climáticos e peleoclimáticos

x = getdata.paleoclim(occ.points$DecLat, occ.points$DecLong)
x2 = getdata.paleoclim(occ.points2$DecLat, occ.points2$DecLong)
x3 = getdata.paleoclim(occ.points3$DecLat, occ.points3$DecLong)

file.save <- paste0(points.path,'/poinst_dataclim_insitu.txt')
fwrite(x,file.save, sep='\t')

file.save <- paste0(points.path,'/poinst_dataclim_literatura.txt')
fwrite(x2,file.save, sep='\t')

file.save <- paste0(points.path,'/TotasAreasArtigo_dataclim3.txt')
fwrite(cbind(occ.points3,x3),file.save, sep='\t')


x###-----------------------------------------------------------------------------------------###

# in situ
x=cbind(occ.points,x)
caa <-  data.frame(x[x$caa %in% c('x','*'),],  fito.cod = 'T') #caa <-  data.frame(x[x$caa %in% c('x'),],  fito.cod = 'T')  
fed <-  data.frame(x[x$fed %in% c('x','*'),],  fito.cod = 'C')
fesd <- data.frame(x[x$fesd %in% c('x','*'),], fito.cod = 'F')
fac <-  data.frame(x[x$fac %in% c('x','*'),],  fito.cod = 'K') #fac <-  data.frame(x[x$fac %in% c('x'),],  fito.cod = 'FAC')
mc <-   data.frame(x[x$mc %in% c('x','*'),],   fito.cod = 'P')
fod <-  data.frame(x[x$fod %in% c('x','*'),],  fito.cod = 'D')
fom <-  data.frame(x[x$fom %in% c('x','*'),],  fito.cod = 'M')
saz.fito <- rbind.data.frame(caa, fed, fesd, fod, fom, fac) 

# liteartura
x2=cbind(occ.points2,x2)
# caa2 <-  data.frame(x[x2$caa %in% c('x','*'),],  fito.cod = 'T')  
#caa2 <-  data.frame(x2[x$caa %in% c('x'),],  fito.cod = 'T')  
fed2 <-  data.frame(x[x2$fed %in% c('x','*'),],  fito.cod = 'C')
fesd2 <- data.frame(x[x2$fesd %in% c('x','*'),], fito.cod = 'F')
fac2 <-  data.frame(x[x2$fac %in% c('x','*'),],  fito.cod = 'K')
#fac2 <-  data.frame(x[x2$fac %in% c('x'),],  fito.cod = 'K')
#mc2 <-   data.frame(x[x2$mc %in% c('x','*'),],   fito.cod = 'P')
fod2 <-  data.frame(x[x2$fod %in% c('x','*'),],  fito.cod = 'D')

caa2=rep(NA,NROW(fed2))
mc2=rep(NA,NROW(fed2))
fom2=rep(NA,NROW(fed2))

saz.fito2 <- rbind.data.frame(caa2, fed2, fesd2, fod2, fom2, fac2) 

# all 
x3=cbind(occ.points3,x3)
caa3 <-  data.frame(x3[x3$T %in% c('x','1'),],  fito.cod = 'T') #caa <-  data.frame(x[x$caa %in% c('x'),],  fito.cod = 'T')  
fed3 <-  data.frame(x3[x3$C %in% c('x','1'),],  fito.cod = 'C')
fesd3 <- data.frame(x3[x3$F %in% c('x','1'),], fito.cod = 'F')
fac3 <-  data.frame(x3[x3$K %in% c('x','1'),],  fito.cod = 'K') #fac <-  data.frame(x[x$fac %in% c('x'),],  fito.cod = 'FAC')
mc3 <-   data.frame(x3[x3$P %in% c('x','1'),],   fito.cod = 'P')
fod3 <-  data.frame(x3[x3$D %in% c('x','1'),],  fito.cod = 'D')
fom3 <-  data.frame(x3[x3$M %in% c('x','1'),],  fito.cod = 'M')
saz.fito3 <- rbind.data.frame(caa3, fed3, fesd3, fod3, fom3, fac3) 


par(bty='l', mfrow=c(1,3))
plot(saz.fito3$fito.cod,saz.fito3$mseco.p2t)# , main='Precipitation Month < 2 vs Temperature Month')
plot(saz.fito3$fito.cod,saz.fito3$predrymonth)#, main='Precipitation < 60 mm')
plot(saz.fito3$fito.cod,saz.fito3$prebio01)#, main='Annual Mean Temperature')
 
dev.off()


# all 
par(bty='l', mfrow=c(1,2))
plot(saz.fito$fito.cod,saz.fito$mseco.p2t)
plot(saz.fito$fito.cod,saz.fito$prebio01)

par(bty='l', mfrow=c(1,2))
plot(saz.fito$fito.cod,saz.fito$predrymonth)
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio01))

# boa
par(bty='l', mfrow=c(1,3))
plot(saz.fito.all$fito.cod,saz.fito.all$predrymonth)
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio09/10),  main=label.bioclim[9]) # boa 1
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio03/10),  main=label.bioclim[3]) # boa 2


plot(saz.fito.all$fito.cod,(saz.fito.all$prebio01),  main=label.bioclim[1])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio02/10),  main=label.bioclim[2])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio03/10),  main=label.bioclim[3]) # boa 2
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio04),  main=label.bioclim[4])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio05),  main=label.bioclim[5])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio06),  main=label.bioclim[6])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio07/10),  main=label.bioclim[7])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio08/10),  main=label.bioclim[8])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio09/10),  main=label.bioclim[9]) # boa 1
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio10/10),  main=label.bioclim[10])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio11/10),  main=label.bioclim[11]) # boa 3
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio12),  main=label.bioclim[12])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio13),  main=label.bioclim[13])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio14),  main=label.bioclim[14])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio15),  main=label.bioclim[15])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio16),  main=label.bioclim[16])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio17),  main=label.bioclim[17])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio18),  main=label.bioclim[18])
plot(saz.fito.all$fito.cod,(saz.fito.all$prebio19),  main=label.bioclim[19])



label.bioclim <-
c('Annual Mean Temperature', #bio1                    # sim
  'Mean Monthly Temperature Range', #bio2             # sim
  'Isothermality', #bio3                              # nao
  'Temperature Seasonality STD x 100',#bio4           # sim ? é expressa em %
  'Max Temperature of Warmest Month',#bio5            # sim
  'Min Temperature of Coldest Month',#bio6            # sim
  'Temperature Annual Range',#bio7                    # sim
  'Mean Temperature of Wettest Quarter',#bio8         # sim
  'Mean Temperature of Driest Quarter',#bio9          # sim
  'Mean Temperature of Warmest Quarter',#bio10        # sim
  'Mean Temperature of Coldest Quarter',#bio11        # sim
  'Annual Precipitation',#bio12                       # nao
  'Precipitation of Wettest Month',#bio13             # nao
  'Precipitation of Driest Month',#bio14              # nao
  'Precipitation Seasonality CV',#bio15               # nao
  'Precipitation of Wettest Quarter',#bio16           # nao
  'Precipitation of Driest Quarter',#bio17            # nao
  'Precipitation of Warmest Quarter',#bio18           # nao
  'Precipitation of Coldest Quarter') #bio19          # nao


par(bty='l', mfrow=c(1,3))
plot(saz.fito$fito.cod,saz.fito$mseco.p2t)
plot(saz.fito$fito.cod,saz.fito$predrymonth)
plot(saz.fito.all$fito.cod,saz.fito.all$prebio01)


par(bty='l', mfrow=c(1,2))
plot(saz.fito$fito.cod,saz.fito$mseco.p2t)
plot(saz.fito$fito.cod,saz.fito$predrymonth)

par(bty='l', mfrow=c(1,1))
plot(saz.fito$fito.cod,saz.fito$prebio01)



plot(saz.fito$fito.cod,saz.fito$mseco.p2t)
   
plot(saz.fito$fito.cod,saz.fito$tipo.macico)

unique(x$dominioFitogeografico)

###-----------------------------------------------------------------------------------------###

x.dc = getdata.paleoclim(occ.points$DecLat, occ.points$DecLong)
x = data.frame(cbind.data.frame(x.dc, occ.points), stringsAsFactors = F)
x$prebio05 = x$prebio05/10
x$prebio06 = x$prebio06/10

linha =12
pastafichas = 'd:/temps/fichas2'
output_file_html = paste0(as.character(x$bioma[linha]),'_',as.character(x$municipio[linha]), '_', linha,'.html')
output_file_pdf = paste0(as.character(x$bioma[linha]),'_',as.character(x$municipio[linha]), '_', linha,'.pdf')
try(render('ImagemSateliteFotoClimasArtigo.Rmd',
           output_file=output_file_html,
           output_dir=pastafichas,
           intermediates_dir=paste0(pastafichas,'/tmp')))


# gera pranchas
for (linha in 1:NROW(x)){
  #linha =4
  output_file_html = paste0(as.character(x$bioma[linha]),'_',as.character(x$municipio[linha]), '_', linha,'.html')
  try(render('ImagemSateliteFotoClimasArtigo.Rmd',
             output_file=output_file_html,
             output_dir=pastafichas,
             intermediates_dir=paste0(pastafichas,'/tmp')))
  
}  


pastafichas = 'd:/temps/fichas2'
output_file_html = 'Phytophysiognomies.html'
try(render('Phytophysiognomies.Rmd',
           output_file=output_file_html,
           output_dir=pastafichas,
           intermediates_dir=paste0(pastafichas,'/tmp')))




#https://github.com/rstudio/shiny-examples/issues/34

# basic-miktex-2.9.6361-x64 in https://miktex.org/2.9/setup
# tufte-common.def \typeoutbool{pdfatex}{@tufte@pdf} to \typeoutbool{xelatex}{@tufte@pdf}

# Sys.setenv(LANGUAGE = 'en')
# 
# pkgs <- c('psych', 'openxlsx', 'lubridate', 'foreach', 'rmarkdown', 'knitr', 'devtools')
# 
# installed <- utils::installed.packages()
# 
# not.installed <- c()
# 
# for (pkg in pkgs) {
#   if (!(pkg %in% installed)) not.installed <- c(not.installed, pkg)
# }
# 
# if (length(not.installed) > 0) install.packages(not.installed)
# 
# for (pkg in pkgs) library(pkg, character.only = TRUE)
# 
# 
# if (Sys.which("pdftex") == '' && .Platform$OS.type == "windows") {
#   Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\Program Files\\MiKTeX 2.9\\miktex\\bin\\x64", sep=.Platform$path.sep))
# }
# 
# source('setup.R')
# 
# # setup()

#devtools::install_github("rstudio/rmarkdown", force=T);library(rmarkdown)
#Sys.setenv(PATH=paste(Sys.getenv("PATH"),"C:/Program Files/MiKTeX 2.9/miktex/bin/x64/",sep=";"))

try(render("ImagemSateliteFotoClimasArtigoPDF.Rmd",
       output_format=pdf_document(latex_engine='xelatex'),
       output_file=output_file_pdf,
       output_dir=pastafichas,
       intermediates_dir=paste0(pastafichas,'/tmp')))





###-----------------------------------------------------------------------------------------###






# occ.points <- cbind(occ.points, checa.bioma(occ.points))
# occ.points <- cbind(occ.points, checa.bacia(occ.points))

###-----------------------------------------------------------------------------------------###



# classifica sazonalidade
x[x$mseco.p2t<=2,]$tipoSazonalidade <- 'assazonal'
x[x$mseco.p2t>2 & x$mseco.p2t<=5,]$tipoSazonalidade <- 'sazonal'
x[x$mseco.p2t>5,]$tipoSazonalidade <- 'supersazonal'

# classifica bioma
if(any(x$Caatinga==1))     {x[x$Caatinga>=1,]$bioma <- 'Caatinga'}
if(any(x$Cerrado==1))      {x[x$Cerrado>=1,]$bioma <- 'Cerrado'}
if(any(x$MataAtlantica==1)){x[x$MataAtlantica>=1,]$bioma <- 'MataAtlantica'}
if(any(x$Pampa==1))        {x[x$Pampa>=1,]$bioma <- 'Pampa'} 
if(any(x$Pantanal==1))     {x[x$Pantanal>=1,]$bioma <- 'Pantanal'}
if(any(x$Amazonia==1))     {x[x$Amazonia>=1,]$bioma <- 'Amazonia'}
#x$bioma

# classifica bacia
x$bacia <- {}
if(any(x$Amazonas==1))          {x[x$Amazonas>=1,]$bacia <- 'Amazonas'}
if(any(x$AtlanticoLeste==1))    {x[x$AtlanticoLeste>=1,]$bacia <- 'AtlanticoLeste'}
if(any(x$AtlanticoNordeste==1)) {x[x$AtlanticoNordeste>=1,]$bacia <- 'AtlanticoNordeste'}
if(any(x$AtlanticoSudeste==1))  {x[x$AtlanticoSudeste>=1,]$bacia <- 'AtlanticoSudeste'}
if(any(x$Parana==1))            {x[x$Parana>=1,]$bacia <- 'Parana'}
if(any(x$SaoFrancisco==1))      {x[x$SaoFrancisco>=1,]$bacia <- 'SaoFrancisco'}
if(any(x$Tocantins==1))         {x[x$Tocantins>=1,]$bacia <- 'Tocantins'}


# aridez
linha=1
x2 <-paleo.data.clima(x, id.periodo=1, linha=linha)
p2 <- x2[1,]
t2m <-x2[4,]*2  
p2 < t2m


x2 <-paleo.data.clima(x, id.periodo=1, linha=linha)
p2 <- x2[1,1]
t2m <-(x2[2,1]+x2[3,1])/2  
p2 < t2m

linha=97
paleo.data.clima(x, id.periodo=1, linha=linha)
meses.secos(x, linha = linha)
meses.chuvosos(x,linha = linha)

paleo.plot.clima(x, 1, linha, linha3 = T) 
paleo.plot.clima(x, 2, linha, linha3 = T) 
paleo.plot.clima(x, 3, linha, linha3 = T) 
paleo.plot.clima(x, 4, linha, linha3 = T) 



linha =11
try(render('PaleoClimaImagemSatelite.Rmd',
           output_file= paste0(x$county[linha],'_',linha,'.html'),
           output_dir='d:/temps/fichas',
           intermediates_dir='d:/temps/fichas/tmp'))




linha =113
render('PaleoClimaSatParFull2.Rmd',
       output_file= paste0(x$county[linha],'_',linha,'.html'),
       output_dir='d:/temps',
       intermediates_dir='./tmp')



linha =17
render('PaleoClima.Rmd',
       output_file= paste0(x$county[linha],'_',linha,'.html'),
       output_dir='d:/temps',
       intermediates_dir='d:/temps/tmp')


linha =52
render('PaleoClimaSat.Rmd',
       output_file= paste0(x$county[linha],'_',linha,'.html'),
       output_dir='d:/temps',
       intermediates_dir='d:/temps/tmp')


linha =52
render('PaleoClimaSatPar.Rmd',
       output_file= paste0(x$county[linha],'_',linha,'.html'),
       output_dir='d:/temps',
       intermediates_dir='d:/temps/tmp')

linha =52
render('PaleoClimaParHTML.Rmd',
       output_file= paste0(x$county[linha],'_',linha,'.html'),
       output_dir='d:/temps',
       intermediates_dir='d:/temps/tmp')

linha =52
render('PaleoClimaPar.Rmd',
       output_file= paste0(x$county[linha],'_',linha,'.html'),
       output_dir='d:/temps',
       intermediates_dir='d:/temps/tmp')



PlotOnStaticMap(MyMap, lat = lat, 
                lon = lon, 
                destfile = "MyTile1.png", cex=1.5,pch=20,
                col=c('red', 'blue', 'green'), add=FALSE)
