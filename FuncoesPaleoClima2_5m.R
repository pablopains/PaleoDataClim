###-----------------------------------------------------------------------------------------###
print.all.paleo.clim <- function(points,path='',save.par=T)
{
  if(!path==''){path.root=getwd(); setwd(path)}
  for(x in 1:NROW(points)){for (e in 1:4){paleo.plot.clima(points, e, x, linha3 = T, save=save.par)}}
  if(!path==''){getwd(path.root)}
}

###-----------------------------------------------------------------------------------------###
paleo.data.clima <- function(data.clim, 
                             id.periodo=1, 
                             linha=1, 
                             nome.colunas=c('pr','tn','tx','ta'),
                             periodo=c('pre','mid','lgm','lig'))
{  
  pr.names.month = c(c(paste0(periodo[id.periodo], nome.colunas[1],'0',(1:9))), c( paste0(periodo[id.periodo], nome.colunas[1],(10:12))) )
  tn.names.month = c(c(paste0(periodo[id.periodo], nome.colunas[2],'0',(1:9))), c( paste0(periodo[id.periodo], nome.colunas[2],(10:12))) )
  tx.names.month = c(c(paste0(periodo[id.periodo], nome.colunas[3],'0',(1:9))), c( paste0(periodo[id.periodo], nome.colunas[3],(10:12))) )
  if(id.periodo==1){
    ta.names.month = c(c(paste0(periodo[id.periodo], nome.colunas[4],'0',(1:9))), c( paste0(periodo[id.periodo], nome.colunas[4],(10:12))) )}
  
  Prec.  <- as.numeric(data.clim[linha, pr.names.month])
  Max.t. <- as.numeric(data.clim[linha, tx.names.month])
  Min.t. <- as.numeric(data.clim[linha, tn.names.month])
  if(id.periodo==1){
    Mean.t <- as.numeric(data.clim[linha, ta.names.month])}
  else {Mean.t <-0}
  dc <- rbind(Prec., Max.t., Min.t., Mean.t)
  return(dc)
  # diagwl(dc,
  #        est=paste0(points@data$county[linha],'-',periodo[id.periodo]),
  #        alt=points@data$alt[linha],
  #        per="1970-2000",
  #        mlab="en",
  #        margen=c(4, 4, 5, 4))
}

###-----------------------------------------------------------------------------------------###
#WALTER H & LIETH H (1960): Klimadiagramm Weltatlas. G. Fischer, Jena.
paleo.plot.clima <- function(data.clim, 
                             id.periodo=1, linha=1, save=F, linha3=F,
                             nome.colunas=c('pr','tn','tx','ta'),
                             periodo=c('pre','mid','lgm','lig'))
{  
  data.clim <- data.frame(data.clim, stringsAsFactors = F)
  if(save){
    dir.root <- getwd()
    if (!dir.exists('ResultsCurvaOmbrotermica')){dir.create('ResultsCurvaOmbrotermica')}
    setwd('ResultsCurvaOmbrotermica')}
  
  
  dc <- paleo.data.clima(data.clim, id.periodo, linha,
                         nome.colunas, periodo)
  
  file.jpg = paste0(data.clim$county[linha],'_', data.clim$referencia[linha],'_',linha,'_',id.periodo,'-',periodo[id.periodo],'.jpg')
  if (save==T) {jpeg(file.jpg)}
  
  # name.map = paste0(data.clim$county[linha],'-',periodo[id.periodo])
  # alt.map = data.clim$alt[linha]
  # per.map = "1970-2000"
  
  # name.map = paste0(paste0(meses.chuvosos(data.clim[linha,])[id.periodo], ' moist month > 100 mm'),
  #                   paste0(', ',meses.secos(data.clim[linha,])[id.periodo], ' dry month < 60 mm'))
  name.map = NA
  alt.map = NA
  per.map = NA
    
  # upper precipitation
  # lower precipitation
  # upper precipitation
  # lower precipitation
  # dry month
  # moist month
  
  diagwl(dc,
         est=name.map,
         alt=alt.map,
         per=per.map,
         mlab="en",
         margen=c(4, 4, 5, 4),
         p3line=linha3,
         shem = T)
  
  if (save==T){
    dev.off()
    setwd(dir.root)}
}

###-----------------------------------------------------------------------------------------###

plot.mapa.paleo <- function(points,fundo='uf') #fundo='bioma','uf'
{
  pto <- SpatialPointsDataFrame(cbind(points$DecLong,points$DecLat),points)
  
  if(fundo=='bioma'){plot(biomas, border='gray10', lty=1, lwd=1)}
  if(fundo=='uf'){plot(br.uf, border='gray10', lty=1, lwd=1)}
  points(pto, pch = 16, cex = .8, col = "red")
}

plot.mapa.paleo2 <- function(DecLong,DecLat,fundo='uf') #fundo='bioma','uf'
{
  id=data.frame(paste0('id_',1:NROW(DecLong)))
  pto <- SpatialPointsDataFrame(cbind(as.numeric(DecLong),as.numeric(DecLat)),id)
  
  if(fundo=='bioma'){plot(biomas, border='gray10', lty=1, lwd=1)}
  if(fundo=='uf'){plot(br.uf, border='gray10', lty=1, lwd=1)}
  points(pto, pch = 16, cex = .8, col = "red")
}

###-----------------------------------------------------------------------------------------###

# # #climebel 1994
# prec.max.seca <- 60
# prec.mim.chuva <- 100

# meses secos e chuvosos
#meses.secos(points.dtc,1)
meses.secos <- function(points, linha=1, prec.max.seca=60, nome.colunas=c('pr','tn','tx','ta'), periodo=c('pre','mid','lgm','lig'))
{
  meses.secos <- meses.chuvosos <- {}
  for(id.periodo in 1:4)
  {
    dc <- paleo.data.clima(points, id.periodo, linha, nome.colunas, periodo)  
    meses.secos[id.periodo] <- as.numeric(colSums(data.frame(ifelse(dc[1,]<=prec.max.seca,1,0))))
  }
  return(meses.secos)
}

#

meses.chuvosos <- function(points, linha=1, prec.mim.chuva=100, nome.colunas=c('pr','tn','tx','ta'), periodo=c('pre','mid','lgm','lig'))
{
  meses.chuvosos <- {}
  for(id.periodo in 1:4)
  {
    dc <- paleo.data.clima(points, id.periodo, linha, nome.colunas, periodo)  
    meses.chuvosos[id.periodo] <- as.numeric(colSums(data.frame(ifelse(dc[1,]>=prec.mim.chuva,1,0))))
  }
  return(meses.chuvosos)
}

#

meses.secos.chuvosos <- function(points, linha=1, prec.max.seca=60, prec.mim.chuva=100, nome.colunas=c('pr','tn','tx'), periodo=c('pre','mid','lgm','lig'))
{
  meses.secos <- meses.chuvosos <- {}
  for(id.periodo in 1:4)
  {
    dc <- paleo.data.clima(points, id.periodo, linha, nome.colunas, periodo)  
    meses.secos[id.periodo] <- as.numeric(colSums(data.frame(ifelse(dc[1,]<=prec.max.seca,1,0))))
    meses.chuvosos[id.periodo] <- as.numeric(colSums(data.frame(ifelse(dc[1,]>=prec.mim.chuva,1,0))))
  }
  return(list(meses.secos,meses.chuvosos))
}

###-----------------------------------------------------------------------------------------###
#p.menor.2t(points.dtc,linha)[[2]]

p.menor.2t <- function(points, linha=1, nome.colunas=c('pr','tn','tx','ta'), periodo=c('pre','mid','lgm','lig'))
{
  meses.secos <- meses.chuvosos <- {}
  dc <- paleo.data.clima(points, 1, linha, nome.colunas, periodo)  
  p <-dc[1,]
  t2 <- dc[4,]*2  
  meses.secos[1] <- as.numeric(colSums(data.frame(ifelse(p <= t2, 1, 0))))
  meses.chuvosos[1] <- as.numeric(colSums(data.frame(ifelse(p > t2, 1, 0))))
  return(list(meses.secos,meses.chuvosos))
}

###-----------------------------------------------------------------------------------------###

# carregar valores por ponto

getdata.paleoclim <- function(DecLat, DecLong)
{
  
  PointToPaleoClim = data.frame(DecLong = DecLong,
                                DecLat = DecLat,
                                stringsAsFactors = F)
  
  points <- SpatialPointsDataFrame(cbind(PointToPaleoClim$DecLong,PointToPaleoClim$DecLat),PointToPaleoClim)

  points.pre.bi <- points.pre.pr <- points.pre.tn <- points.pre.tx <- points.pre.ta <- list({})
  points.mid.bi <- points.mid.pr <- points.mid.tn <- points.mid.tx <- list({})
  points.lgm.bi <- points.lgm.pr <- points.lgm.tn <- points.lgm.tx <- list({})
  points.lig.bi <- points.lig.pr <- points.lig.tn <- points.lig.tx <- list({})
  
  # bio=1
  for (bio in 1:19)
  {
    points.pre.bi[[bio]] <- extract(pre.bi.neo[[bio]], points)
    points.mid.bi[[bio]] <- extract(mid.bi.neo[[bio]], points)
    points.lgm.bi[[bio]] <- extract(lgm.bi.neo[[bio]], points)
    points.lig.bi[[bio]] <- extract(lig.bi.neo[[bio]], points)
  }
  
  # pr tn tx 
  for (mes in 1:12)
  {
    points.pre.pr[[mes]] <- extract(pre.pr.neo[[mes]], points)
    points.pre.tn[[mes]] <- extract(pre.tn.neo[[mes]], points)
    points.pre.tx[[mes]] <- extract(pre.tx.neo[[mes]], points)
    points.pre.ta[[mes]] <- extract(pre.ta.neo[[mes]], points)
    
    points.mid.pr[[mes]] <- extract(mid.pr.neo[[mes]], points)
    points.mid.tn[[mes]] <- extract(mid.tn.neo[[mes]], points)
    points.mid.tx[[mes]] <- extract(mid.tx.neo[[mes]], points)
    
    points.lgm.pr[[mes]] <- extract(lgm.pr.neo[[mes]], points)
    points.lgm.tn[[mes]] <- extract(lgm.tn.neo[[mes]], points)
    points.lgm.tx[[mes]] <- extract(lgm.tx.neo[[mes]], points)
    
    points.lig.pr[[mes]] <- extract(lig.pr.neo[[mes]], points)
    points.lig.tn[[mes]] <- extract(lig.tn.neo[[mes]], points)
    points.lig.tx[[mes]] <- extract(lig.tx.neo[[mes]], points)
  }
  
  ###-----------------------------------------------------------------------------------------###
  
  # nome variáveis
  
  bi.names=c(paste0("bio0", 1:9), paste0("bio", 10:19))
  pr.names.month=c(c(paste0('pr0',1:9)),c(paste0('pr',10:12)))
  tn.names.month=c(c(paste0('tn0',1:9)),c(paste0('tn',10:12)))
  tx.names.month=c(c(paste0('tx0',1:9)),c(paste0('tx',10:12)))
  ta.names.month=c(c(paste0('ta0',1:9)),c(paste0('ta',10:12)))
  
  # periodos
  preriod <- c('pre','mid','lgm','lig')  
  
  # nomero de linas
  l <-  NROW(points)
  
  ###-----------------------------------------------------------------------------------------###
  
  #
  points.dtc <- points@data
  
  # cur
  
  # pr
  vm<-points.pre.pr
  tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[1],pr.names.month)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # tn
  vm<-points.pre.tn
  tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[1],tn.names.month)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # tx
  vm<-points.pre.tx
  tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[1],tx.names.month)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # ta
  vm<-points.pre.ta
  tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[1],ta.names.month)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # bio
  vb=points.pre.bi
  tmp.dtc=data.frame(vb[[1]], vb[[2]], vb[[3]], vb[[4]], vb[[5]], vb[[6]], vb[[7]], vb[[8]], vb[[9]],
                     vb[[10]], vb[[11]], vb[[12]], vb[[13]],vb[[14]], vb[[15]], vb[[16]], vb[[17]], vb[[18]], vb[[19]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[1],bi.names)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # mid
  
  # pr
  vm<-points.mid.pr
  tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[2],pr.names.month)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # tn
  vm<-points.mid.tn
  tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[2],tn.names.month)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # tx
  vm<-points.mid.tx
  tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[2],tx.names.month)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # bio
  vb=points.mid.bi
  tmp.dtc=data.frame(vb[[1]], vb[[2]], vb[[3]], vb[[4]], vb[[5]], vb[[6]], vb[[7]], vb[[8]], vb[[9]],
                     vb[[10]], vb[[11]], vb[[12]], vb[[13]],vb[[14]], vb[[15]], vb[[16]], vb[[17]], vb[[18]], vb[[19]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[2],bi.names)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  
  # lgm
  
  # pr
  vm<-points.lgm.pr
  tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[3],pr.names.month)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # tn
  vm<-points.lgm.tn
  tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[3],tn.names.month)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # tx
  vm<-points.lgm.tx
  tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[3],tx.names.month)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # bio
  vb=points.lgm.bi
  tmp.dtc=data.frame(vb[[1]], vb[[2]], vb[[3]], vb[[4]], vb[[5]], vb[[6]], vb[[7]], vb[[8]], vb[[9]],
                     vb[[10]], vb[[11]], vb[[12]], vb[[13]],vb[[14]], vb[[15]], vb[[16]], vb[[17]], vb[[18]], vb[[19]], stringsAsFactors = F)
  colnames(tmp.dtc) <- paste0(preriod[3],bi.names)
  points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # lig

  # pr
   vm<-points.lig.pr
   tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
   colnames(tmp.dtc) <- paste0(preriod[4],pr.names.month)
   points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
  # tn
   vm<-points.lig.tn
   tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
   colnames(tmp.dtc) <- paste0(preriod[4],tn.names.month)
   points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
   # tx
   vm<-points.lig.tx
   tmp.dtc=data.frame(vm[[1]], vm[[2]], vm[[3]], vm[[4]], vm[[5]], vm[[6]], vm[[7]], vm[[8]], vm[[9]], vm[[10]], vm[[11]], vm[[12]], stringsAsFactors = F)
   colnames(tmp.dtc) <- paste0(preriod[4],tx.names.month)
   points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
   # bio
   vb=points.lig.bi
   tmp.dtc=data.frame(vb[[1]], vb[[2]], vb[[3]], vb[[4]], vb[[5]], vb[[6]], vb[[7]], vb[[8]], vb[[9]],
                      vb[[10]], vb[[11]], vb[[12]], vb[[13]],vb[[14]], vb[[15]], vb[[16]], vb[[17]], vb[[18]], vb[[19]], stringsAsFactors = F)
   colnames(tmp.dtc) <- paste0(preriod[4],bi.names)
   points.dtc <- cbind.data.frame(points.dtc, tmp.dtc)
  
   mseco <- data.frame(predrymonth=NA, middrymonth=NA, lgmdrymonth=NA, ligdrymonth=NA)
   mchuva <- data.frame(premoistmonth=NA, midmoistmonth=NA, lgmmoistmonth=NA, ligmoistmonth=NA)
   mseco.p2t <- mchuva.p2t <- {}
   tipoSazonalidade <- bioma <- bacia <- {}
   
   for (linha in 1:NROW(points.dtc)){
     mseco[linha,] <- meses.secos(points.dtc, linha = linha)
     mchuva[linha,] <- meses.chuvosos(points.dtc, linha = linha)
     mseco.p2t[linha] <- p.menor.2t(points.dtc,linha)[[1]]
     mchuva.p2t[linha] <- p.menor.2t(points.dtc,linha)[[2]]
   }
  
   points.dtc <- cbind.data.frame(points.dtc, mseco, mchuva, mseco.p2t, mchuva.p2t)

   points.dtc <- cbind(points.dtc, checa.bioma(points.dtc))
   points.dtc <- cbind(points.dtc, checa.bacia(points.dtc))
   
   tipoSazonalidadeI <- tipoSazonalidadeII <- dominioFitogeografico <- baciaHidrografica <- rep('',NROW(points.dtc))

   #Duração da estação seca foi definida em três grupos, 
   #assazonal (I, meses secos <= 2; e II, meses secos <= 2), 
   #sazonal (I,  meses secos > 2  e meses secos <= 5  e precipitação média anual > 1000 mm; 
   #        II, meses secos > 2  e meses secos <= 6 e precipitação média anual > 1000 mm ) e 
   #supersazonal (I, meses secos >= 5 e precipitação média anual < 1000 mm; e 
   #              II, meses secos >= 6  ; e precipitação média anual < 1000 mm). 

   for (linha in 1:NROW(points.dtc)){
     
     #classifica sazonalidade artigo criterio I
     if(points.dtc$mseco.p2t[linha]<=2) {tipoSazonalidadeI[linha] <- 'assazonal'}
     if(points.dtc$mseco.p2t[linha]>2 & 
        points.dtc$mseco.p2t[linha]<=5) {tipoSazonalidadeI[linha] <- 'sazonal'}
     if(points.dtc$mseco.p2t[linha]<=6 &
        points.dtc$prebio12[linha]>800 &
        points.dtc$prebio12[linha]<1000){tipoSazonalidadeI[linha] <- 'sazonal/superssazonal'}
     if(points.dtc$mseco.p2t[linha]>=6&
        points.dtc$prebio12[linha]<800) {tipoSazonalidadeI[linha] <-'supersazonal'}
     
     #classifica sazonalidade artigo criterio II
     if(points.dtc$predrymonth[linha]<=2) {tipoSazonalidadeII[linha] <- 'assazonal'}
     if(points.dtc$predrymonth[linha]>2 & 
        points.dtc$predrymonth[linha]<=6) {tipoSazonalidadeII[linha] <- 'sazonal'}
     if(points.dtc$predrymonth[linha]<=6 &
        points.dtc$prebio12[linha]>800 &
        points.dtc$prebio12[linha]<1000)  {tipoSazonalidadeII[linha] <- 'sazonal/superssazonal'}
     if(points.dtc$predrymonth[linha]>=6&
        points.dtc$prebio12[linha]<800) {tipoSazonalidadeII[linha] <-'supersazonal'}

     # #classifica sazonalidade areas abertas segundo Velozo 1992
     # if(points.dtc$mseco.p2t[linha]<=2)                                {tipoSazonalidade[linha] <- 'ombrófilo'}
     # if(points.dtc$mseco.p2t[linha]>2 & points.dtc$mseco.p2t[linha]<=5){tipoSazonalidade[linha] <- 'estacional'}
     # if(points.dtc$mseco.p2t[linha]>=6)                                 {tipoSazonalidade[linha] <- 'estacional xerófito'}
     
     # # classifica sazonalidade floresta Velozo 1992
     # if(points.dtc$mseco.p2t[linha]<=4)                                {tipoSazonalidade[linha] <- 'ombrófilo'}
     # if(points.dtc$mseco.p2t[linha]>4 & points.dtc$mseco.p2t[linha]<=6){tipoSazonalidade[linha] <- 'estacional'}
     # if(points.dtc$mseco.p2t[linha]>6)                                 {tipoSazonalidade[linha] <- 'estacional xerófito'}
     
     # # classifica climebel
     # if(points.dtc$predrymonth[linha]<=4)                                  {tipoSazonalidade.60[linha] <- 'ombrófilo'}
     # if(points.dtc$predrymonth[linha]>4 & points.dtc$predrymonth[linha]<=6){tipoSazonalidade.60[linha] <- 'estacional'}
     # if(points.dtc$predrymonth[linha]>6)                                   {tipoSazonalidade.60[linha] <- 'estacional xerófito'}
     
     
     
     # classifica bioma
     if(points.dtc$Caatinga[linha]==1)     {dominioFitogeografico[linha] <- 'Caatinga'}
     if(points.dtc$Cerrado[linha]==1)      {dominioFitogeografico[linha] <- 'Cerrado'}
     if(points.dtc$MataAtlantica[linha]==1){dominioFitogeografico[linha] <- 'MataAtlantica'}
     if(points.dtc$Pampa[linha]==1)        {dominioFitogeografico[linha] <- 'Pampa'} 
     if(points.dtc$Pantanal[linha]==1)     {dominioFitogeografico[linha] <- 'Pantanal'}
     if(points.dtc$Amazonia[linha]==1)     {dominioFitogeografico[linha] <- 'Amazonia'}
     #points.dtc$bioma
     
     # classifica bacia
     points.dtc$bacia <- {}
     if(points.dtc$Amazonas[linha]==1)          {baciaHidrografica[linha] <- 'Amazonas'}
     if(points.dtc$AtlanticoLeste[linha]==1)    {baciaHidrografica[linha] <- 'AtlanticoLeste'}
     if(points.dtc$AtlanticoNordeste[linha]==1) {baciaHidrografica[linha] <- 'AtlanticoNordeste'}
     if(points.dtc$AtlanticoSudeste[linha]==1)  {baciaHidrografica[linha] <- 'AtlanticoSudeste'}
     if(points.dtc$Parana[linha]==1)            {baciaHidrografica[linha] <- 'Parana'}
     if(points.dtc$SaoFrancisco[linha]==1)      {baciaHidrografica[linha] <- 'SaoFrancisco'}
     if(points.dtc$Tocantins[linha]==1)         {baciaHidrografica[linha] <- 'Tocantins'} 
   }   
   
  points.paleo.clim <- cbind.data.frame(points.dtc, tipoSazonalidadeI,tipoSazonalidadeII, dominioFitogeografico, baciaHidrografica)

  return(points.paleo.clim)
}

###-----------------------------------------------------------------------------------------###



# mseco <- data.frame(predrymonth=NA, midrymonth=NA, lgmdrymonth=NA, ligdrymonth=NA)
# mchuva <- data.frame(premoistmonth=NA, mimoistmonth=NA, lgmmoistmonth=NA, ligmoistmonth=NA)
# 
# for (linha in 1:NROW(x)){
#   mseco[linha,] <- data.frame(meses.secos(x, linha = linha), stringsAsFactors = F)
#   mchuva[linha,] <- data.frame(meses.chuvosos(x, linha = linha), stringsAsFactors = F)
# }  
# 
# x <- cbind(x,mseco,mchuva)
