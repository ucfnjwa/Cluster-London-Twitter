#report tool 
#account:ucfnjwa@ucl.ac.uk
#password    wj19931025
#reade("fireworkeve")is a sample
#install.packages("twitteR")
#install.packages("rtweet")
#install.packages("devtools")
#install.packages("bit64")
#install.packages("httr")
#install.packages("rjson")
#install.packages("data.table")
#install.packages("sf")
#install.packages("sp")
#install.packages("plyr")
#install.packages("spatstat")
#install.packages("sp")
#install.packages("rgeos")
#install.packages("maptools")
#install.packages("GISTools")
#install.packages("tmap")
#install.packages("sf")
#install.packages("geojsonio")
#install.packages("data.table")
#install.packages("sf")
#install.packages("Data.talble")
#install.packages("OpenStreetMap")



#step1    start
startprogram<-function(times){
  
  library(twitteR)
  library(rtweet)
  library(devtools)
  library(bit64)
  library(httr)
  library(rjson)
  library(data.table)
  library(sf)
  library(sp)
  library(plyr)
  library(spatstat)
  library(sp)
  library(rgeos)
  library(maptools)
  library(GISTools)
  library(tmap)
  library(sf)
  library(geojsonio)
  library(data.table)
 
  
  options(httr_oauth_cache=T)
  api_key<<-"	g4R3T96IKnCe1daJnX8Gh8iN9"
  api_secret<<-"iKU0X7EscnJp7gueluXphyNGPJcoWRHwXgckoEH3aDrKBUM6PW"
  access_token<<-"939508156316770305-AU87eUc1GLiizAtMJdhYulJhRHbdBlF"
  access_token_token<<-"GsCxqVFveyQwJbX2hFmrYNz66ChRGKrB5FddLnPUGnSlF"
  twitteR:::setup_twitter_oauth("g4R3T96IKnCe1daJnX8Gh8iN9","iKU0X7EscnJp7gueluXphyNGPJcoWRHwXgckoEH3aDrKBUM6PW")
  #get BoroughlondonMap
  BNG <<- "+init=epsg:27700"
  WGS <<- "+init=epsg:4326"
  EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")
  #pull out london using grep and the regex wildcard for'start of the string' (^) to to look for the bit of the district code that relates to London (E09) from the 'lad15cd' column in the data slot of our spatial polygons dataframe
  BoroughlondonMap <- EW[grep("^E09",EW@data$lad15cd),]
  BoroughlondonMap<<-spTransform(BoroughlondonMap,BNG)
  
}


#step2 read or search
#for read analysis  a="fireworkeve.csv"
reade<-function(a){
  
  #read 
  library(data.table)
  quanjusfread<-st_read(a,options = c("X_POSSIBLE_NAMES=X","Y_POSSIBLE_NAMES=Y"))
  quanjusfreadbng<-st_set_crs(quanjusfread,27700)
  library(sf)
  
  quanjusp<<-as(quanjusfreadbng,"Spatial")
  BNG = "+init=epsg:27700"
  quanjusp<-spTransform(quanjusp,BNG)
  quanjusf<-st_as_sf(quanjusp)
  BoroughlondonMapsf<<-st_as_sf(BoroughlondonMap)
  quanjusf<<-quanjusf[BoroughlondonMapsf,]
  quanjusp<<-as(quanjusf,"Spatial")
}


#search     Error: Rate limit exceeded - please wait! 
#a="...."
word<-function(a){
  #repeat
  data1<-search_tweets(q=a,lang="en",geocode = lookup_coords("london"),n=10000,retryonratelimit = TRUE,type = "recent")
  data2<-search_tweets(q=a,lang="en",geocode = lookup_coords("london"),n=10000,retryonratelimit = TRUE,type = "recent")
  data3<-search_tweets(q=a,lang="en",geocode = lookup_coords("london"),n=10000,retryonratelimit = TRUE,type = "recent")
  data4<-search_tweets(q=a,lang="en",geocode = lookup_coords("london"),n=10000,retryonratelimit = TRUE,type = "recent")
  data5<-search_tweets(q=a,lang="en",geocode = lookup_coords("london"),n=10000,retryonratelimit = TRUE,type = "recent")
  data6<-search_tweets(q=a,lang="en",geocode = lookup_coords("london"),n=10000,retryonratelimit = TRUE,type = "recent")
  data7<-search_tweets(q=a,lang="en",geocode = lookup_coords("london"),n=10000,retryonratelimit = TRUE,type = "recent")
  data8<-search_tweets(q=a,lang="en",geocode = lookup_coords("london"),n=10000,retryonratelimit = TRUE,type = "recent")
  dataall<-rbind(data1,data2,data3,data4,data5,data6,data7,data8)
  dataall <- lat_lng(dataall)
  #merge
  dataall<-dataall[complete.cases(dataall[,44]),]
  #sf
  library(data.table)
  library(sf)
  dataallsf = st_as_sf(dataall, coords = c("lng", "lat"), 
                       crs = 4326, agr = "constant",na.fail = FALSE)
  quanjusp<<-as(dataallsf,"Spatial")
  BNG = "+init=epsg:27700"
  quanjusp<-spTransform(quanjusp,BNG)
  quanjusf<-st_as_sf(quanjusp)
  BoroughlondonMapsf<<-st_as_sf(BoroughlondonMap)
  quanjusf<<-quanjusf[BoroughlondonMapsf,]
  quanjusp<<-as(quanjusf,"Spatial")
  quanjusp<<-remove.duplicates(quanjusp)
}

#save   .csv
savemyresreach<-function(a){
  st_write(obj = quanjusf, a,layer_options = "GEOMETRY=AS_XY")
}



#step3:      look tmap  and  k

lookmapview<-function(c,b){
  library(tmap)
  tmap_mode("view")
  tm_shape(BoroughlondonMap) +
    tm_polygons(col = NA, alpha = 0.5) +
    tm_shape(quanjusp) +
    tm_dots(col = c,size = 0.2)+
    tm_layout(b, title.position = c("right", "bottom"))
  
  
}
#times=1
Ripleyk<-function(times){
  boroughspe <- BoroughlondonMap
  boroughspesf<-st_as_sf(boroughspe)
  quanjuboroughsf<-quanjusf[boroughspesf,]
  quanjuboroughsp<-as(quanjuboroughsf,"Spatial")
  quanjuboroughsp<-remove.duplicates(quanjuboroughsp)
  window<-as.owin(boroughspe)
  point.ppp <- ppp(x=quanjuboroughsp@coords[,1],y=quanjuboroughsp@coords[,2],window=window)
  R <- Kest(point.ppp, correction="border")
  plot(R)
}


#step4 : DSCAN
Cluster1<-function(r,n){
  library(fpc)
  londonPoints <- data.frame(quanjusp@coords[,1:2])
  re <- fpc::dbscan(londonPoints, eps = r, MinPts = n)
  library(ggplot2)
  library(raster)
  library(plyr)
  library(OpenStreetMap)
  londonPoints$cluster <- re$cluster
  chulls <- ddply(londonPoints, .(cluster), function(re) re[chull(re$coords.x1, re$coords.x2), ])
  chulls <- subset(chulls, cluster>=1)
  basemap<-OpenStreetMap::openmap(c(51.3589310,-0.3101178),c(51.64527550,0.08932426), zoom=NULL,"stamen-toner")
  #convert the basemap to British National Grid - remember we created the BNG object right at the beginning of the practical - it's an epsg string...
  basemap_bng<-openproj(basemap, projection=BNG)
  autoplot(basemap_bng) + geom_point(data=londonPoints, aes(coords.x1,coords.x2, colour=cluster, fill=cluster)) + geom_polygon(data = chulls, aes(coords.x1,coords.x2, group=cluster, fill=cluster), alpha = 0.5)  
}


#step5:   boroughpre
#a="City of London", b="the map of london"
lookbor<-function(a,b){
  library(raster)
  library(fpc)
  library(plyr)

  
  
  boroughspe <<- BoroughlondonMap[BoroughlondonMap@data$lad15nm==a,]
  boroughspesf<<-st_as_sf(boroughspe)
  quanjuboroughsf<<-quanjusf[boroughspesf,]
  quanjuboroughsp<<-as(quanjuboroughsf,"Spatial")
  quanjuboroughsp<<-remove.duplicates(quanjuboroughsp)
  
  library(tmap)
  tmap_mode("view")
  tm_shape(boroughspe) +
    tm_polygons(col = NA, alpha = 0.5) +
    tm_shape(quanjuboroughsp) +
    tm_dots(col = "red",size = 0.2)+
    tm_layout(b, title.position = c("right", "bottom"))
}

#a="City of London"
Ripleykborough<-function(a){
  boroughspe <- BoroughlondonMap[BoroughlondonMap@data$lad15nm==a,]
  boroughspesf<-st_as_sf(boroughspe)
  quanjuboroughsf<-quanjusf[boroughspesf,]
  quanjuboroughsp<-as(quanjuboroughsf,"Spatial")
  quanjuboroughsp<-remove.duplicates(quanjuboroughsp)
  window<-as.owin(boroughspe)
  point.ppp <- ppp(x=quanjuboroughsp@coords[,1],y=quanjuboroughsp@coords[,2],window=window)
  R <- Kest(point.ppp, correction="border")
  plot(R)
}


boroughpre<-function(a){
  
  latlong <- "+init=epsg:4326" 
  BoroughWGS <-spTransform(quanjuboroughsp, CRS(latlong))
  BoroughWGS@bbox
}

#step6    boroughDBSCAN
Clusterb<-function(r,n,y1,x1,y2,x2){
  library(fpc)
  londonPoints <- data.frame(quanjuboroughsp@coords[,1:2])
  re <- fpc::dbscan(londonPoints, eps = r, MinPts = n)
  library(ggplot2)
  library(raster)
  library(plyr)
  library(OpenStreetMap)
  londonPoints$cluster <- re$cluster
  chulls <- ddply(londonPoints, .(cluster), function(re) re[chull(re$coords.x1, re$coords.x2), ])
  chulls <- subset(chulls, cluster>=1)
  basemap<-OpenStreetMap::openmap(c(y1,x1),c(y2,x2), zoom=NULL,"stamen-toner")
  #convert the basemap to British National Grid - remember we created the BNG object right at the beginning of the practical - it's an epsg string...
  basemap_bng<-openproj(basemap, projection=BNG)
  autoplot(basemap_bng) + geom_point(data=londonPoints, aes(coords.x1,coords.x2, colour=cluster, fill=cluster)) + geom_polygon(data = chulls, aes(coords.x1,coords.x2, group=cluster, fill=cluster), alpha = 0.5)  
}




















