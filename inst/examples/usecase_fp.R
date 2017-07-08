traddelkopf60 <- uavRmd::makeAP(projectDir="~/proj/uav/gentree/Traddelkopf/",
                                locationName = "65msolo",
                                surveyArea="~/proj/uav/gentree/Traddelkopf/data/task_areas/Traddelkopf_2017_02_paralell.json",
                                followSurface = TRUE,
                                followSurfaceRes = 1,
                                flightAltitude = 65,
                                overlap = 0.75,
                                demFn = "~/proj/uav/gentree/Traddelkopf/data/DGM1_kellerwald.tif",
                                altFilter = 1.,
                                horizonFilter = 25,
                                maxSpeed = 10,
                                uavType = "solo",
                                cameraType = "MAPIR2",
                                windCondition = 2) 





library(mapedit)
library(leaflet)
library(mapview)
library(uavRmd)
vec(overlay = traddelkopf60$demA)
fA <- edit_map(mapview(traddelkopf60$demA)@map)



fp<-makeAP(projectDir="~/uav/hasenkopf",
           locationName = "hase50",
           surveyArea = "hase50.json", 
           followSurface = TRUE, 
           flightAltitude = 50,
           overlap = 0.6,
           demFn = "mrbiko.tif",
           altFilter = 5,
           maxSpeed = 30,
           windCondition = 2)

fp<-makeAP(projectDir="~/uav/npbw",
           locationName = "npbw100m_nord",
           surveyArea="bw100_nord.json", 
           followSurface = TRUE, 
           flightAltitude = 100,
           overlap = 0.6,
           altFilter = 5,
           maxSpeed = 40,
           windCondition = 3)

fp<-makeAP(projectDir ="/home/creu/uav/",
           locationName = "grossfilz100m_nord",
           surveyArea="grossfilz100_nord.json", 
           followSurface = TRUE, 
           flightAltitude = 100,
           demFn = "grosser-filz_latlon.tif",
           overlap = 0.6,
           altFilter = 5,
           maxSpeed = 40,
           windCondition = 3)

fpdata<-makeAP(projectDir ="/home/creu/uav/",
               locationName = "grossfilz100m_sued",
               surveyArea="/home/creu/uav/grossfilz/grossfilz100m_sued3.json", 
               followSurface = TRUE, 
               flightAltitude = 100,
               demFn = "/home/creu/uav/grossfilz/grosserfilz.tif",
               overlap = 0.6,
               altFilter = 5,
               maxSpeed = 40,
               windCondition = 3)          



fp<-makeAP(projectDir ="/home/creu/uav/kellerwald",
           locationName = "hagenstein100",
           surveyArea="~/proj/uav_basics/hagenstein.json",
           followSurface = TRUE,
           flightAltitude = 100,
           demFn = "~/uav/Hagenstein_dsm.tif",
           windCondition = 2,
           uavType = "djip3"
)

fp<-makeAP(projectDir ="/home/creu/uav/kellerwald",
           locationName = "hagenstein100",
           surveyArea="~/uav/hagenstein.json",
           followSurface = TRUE,
           flightAltitude = 100,
           windCondition = 2,
           uavType = "djip3"
)

fp<-makeTP(projectDir ="/home/creu/uav/bayerwald",
        locationName = "filzmoosTree",
        missionTrackList="~/uav/bayerwald/Selected_trees_Filz.txt",
        demFn = "~/uav/grossfilz/grosserfilz_latlon.tif",
        windCondition = 2,
        uavType = "solo",
        followSurfaceRes=5)

fp<-makeAP(projectDir ="/home/creu/uav/kellerwald",
           locationName = "hagenstein100",
           surveyArea="~/uav/hagenstein.json",
           followSurface = TRUE,
           flightAltitude = 100,
           demFn = "~/uav/uniwald/data/mrbiko.tif",
           windCondition = 2,
           followSurfaceRes=10,
           altFilter = 3
)

fp<-makeAP(projectDir ="~/uav/cookbook",
           locationName = "firstSurvey",
           surveyArea="~/proj/drone/uniwald/myFirstSurvey.json",
           followSurface = TRUE,
           flightAltitude = 100,
           demFn = "~/uav/uniwald/data/mrbiko.tif",
           windCondition = 2,
           followSurfaceRes=10,
           altFilter = 3
)

data(mrbiko)
writeRaster(mrbiko,"~/dem.tif")
fp<-makeAP(projectDir ="~/uav/cookbook",
           locationName = "firstSurvey",
           surveyArea="~/proj/drone/uniwald/myFirstSurvey.json",
           flightAltitude = 100,
           demFn ="~/dem.tif")


fp<-makeTP(projectDir ="/home/creu/uav/bayerwald",
         locationName = "filzmoosTree",
         missionTrackList="~/uav/bayerwald/Selected_trees_Filz.txt",
         demFn = "~/uav/grossfilz/grosserfilz.tif",
         windCondition = 2,
         uavType = "solo",
         followSurfaceRes=5,
         launchPos = c(13.409114897133804,48.92039612988935))

fp<-makeAP(projectDir ="/home/creu/uav/kellerwald",
           locationName = "traddelkopf30",
           surveyArea="~/uav/traddelkopf/Traddelkopf2016_09_19_30.json",
           followSurface = TRUE,
           flightAltitude = 30,
           demFn = "~/uav/kellerwald/data/Traddelkopf75_dem.tif",
           windCondition = 3,
           uavType = "djip3",
           followSurfaceRes = 5)



fp<-makeAP(projectDir ="/home/creu/uav/uniwald",
           locationName = "uniwald_75_watershed1",
           surveyArea="~/tmp/uniwald.json",
           followSurface = TRUE,
           flightAltitude = 75,
           demFn = "~/lehre/msc/active/msc-2016/data/gis/input/geonode-lidar_dsm_01m.tif",
           windCondition = 2,
           followSurfaceRes = 1,
           uavType = "solo"
           
)

hagenstein75_full<-makeAP(projectDir ="/home/creu/uav/gentree/hagenstein",
                          locationName = "hagenstein75_full",
                          surveyArea="~/uav/gentree/hagenstein/hagesteinall.json",
                          followSurface = TRUE,
                          flightAltitude = 75,
                          followSurfaceRes = 1,
                          altFilter = 2.5,
                          demFn = "/home/creu/uav/gentree/hagenstein/data/dgm2.tif",
                          overlap = 0.8,
                          windCondition = 2,
                          maxSpeed = 15,
                          uavType = "solo",
                          dA = TRUE,
                          rcRange = TRUE,
                          heatMap = TRUE,
                          picFootprint = TRUE
)


hagenstein50_full<-makeAP(projectDir ="/home/creu/uav/gentree/hagenstein",
                          locationName = "hagenstein50_full",
                          surveyArea=c(51.16435558686152,8.909043073654177,51.15986111067753,8.91019105911255,51.159491036988726,8.907798528671266,51.15936992131852,8.906350135803224),
                          followSurface = TRUE,
                          flightAltitude = 50,
                          followSurfaceRes = 1,
                          altFilter = .5,
                          demFn = "/home/creu/uav/gentree/hagenstein/data/dgm2.tif",
                          overlap = 0.8,
                          windCondition = 1,
                          maxSpeed = 10,
                          uavType = "djip3"
)




  mapview(fp$wp,zcol = "altitude",cex=4, lwd=0.5)+
  mapview(fp$lp,color = "red", lwd=1,cex=4)+
  mapview(fp$fA,color="blue", alpha.regions = 0.1,lwd=0.5)+
  mapview(fp$oDEM,col=terrain.colors(256))
