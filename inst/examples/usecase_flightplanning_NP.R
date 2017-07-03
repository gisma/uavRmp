# planning two different target tasks
hagen_c1 <- makeTP(projectDir ="~/proj/uav/gentree/Hagenstein",
                      locationName = "hagen_c1",
                      missionTrackList="~/proj/uav/gentree/Hagenstein/data/Hagenstein_NEU_sorted_North.csv",
                      demFn = "~/proj/uav/gentree/Traddelkopf/data/basic/DGM1_kellerwald.tif",
                      uavType = "solo",
                      followSurfaceRes=1,
                      flightAltitude = 60,
                      aboveTreeAlt = 45,
                      launchPos = c(8.905967,51.161932))

# retrieving the current logfile
soloLog(logDir = "/home/creu/temp3",
        downloadOnly = TRUE,logFiles = "solo.tlog")

traddel_c1 <- makeTP(projectDir ="~/proj/uav/gentree/Traddelkopf",
                        locationName = "traddel_c1",
                        missionTrackList="~/proj/uav/gentree/Traddelkopf/data/Traddelkopf_NEU_sorted_core_1.csv",
                        demFn = "~/proj/uav/gentree/Traddelkopf/data/basic/DGM1_kellerwald.tif",
                        uavType = "solo",
                        followSurfaceRes=1,
                        flightAltitude = 60,
                        aboveTreeAlt = 45,           
                        launchPos = c(8.976860,51.1322252))

# retrieving the current logfile
soloLog(logDir = "/home/creu/temp3",
        downloadOnly = TRUE,logFiles = "solo.tlog")

