---
title: "Export to Litchi"
author: "Chris Reudenbach"
date: '2024-06-07'
editor_options:
  chunk_output_type: console
output:
  html_document: 
    theme: united
    toc: yes
  rmarkdown: default
  pdf_document:
    latex_engine: xelatex
    toc: yes
urlcolor: blue
vignette: >
  %\VignetteIndexEntry{Export to Litchi}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
---





# Introduction

Originally, both the planning and output of the control files with `uavRmp` was intended for terrain/surfaces with high relief energy and supported direct upload to the `3DR Solo` Drone and the CSV based interface of the `Litchi` software for `DJI` drones, respectively. 

## PixHawk based UAVs

The `PixHawk` based UAVs can be flown directly from `QGroundcontrol`. 

**Note:** If  a higher resolved resolution terrain is to be taken into account for planning, `uavRmp` **must** be used beforehand to perform this terrain analysis for flight planning. The control file must then be reloaded into `QGroundcontrol` as a waypoint file and can thean be used as a flight control file. 

Open the control file via QGroundcontrol or one of its derivates and proceed like usual. 

3DR Solo:
```solo_upload("export_1001_solo.waypoints")```

## DJI UAVs as supported by Litchi 
For the `DJI` consumer drones there is no opensource or inexpensive planning tool. As a flight control app, the powerful and very inexpensive `Litchi` is a good choice. The output of both `Qgroundcontrol` and `uavRmp` planned surveys is a `Litchi` CSV control file. After upload/import the flights are available via the `Litchi mission Hub` Cloud. 

Open [Litchi Mission](https://flylitchi.com/) and click on the button ''Missions->Import'' and navigate to the control file firstSurvey_1001.csv. To export it click ''Missions->Save''. 








