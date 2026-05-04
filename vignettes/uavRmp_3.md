---
title: "Export to Litchi"
author: "Chris Reudenbach"
date: '2026-05-04'
editor_options:
  chunk_output_type: console
output:
  # html_document: 
  #   theme: united
  #   toc: true
#  rmarkdown: default
  pdf_document:
    latex_engine: xelatex
    toc: true
urlcolor: blue
vignette: >
  %\VignetteIndexEntry{Export to Litchi}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
---




# Introduction

Originally, both the planning and output of the control files with `uavRmp` was intended for terrain and surfaces with high relief energy. The package supported direct upload to the `3DR Solo` drone and the CSV based interface of the `Litchi` software for `DJI` drones, respectively.

At the time this workflow was designed, `Litchi` was mainly used as a waypoint execution and mission management tool. Systematic mapping flights, terrain-aware planning, and 3D preview functionality had to be prepared externally. In this context, `uavRmp` provided a relevant planning layer by generating terrain-aware survey routes and exporting them as Litchi-compatible CSV control files.

This role has changed substantially. Since the introduction of the new `Litchi Hub` in 2025, `Litchi` itself provides browser-based flight planning, area mapping for photogrammetry, Google Earth based 3D planning, 2D/3D flight simulation, KML/KMZ import for mapping areas, and improved mission management. Litchi also supports user-provided elevation data for mission planning. In the classic Mission Hub workflow, elevation data can be imported as Digital Elevation Models in Esri ASCII Grid format (`.asc`) using the WGS-84 coordinate system. In the newer Litchi Hub, additional map and overlay functionality is available, including GeoTIFF overlays.

Consequently, the Litchi export functionality in `uavRmp` is no longer required for standard rectangular or polygon-based area mapping missions under ordinary terrain conditions. For many routine DJI mapping applications, such missions can now be created directly in Litchi Hub.

The `uavRmp` workflow remains relevant for specialised use cases, for example when flight lines must be generated from a custom terrain model, when terrain-following behaviour has to be controlled explicitly before import into a flight app, when reproducible mission generation from R/GIS data is required, or when survey geometry has to be derived from external geospatial analysis rather than drawn manually in a web interface.

## PixHawk based UAVs

The `PixHawk` based UAVs can be flown directly from `QGroundControl`.

**Note:** If a higher resolved terrain model is to be taken into account for flight planning, `uavRmp` **must** be used beforehand to perform this terrain analysis. The generated control file must then be reloaded into `QGroundControl` as a waypoint file and can then be used as a flight control file.

Open the control file via `QGroundControl` or one of its derivatives and proceed as usual.

3DR Solo:


``` r
solo_upload("export_1001_solo.waypoints")
```

## DJI UAVs as supported by Litchi

For `DJI` consumer drones, `Litchi` remains a powerful and comparatively inexpensive flight control and mission planning environment. The output of both `QGroundControl` and `uavRmp` planned surveys can be exported as a `Litchi` CSV control file. After upload or import, the flights are available via the `Litchi Mission Hub` or the newer `Litchi Hub`.

For the classic Mission Hub workflow, open [Litchi Mission Hub](https://flylitchi.com/) and click on `Missions -> Import`. Then navigate to the generated control file, for example `firstSurvey_1001.csv`. To store the mission in the cloud, click `Missions -> Save`.

For the newer Litchi Hub workflow, open [Litchi Hub](https://hub.flylitchi.com/). Standard area mapping missions can often be planned directly in the browser using the built-in Area Mapping tools. Use the `uavRmp` export only when the mission geometry, altitude model, or terrain-following logic has been generated externally and must be transferred into Litchi as a reproducible control file.

## Current practical relevance of the Litchi export

The Litchi export should now be understood as a compatibility and special-purpose interface rather than as the default planning workflow for all DJI mapping flights.

Use direct Litchi Hub planning when:

* the survey area can be drawn or imported as a simple mapping polygon,
* the required flight is a standard photogrammetry grid,
* overlap, speed, gimbal angle, and capture settings can be configured directly in Litchi,
* the available Litchi elevation and preview functions are sufficient.

Use `uavRmp` before Litchi when:

* flight planning depends on a custom high-resolution DEM,
* terrain-following altitude logic must be computed explicitly,
* survey lines are derived from GIS analysis,
* the mission must be reproducible from R code,
* flight geometry must be generated automatically for many sites,
* the terrain or object surface has high relief energy and manual browser-based planning is insufficient.

In short, Litchi Hub now covers many standard mapping and 3D planning tasks directly. `uavRmp` remains useful where the flight plan is not merely drawn, but computed from geospatial data.



[1]: https://forum.flylitchi.com/t/open-beta-litchi-hub/23994 "[OPEN BETA] Litchi Hub - Beta Feedback - Litchi Forum"






