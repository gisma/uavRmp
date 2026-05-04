## Unmanned Aerial Vehicle R based Mission Planning - uavRmp

<!-- badges: start -->
[![cran checks](https://badges.cranchecks.info/worst/uavRmp.svg)](https://cran.r-project.org/web/checks/check_results_uavRmp.html)
![monthly](https://cranlogs.r-pkg.org/badges/uavRmp)
![total](https://cranlogs.r-pkg.org/badges/grand-total/uavRmp)
[![CRAN](https://www.r-pkg.org/badges/version/uavRmp?color=009999)](https://cran.r-project.org/package=uavRmp)
[![](https://img.shields.io/github/stars/gisma/uavRmp?style=flat)](https://github.com/gisma/uavRmp)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
<!-- badges: end -->

The [uavRmp](https://github.com/gisma/uavRmp) package provides `R` based mission planning tools for autonomous UAV survey flights. It was originally designed as a lightweight open source planning workflow for low-budget UAV mapping missions, including terrain-aware flight planning, battery-dependent task splitting, and safe departure and approach sections for each mission chunk.

Typical use cases include survey flights for Digital Surface Models (DSM), Digital Elevation Models (DEM), orthophotos, point clouds, land use and landscape classification, NDVI products, and forest structure mapping.

## Project status

`uavRmp` is entering maintenance mode.

The development context has changed substantially since the package was started. As of 2026, the new Litchi Hub has become a practical low-budget baseline tool for many DJI-based mapping missions. It provides browser-based mission planning, area mapping for photogrammetry, Google Earth based 3D planning and simulation, KML/KMZ import, and elevation-aware workflows. For many ordinary and moderately demanding DJI survey missions, including terrain-aware planning and 3D visual mission preparation, Litchi Hub is now sufficient as the primary planning environment.

Therefore, `uavRmp` is no longer intended to compete with Litchi Hub as a general-purpose low-budget DJI mission planner.

Version `0.8` is planned as the final feature release. After that, the package will be maintained in bug-fix mode only. The main purpose of future maintenance is to keep existing workflows usable, not to extend the package into a new general UAV planning platform.

`uavRmp` remains useful for specialised workflows where the mission is not simply drawn in a graphical interface, but computed from external geospatial data. This includes reproducible `R`/GIS workflows, custom terrain models, explicit terrain-following logic, batch generation of survey missions, conversion between mission formats, and legacy support for existing Pixhawk, 3DR Solo, and Litchi CSV workflows.

## Introduction

The majority of the open UAV community uses the Pixhawk autopilot ecosystem together with Ground Control Station software such as [Mission Planner](https://ardupilot.org/planner/) or [QGroundControl](https://qgroundcontrol.com/). Both are well documented and provide APIs and graphical user interfaces. However, depending on the workflow, they still offer limited support for fully reproducible terrain-following survey planning, battery-dependent task splitting, and explicit safe departure and approach generation.

`uavRmp` bridges this gap by generating mission files that can be uploaded to Pixhawk-based flight controllers directly or via Ground Control Station software. It also provides export and conversion workflows for Litchi-compatible DJI missions.

The package should be understood as a specialised mission generation and conversion toolkit. It is not a flight safety system and it does not replace field checks, legal checks, aircraft-specific mission validation, or pilot responsibility.

## Supported UAV platforms

Up to now, `uavRmp` has mainly supported low-budget ready-to-fly UAVs, including DJI drones supported by [Litchi](https://flylitchi.com/) and Pixhawk-based platforms such as Yuneec UAVs or the outdated but still usable 3DR Solo.

The core planning tool `makeAP()` creates area survey flight plans. Depending on the selected output workflow, it creates intermediate flight control files for DJI/Litchi workflows or waypoint files for the Pixhawk family.

The option `useMP` supports conversion-oriented workflows. It can be used to process survey missions planned with `QGroundControl` or `Mission Planner` and convert them into formats readable by Litchi or Pixhawk-compatible systems, including safe connection flights to the mission start, return sections, and task splitting.

DJI support should now be interpreted differently from earlier package versions. Historically, Litchi was mainly used as a way to execute externally generated CSV waypoint files on DJI consumer drones. With the new Litchi Hub, many standard DJI mapping missions can be planned directly in the browser. For DJI users, `uavRmp` is therefore primarily useful when missions are generated from external GIS data, when the terrain-following logic must be controlled explicitly, or when an existing reproducible `R` workflow has to be preserved.

Pixhawk-based UAVs remain the more open platform for scriptable and reproducible mission planning. `uavRmp` continues to provide MAVLink-compatible output for Pixhawk workflows.

## News

**NOTE:** Starting with version `0.8`, `uavRmp` is planned to enter its final feature release line. After that, the package will receive bug fixes only.

**NOTE:** As of 2026, Litchi Hub is suitable as a low-budget baseline planning tool for many DJI mapping workflows, including ordinary and moderately demanding terrain-aware missions and 3D visual mission preparation. For such missions, direct Litchi Hub planning is usually the preferred workflow.

**NOTE:** The Litchi export remains available as a compatibility and special-purpose interface. Use it when flight plans are computed from external geospatial data, when mission geometry must be generated reproducibly from `R`, or when conversion from `QGroundControl`/`Mission Planner` workflows to Litchi is required.

**NOTE:** Starting with version `0.6.3`, the `raster` package was removed. Raster input should be provided as `terra` `SpatRaster` objects.

**NOTE:** You may use the survey planning tools of `QGroundControl` or `Mission Planner` and convert their missions either to DJI-compatible Litchi format or to MAVLink files. This includes safe flights to the mission start, return-to-home sections, and task splitting. A simple GUI interface is available via:

```r
shiny::runApp(system.file("shiny/plan2litchi/", "app.R", package = "uavRmp"))
```

**NOTE:** DJI control files are designed for use with the proprietary `Litchi` flight control app exchange format, while Pixhawk/3DR Solo files use the `MAVLink` common message format used by the Pixhawk flight controller family.

## Installation

You need GDAL and the usual geospatial system libraries installed. For some of the older 3DR Solo related functions, additional Python libraries such as `dronekit` may be required.

The recommended way to install the current development version from GitHub is `pak`:

```r
install.packages("pak")
pak::pak("gisma/uavRmp")
```

If `pak` is already installed, use:

```r
pak::pak("gisma/uavRmp")
```

The older `devtools::install_github()` workflow is no longer the recommended installation path for this package. It may still work in existing development environments, but new installations should use `pak`.

## Basic usage

A minimal area planning workflow uses a survey area and a DEM:

```r
library(uavRmp)

fn <- system.file("extdata", "mrbiko.tif", package = "uavRmp")
fa <- system.file("extdata", "flightarea.kml", package = "uavRmp")

fp <- makeAP(
  surveyArea = fa,
  demFn = fn
)
```

The resulting object contains the planned mission geometry and the generated mission control output. Depending on the selected options, the output can be used for Litchi-compatible DJI workflows or Pixhawk/MAVLink workflows.

## When to use Litchi Hub directly

Use direct Litchi Hub planning when:

- the survey area can be drawn manually or imported as a simple KML/KMZ mapping polygon,
- the mission is a standard photogrammetry grid,
- overlap, speed, gimbal angle, and capture settings can be handled in Litchi,
- Litchi's terrain and 3D planning tools are sufficient,
- manual browser-based planning is acceptable.

This is now the recommended low-budget workflow for many ordinary DJI mapping tasks.

## When to use `uavRmp`

Use `uavRmp` when:

- the flight plan must be generated reproducibly from `R` code,
- the survey geometry is derived from GIS analysis,
- terrain-following logic must be computed explicitly,
- a custom high-resolution DEM is required in the planning workflow,
- many similar missions must be generated automatically,
- mission chunks must be split according to battery or distance constraints,
- safe departures and approaches must be generated programmatically,
- existing Pixhawk, 3DR Solo, or Litchi CSV workflows must be preserved,
- `QGroundControl` or `Mission Planner` missions must be converted into Litchi-compatible output.

In short: Litchi Hub is now the practical baseline tool for many DJI missions. `uavRmp` remains relevant where the mission is not merely planned interactively, but computed from geospatial data.

## Safety warning

Autonomous UAV missions can cause serious damage if coordinates, altitude references, terrain models, camera parameters, launch positions, or flight modes are wrong.

Always double-check generated missions before flying. Validate the mission in the target flight software, inspect altitudes and waypoint order, check the launch position, check terrain clearance, and perform a conservative field assessment.

Use `uavRmp` as a planning and conversion tool only. The pilot remains responsible for legal compliance, field safety, aircraft behaviour, and mission execution.

## License

GPL (>= 3)