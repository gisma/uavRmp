#' Rectangle flight area around points
#'
#' Creates optimal rectangle area around points
#'
#' @param points a sf object, points you want to fly over
#' @param buffer buffer distance between the points and the rectangle; defaults 0
#' @param epsg reference system
#'
#' @return SpatialPoints: Corners of the flight area
#'
#' @author Marvin Ludwig
#'
#' @details The code is based on a Rotating Caliper Algorithm and mostly copy and pasted (see reference)
#'
#' @references http://dwoll.de/rexrepos/posts/diagBounding.html
#'
#' @export

minBB <- function(points, buffer = 0, epsg = 25832){

  # input and conversion
  #--------------------------
  xy <- do.call(rbind, sf::st_geometry(points))


  # call the Rotating Caliper Algorithm
  pts <- optimalBB(xy)

  # convert to polygon
  sf_pts <- sf::st_sfc(sf::st_polygon(list(pts)), crs = epsg)

  # square buffer
  pts_buffer <- sf::st_buffer(sf_pts, dist = buffer, nQuadSegs = 40, endCapStyle = "FLAT",
            joinStyle = "MITRE", mitreLimit = Inf)

  pts_buffer <- sf::st_sfc(pts_buffer, crs = epsg)

  # return spatial object
  return(pts_buffer)

}


#-------------------------------------------------------
# here starts the copies algorithm from:
# http://dwoll.de/rexrepos/posts/diagBounding.html


optimalBB <- function(xy){
  stopifnot(is.matrix(xy), is.numeric(xy), nrow(xy) >= 2, ncol(xy) == 2)

  ## rotating calipers algorithm using the convex hull
  H    <- grDevices::chull(xy)      ## hull indices, vertices ordered clockwise
  n    <- length(H)      ## number of hull vertices
  hull <- xy[H, ]        ## hull vertices

  ## unit basis vectors for all subspaces spanned by the hull edges
  hDir  <- diff(rbind(hull, hull[1, ])) ## hull vertices are circular
  hLens <- sqrt(rowSums(hDir^2))        ## length of basis vectors
  huDir <- diag(1/hLens) %*% hDir       ## scaled to unit length

  ## unit basis vectors for the orthogonal subspaces
  ## rotation by 90 deg -> y' = x, x' = -y
  ouDir <- cbind(-huDir[ , 2], huDir[ , 1])

  ## project hull vertices on the subspaces spanned by the hull edges, and on
  ## the subspaces spanned by their orthogonal complements - in subspace coords
  projMat <- rbind(huDir, ouDir) %*% t(hull)

  ## range of projections and corresponding width/height of bounding rectangle
  rangeH  <- matrix(numeric(n*2), ncol=2)  ## hull edge
  rangeO  <- matrix(numeric(n*2), ncol=2)  ## orthogonal subspace
  widths  <- numeric(n)
  heights <- numeric(n)

  for(i in seq(along=numeric(n))) {
    rangeH[i, ] <- range(projMat[  i, ])

    ## the orthogonal subspace is in the 2nd half of the matrix
    rangeO[i, ] <- range(projMat[n+i, ])
    widths[i]   <- abs(diff(rangeH[i, ]))
    heights[i]  <- abs(diff(rangeO[i, ]))
  }

  ## extreme projections for min-area rect in subspace coordinates
  ## hull edge leading to minimum-area
  eMin  <- which.min(widths*heights)
  hProj <- rbind(   rangeH[eMin, ], 0)
  oProj <- rbind(0, rangeO[eMin, ])

  ## move projections to rectangle corners
  hPts <- sweep(hProj, 1, oProj[ , 1], "+")
  oPts <- sweep(hProj, 1, oProj[ , 2], "+")

  ## corners in standard coordinates, rows = x,y, columns = corners
  ## in combined (4x2)-matrix: reverse point order to be usable in polygon()
  ## basis formed by hull edge and orthogonal subspace
  basis <- cbind(huDir[eMin, ], ouDir[eMin, ])
  hCorn <- basis %*% hPts
  oCorn <- basis %*% oPts
  pts   <- t(cbind(hCorn, oCorn[ , c(2, 1)]))

  ## angle of longer edge pointing up
  dPts <- diff(pts)
  e    <- dPts[which.max(rowSums(dPts^2)), ] ## one of the longer edges
  eUp  <- e * sign(e[2])       ## rotate upwards 180 deg if necessary
  deg  <- atan2(eUp[2], eUp[1])*180 / pi     ## angle in degrees


  pts <- rbind(pts, pts[1,])
  return(pts)
}



