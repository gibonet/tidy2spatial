
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom dplyr select_ distinct_
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom maptools unionSpatialPolygons
#' @importFrom broom tidy
#' @import rgeos




#' @title Transforms a data frame with map coordinates in a SpatialPolygonsDataFrame.
#'
#' @description
#' Starting from a data frame with map coordinates, transforms
#' it in an object of class SpatialPolygonsDataFrame.
#'
#'
#' @param .data data frame with map coordinates of polygons
#'
#' @return an object of class SpatialPolygonsDataFrame (from package sp)
#'
#' @examples
#' data(g1k15)
#' g1k15_sp <- tidy2sp(g1k15)
#' class(g1k15_sp)
#' str(g1k15_sp)
#' @export
tidy2sp <- function(.data){
  tmp1 <- .data %>% split(f = list(.data[["id"]]), drop = TRUE)
  tmp1 <- lapply(tmp1, function(x) split(x, f = list(x[["group"]]), drop = TRUE))
  # Così ho una lista con due elementi (GR e TI), dove GR sono le coordinate dei Grigioni e TI è una lista
  # con le coordinate del TI e del suo hole

  # Polygon (ogni elemento della lista tmp1)
  # Vado, per il momento, con un ciclo for (lista 'nested')
  tmp1_polygon <- vector(mode = "list", length = length(tmp1))
  for(i in seq_along(tmp1)){
    tmp1_polygon[[i]] <- lapply(tmp1[[i]], function(x) Polygon(x[ , c("long", "lat")], hole = unique(x$hole)))
  }

  # Qua sotto da mettere a posto (così andava bene per la creazione di un solo Polygons)
  # tmp1_polygons <- Polygons(tmp1_polygon, ID = as.character(tmp1_data$id))  # Polygons (uno solo neh?)
  tmp1_data <- .data %>% distinct_(.dots = "id", .keep_all = TRUE) #%>% select_(.dots = "id")
  row.names(tmp1_data) <- as.character(tmp1_data[["id"]])

  tmp1_polygons <- vector(mode = "list", length = length(tmp1))
  for(i in seq_along(tmp1_polygons)){
    tmp1_polygons[[i]] <- Polygons(tmp1_polygon[[i]], ID = tmp1_data[["id"]][i])
  }

  # SpatialPolygons
  tmp1_spatialpolygons <- SpatialPolygons(tmp1_polygons)

  # SpatialPolygonsDataFrame
  tmp1_spatialpolygonsdataframe <- SpatialPolygonsDataFrame(tmp1_spatialpolygons, data = tmp1_data)

  tmp1_spatialpolygonsdataframe
}



# new_id = g1k15_sp@data$GRNR
# union_polygons(g1k15_sp, new_id = g1k15_sp@data$GRNR)

#' @title Aggregate Polygons in a SpatialPolygonsDataFrame object
#'
#' @description Aggregate Polygons in a SpatialPolygonsDataFrame object, based on a vector (new_id) of the same length of the number of polygons in the starting SpatialPolygonsDataFrame (argument .sp).
#'
#'
#' @param .sp SpatialPolygonsDataFrame
#' @param new_id A vector defining the output Polygons objects, equal in length to the length of the polygons slot of .sp; it may be character, integer, or factor
#'
#' @return an object of class SpatialPolygonsDataFrame
#'
#' @examples
#' data(g1k15)
#'
#' # Transform data frame g1k15 in a SpatialPolygonsDataFrame
#' g1k15_sp <- tidy2sp(g1k15)
#'
#' # Create a new SpatialPolygonsDataFrame, aggregating the polygons based on the GRNR (grand regions)
#' g1k15_sp_union <- union_polygons(g1k15_sp, new_id = g1k15_sp@data$GRNR)
#' str(g1k15_sp_union)
#'
#' @export
union_polygons <- function(.sp, new_id){
  x_data <- .sp@data
  stopifnot(length(new_id) == nrow(x_data))

  x_data[["new_id"]] <- new_id
  x_union_data <- x_data %>% distinct_(.dots = "new_id")
  x_union <- unionSpatialPolygons(.sp, IDs = new_id)

  x_union_sp <- SpatialPolygonsDataFrame(x_union, data = x_union_data)

  # Output: SpatialPolygonsDataFrame
  x_union_sp

}



# E aggiungo una nuova funzione: tidy2sp -> union_polygons -> tidy

#' @title Aggregate polygons in a tidy data frame with map coordinates
#'
#' @description Combine tidy2sp, union_polygons and broom::tidy, returning a tidy data frame with aggregated polygons, based on a new_id vector.
#'
#' @param .data data frame with map coordinates of polygons
#' @param new_id A vector defining the output Polygons objects, equal in length to the length of the polygons slot of .sp; it may be character, integer, or factor
#'
#' @return a data frame with map coordinates of aggregated polygons
#'
#' @examples
#' data(g1k15)
#'
#' # Aggregate cantons into grand regions
#' grand_regions <- unique(g1k15[ , c("KTNR", "GRNR")])[["GRNR"]]
#' g1k15_union <- union_tidy(g1k15, new_id = grand_regions)
#' str(g1k15_union)
#'
#' @export
union_tidy <- function(.data, new_id){
  x <- tidy2sp(.data)

  x_union_sp <- union_polygons(.sp = x, new_id = new_id)

  tidy(x_union_sp, region = "new_id")

}
# x <- union_tidy(g1k15, new_id = g1k15_sp@data$GRNR)
# maps2_(x)
