
# install.packages(c("leaflet", "httr", "jsonlite", "dplyr"))

#' Show golf courses on a map
#' @details Opens a leaflet map with clickable markers for each golf course in Amsterdam.
#' @export
#'
#' @examples
#' # Requires an interactive R session
#' show_golf_map()
#' @importFrom leaflet leaflet addTiles addMarkers
show_golf_map <- function() {
  courses <- data.frame(
    name = c("Golfbaan Sloten", "Amsterdam Old Course", "Golfclub Ookmeer",
             "Golf Waterland", "Golf Amsteldijk", "De Hoge Dijk",
             "Golf Club Spaarnwoude", "Haarlemmermeersche Golfclub",
             "Zaanse Golf Club", "Golfclub Almeerderhout"),
    lat = c(52.3381, 52.3194, 52.37460119926424, 52.41349233579652,
            52.28276096665965, 52.291218067495834, 52.433887183588254,
            52.350288071409175, 52.48429520152448, 52.35840239728196),
    lng = c(4.7967, 4.9472, 4.796779448899261, 4.942104673836031,
            4.8857106720634595, 4.961750474661375, 4.699245997874294,
            4.660662034138916, 4.870160491211002, 5.287113513993141)
  )

  leaflet::leaflet(courses) |>
    leaflet::addTiles() |>
    leaflet::addMarkers(
      ~lng, ~lat,
      popup = ~name
    )
}
