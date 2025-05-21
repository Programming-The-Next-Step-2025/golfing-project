# Install required packages if not already installed
# install.packages(c("leaflet", "httr"))

#' Get current weather by coordinates
#'
#' @param lat Latitude of the location.
#' @param lon Longitude of the location.
#' @param api_key OpenWeatherMap API key.
#'
#' @return Weather information of entered location.
#' @export
#' @importFrom httr GET content status_code
weather_by_coords <- function(lat, lon, api_key) {
  url <- paste0("https://api.openweathermap.org/data/2.5/weather?lat=",
                lat, "&lon=", lon, "&appid=", api_key, "&units=metric")

  response <- httr::GET(url)

  if (httr::status_code(response) != 200) {
    return("Weather data unavailable.")
  }

  data <- httr::content(response)

  temp <- data$main$temp
  humidity <- data$main$humidity
  pressure <- data$main$pressure
  wind_speed <- data$wind$speed
  wind_deg <- data$wind$deg
  clouds <- data$clouds$all
  visibility <- data$visibility
  desc <- data$weather[[1]]$description

  # Convert degrees to compass direction
  deg_to_dir <- function(deg) {
    dirs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    ix <- floor((deg + 22.5) / 45) %% 8 + 1
    dirs[ix]
  }
  wind_dir <- deg_to_dir(wind_deg)

  paste0(
    "Temp: ", temp, "°C<br>",
    "Humidity: ", humidity, "%<br>",
    "Pressure: ", pressure, " hPa<br>",
    "Wind: ", wind_speed, " m/s ", wind_dir, "<br>",
    "Cloudiness: ", clouds, "%<br>",
    "Visibility: ", visibility, " m<br>",
    "Conditions: ", desc
  )
}

# Adding weather to the final golf map------------------------------------------

#' Show golf courses with weather info on a map
#'
#' @param api_key Your OpenWeatherMap API key.
#'
#' @export
#' @importFrom leaflet leaflet addTiles addMarkers
#' @examples
#' golf_map_weather(api_key = "your_api_key_here")
golf_map_weather <- function(api_key) {
  courses <- data.frame(
    name = c("Golfbaan Sloten", "Amsterdam Old Course", "Golfclub Ookmeer",
             "Golf Waterland", "Golf Amsteldijk", "De Hoge Dijk",
             "Golf Club Spaarnwoude", "Haarlemmermeersche Golfclub",
             "Zaanse Golf Club", "Golfclub Almeerderhout"),
    lat = c(52.3381, 52.3194, 52.3746, 52.4135,
            52.2828, 52.2912, 52.4339,
            52.3503, 52.4843, 52.3584),
    lng = c(4.7967, 4.9472, 4.7968, 4.9421,
            4.8857, 4.9618, 4.6992,
            4.6607, 4.8702, 5.2871)
  )

  # Get weather info for each course
  courses$popup <- mapply(function(name, lat, lng) {
    weather_info <- weather_by_coords(lat, lng, api_key)
    paste0("<b>", name, "</b><br>", weather_info)
  }, courses$name, courses$lat, courses$lng)

  leaflet::leaflet(courses) |>
    leaflet::addTiles() |>
    leaflet::addMarkers(
      ~lng, ~lat,
      popup = ~popup
    )
}




