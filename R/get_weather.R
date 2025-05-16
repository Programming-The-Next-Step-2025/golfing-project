# install.packages(c("devtools", "roxygen2", "httr"))

#' Get current weather for a location
#'
#' Fetches the current weather for a given city using the OpenWeatherMap API.
#'
#' @param city A character string with the name of the city (e.g., "Amsterdam").
#' @param api_key A character string with your OpenWeatherMap API key.
#'
#' @return A list containing temperature, weather description, and location name.
#' @export
#'
#' @examples
#' get_weather("Amsterdam", api_key = "your_api_key_here")
#'
#' @importFrom httr GET content status_code
get_weather <- function(city, api_key) {
  url <- paste0("https://api.openweathermap.org/data/2.5/weather?q=", city, "&appid=", api_key, "&units=metric")
  response <- httr::GET(url)

  if (httr::status_code(response) != 200) {
    stop("Failed to fetch weather data. Check city name or API key.")
  }

  data <- httr::content(response)
  list(
    location = data$name,
    temperature = data$main$temp,
    weather = data$weather[[1]]$description
  )
}
