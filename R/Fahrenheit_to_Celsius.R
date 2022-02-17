airtemps <- c(212, 30.3, 78, 32)

celsius1 <- (airtemps[1]-32)*5/9
celsius2 <- (airtemps[2]-32)*5/9
celsius3 <- (airtemps[3]-32)*5/9

Fahrenheit_to_Celsius <- function(Fahrenheit) {
  Celsius <- (Fahrenheit-32)*5/9
  return(Celsius)
}

celsius4 <- Fahrenheit_to_Celsius(Fahrenheit=airtemps[1])
celsius4

celsius1 == celsius4

airtemps_C <- Fahrenheit_to_Celsius(Fahrenheit=airtemps)

#' Convert temperature data from Celsius to Fahrenheit
#' 
#' @param Celsius Temperature data in degrees Celsius (as a vector) to be converted
#' @return temperature value in degrees Fahrenheit
#' @keywords conversion
#' @export
#' @examples
#' Celsius_to_Fahrenheit(12)
#' Celsius_to_Fahrenheit(12, 100, 25)
Celsius_to_Fahrenheit <- function(Celsius) {
  Fahrenheit <- (Celsius*9/5)+32
  return(Fahrenheit)
}

airtemps_F <- Celsius_to_Fahrenheit(Celsius=airtemps_C)
airtemps == airtemps_F

#' Convert temperature data from Fahrenheit to Celsius
#'
#' @param Fahrenheit Temperature data in degrees Fahrenheit (as a vector) to be converted
#' @return temperature value in degrees Celsius
#' @keywords conversion
#' @export
#' @examples
#' Fahrenheit_to_Celsius(32)
#' Fahrenheit_to_Celsius(c(32, 212, 72))
Fahrenheit_to_Celsius <- function(Fahrenheit) {
  Celsius <- (Fahrenheit-32)*5/9
  return(Celsius)
}

# multiple conversions
convert_temps <- function(Fahrenheit) {
  Celsius <- (Fahrenheit-32)*5/9
  Kelvin <- Celsius + 273.15
  return(list(Fahrenheit=Fahrenheit, Celsius=Celsius, Kelvin=Kelvin))
}

temps_df <- data.frame(convert_temps(seq(-100,100,10)))

source("../functions/custom_theme.R")

#' Create function for plotting
#'
#' @param base_size 
#'
#' @return
#' @export
#'
#' @examples
custom_theme <- function(base_size=9) {
  ggplot2::theme(
    axis.ticks      =ggplot2::element_blank(),
    text            =ggplot2::element_text(family='Helvetica', color='gray30', size=base_size),
    plot.title      =ggplot2::element_text(size=ggplot2::rel(1.25), hjust=0.5, face='bold'),
    panel.background=ggplot2::element_blank(),
    legend.position ='right',
    panel.border    =ggplot2::element_blank(),
    panel.grid.minor=ggplot2::element_blank(),
    panel.grid.major=ggplot2::element_line(colour='grey90', size=.25),
    legend.key      =ggplot2::element_rect(colour=NA, fill=NA),
    axis.line       =ggplot2::element_blank()
  )
}

library(ggplot2)

ggplot(data=temps_df, mapping=aes(x=Fahrenheit, y=Celsius, color=Kelvin)) +
  geom_point() +
  custom_theme(base_size=12)

scatterplot <- function(df, point_size=2.5, font_size=9) {
  ggplot(data=df, mapping=aes(x=Fahrenheit, y=Celsius, color=Kelvin)) +
    geom_point(size=point_size) +
    custom_theme(font_size)
}

scatterplot(df=temps_df, point_size=3, font_size=16)

