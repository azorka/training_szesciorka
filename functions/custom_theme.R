#' Create custom ggplot function for plotting
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