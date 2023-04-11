#' theme_bgg: custom theme for bgg data and ggplot2
#'
#' @description This is a custom theme for use with ggplot2
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' plot+theme_bgg()
theme_bgg <- function () {

        ggplot2::theme_minimal() %+replace%
                theme(
                        axis.title = element_text(),
                        # strip.background = element_rect(fill="grey100"),
                        #strip.text = element_text(colour = 'black'),
                        #  strip.text.x = element_text(size = 7),
                        #  strip.text.y = element_text(size = 7),
                        legend.position = "top",
                        # plot.subtitle = element_text(size = 10,
                        #                              hjust = 0),
                        axis.text.x = element_text(size=rel(0.9),
                                                   vjust = 1),
                        axis.text.y = element_text(size=rel(0.9),
                                                   hjust = 1),
                        plot.caption = element_text(size=8,
                                                    hjust = 1,
                                                    vjust = 1,
                                                    margin = unit(c(6,0,0,0), "points")),
                        legend.title = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.spacing = unit(6.5, "mm"),
                        plot.title = element_text(hjust = 0.5, size = 16),
                        plot.subtitle =  element_text(hjust = 0.5),
                        strip.text.x = element_text(size = 12),
                        panel.grid = element_line(colour = "grey90")

                )

}

