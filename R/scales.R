#' retinal palette colors ramped to a specified length
#'
#' @param n Number of colors to display
#'
#' @param palette Choose from 'jens_palettes' list
#'
#' @param alpha transparency
#'
#' @param reverse If TRUE, the direction of the colours is reversed.
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' library(jenshelper)
#' image(volcano, col = retinal("aurora", 20))
#' @export
retinal <- function(palette = "aurora", n, alpha = 1, reverse = FALSE) {

  pal <- retinal_palettes[[palette]]

  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (reverse) {
    pal <- rev(pal)
  }

  pal <- colorRampPalette(pal, alpha)(n)

  return(pal)

}

#' retinal palette with ramped colours
#'
#' @param palette Choose from 'retinal_palettes' list
#'
#' @param alpha transparency
#'
#' @param reverse If TRUE, the direction of the colours is reversed.
#'
#' @importFrom grDevices colorRampPalette
retinal_pal <- function(palette = "aurora", alpha = 1, reverse = FALSE) {

  function(n) {
    retinal(palette, n, alpha, reverse)
  }

}


#' retinal color scale for ggplot2
#'
#' @rdname scale_color_retinal
#'
#' @param palette Choose from 'retinal_palettes' list
#'
#' @param reverse logical, Reverse the order of the colours?
#'
#' @param alpha transparency
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @inheritParams viridis::scale_color_viridis
#'
#' @importFrom ggplot2 scale_colour_manual
#'
#' @examples
#' library(ggplot2)
#' library(retinal)
#'
#' ggplot(diamonds) +
#'  geom_point(aes(x = carat, y = price, color = cut)) +
#'  scale_color_retinal("lumina")
#'
#' @export
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
scale_color_retinal <- function(palette = "aurora", discrete = TRUE, alpha = 1, reverse = FALSE, ...) {

  if (discrete) {
    discrete_scale("colour", "retinal", retinal_pal(palette, alpha = alpha, reverse = reverse), ...)
  }
  else {
    scale_color_gradientn(colours = retinal(palette, 256, alpha = alpha, reverse = reverse), ...)
  }
}

#' @rdname scale_color_retinal
#' @export
scale_colour_retinal <- scale_color_retinal


#' #' retinal fill scale for ggplot2
#'
#' @param palette Choose from 'retinal_palettes' list
#'
#' @inheritParams viridis::scale_fill_viridis
#' @inheritParams retinal_pal
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @importFrom ggplot2 scale_fill_manual discrete_scale scale_fill_gradientn
#'
#' @examples
#' library(ggplot2)
#' library(retinal)
#'
#' ggplot(diamonds) +
#'   geom_bar(aes(x = cut, fill = clarity)) +
#'   scale_fill_retinal("victory_bonds")
#' @export
scale_fill_retinal <- function(palette = "aurora", discrete = TRUE, alpha = 1, reverse = FALSE, ...) {

  if (discrete) {
    discrete_scale("fill", "retinal", retinal_pal(palette, alpha = alpha, reverse = reverse), ...)
  }
  else {
    scale_fill_gradientn(colours = retinal(palette, 256, alpha = alpha, reverse = reverse), ...)
  }
}
