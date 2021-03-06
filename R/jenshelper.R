# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

################################################################################
########################## FUNCTION DEFINITIONS ################################
################################################################################


#' Inverse hyperbolic sine transformation
#' Can normalize data by log(x + sqrt(x ^ 2 + 1)
#' @param x variable (e. g. vector) to be transformed
#'
#' @return
#' @export
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}


#' Simple median imputation
#' Multiple imputation with Amelia & mice is better for a set with several missing data points.
#' @param x variable with missing values
#'
#' @return imputed data with median of existing data plus random number
#' @export
impmed <- function(x) {
  median(x, na.rm = T) + rnorm(sum(is.na(x))) * sd(x, na.rm = T)
}



#' T1 mat to dataframe
#' Turn tableone output matrix into tidyverse data_frame
#' @param mat
#'
#' @return
#' @export
#'
#' @examples
tableone_mat_to_data_frame <- function(mat) {
  bind_cols(data_frame(Variable = rownames(mat)),
            as_data_frame(mat))
}


#' Write a matrix via data frame to an xlsx file
#' For Tableone objects: It writes a tableone (w/o print) to Excel file
#' @param df
#' @param file
#' @param font_size
#'
#' @return an .xlsx file
#'
#' @export
write_tableone_to_xlsx <- function(mat, file) {
  write_df_to_xlsx(df = s(mat),
                   file = file,
                   font_size = 8)
}

#' @export
write_df_to_xlsx <- function(df, file, font_size) {
  ## Create a workbook object with one sheet
  ## https://rdrr.io/cran/openxlsx/man/setColWidths.html
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "1")

  ## Write data frame data to the workbook object
  openxlsx::writeData(wb, sheet = 1, x = df)

  ## Format the variable name column
  ## https://rdrr.io/cran/openxlsx/man/createStyle.html
  varname_style <- openxlsx::createStyle(fontSize = font_size, halign = "left", valign = "center")
  openxlsx::addStyle(wb, sheet = 1, style = varname_style,
           rows = seq_len(nrow(df) + 1),
           cols = 1,
           gridExpand = TRUE)

  ## Format all other columns
  varval_style <- openxlsx::createStyle(fontSize = font_size, halign = "center", valign = "center")
  openxlsx::addStyle(wb, sheet = 1, style = varval_style,
           rows = seq_len(nrow(df) + 1),
           cols = seq_len(ncol(df))[-1],
           gridExpand = TRUE)

  ## Fix column width automatically
  openxlsx::setColWidths(wb, sheet = 1, cols = seq_len(ncol(df)), widths = "auto")

  ## Save to a file
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}

#' A customized ggplot2 theme for bar charts
#' It has no x axis line, no grid. Does not work with coord_flip() therefore.
#' Based on Claus Wilkens function but heavily modified.
#' *Type scales*
#' - __default__ is 1.25, a major third,
#' - golden ratio: (1 + sqrt(5)) / 2,
#' - perfect fourth: 1.333,
#' - augmented 4th: 1.414
#' @param font_size base font size (e.g. axis labels)
#' @param font_family font
#' @param line_size the line width
#' @param rel_small a fraction or relative type scale factor (e.g. axis numbers)
#' @param rel_tiny a fraction or relative type scale factor (e.g. subtitles)
#' @param rel_large a fraction or relative type scale factor (e.g. titles)
#'
#' @return a theme that can be + theme_jens() to a ggplot()
#' @export
theme_jb <- function(font_size = 19, font_family = "IBM Plex Sans Condensed",
                     line_size = 0.54, rel_small = 15.2/19, rel_tiny = 12.2/19, rel_large = 23.75/19) {

  half_line  <- font_size / 2
  small_size <- rel_small * font_size

  theme_minimal(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text = element_text(
        family = font_family,
        face = "plain",
        color = "black",
        size = font_size,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.9,
        margin = margin(),
        debug = FALSE),
      axis.line = element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x = element_blank(),
      axis.line.y = NULL,
      axis.text = element_text(color = "black",
                               size = small_size),
      axis.text.x = element_text(margin = margin(t = half_line),
                                 size = font_size,
                                 vjust = 1),
      axis.text.x.top = element_text(margin = margin(b = small_size),
                                     vjust = 0),
      axis.text.y = element_text(margin = margin(r = small_size),
                                 size = small_size,
                                 hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = small_size),
                                       hjust = 0),
      axis.ticks = element_line(color = "black", size = line_size),
      axis.ticks.length = unit(half_line, "pt"),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(angle = 90, margin = margin(r = font_size * rel_tiny), # axis title orientation!
                                  vjust = 1),
      axis.title.y.right = element_text(angle = -90,
                                        margin = margin(l = half_line),
                                        vjust = 0),

      legend.position = "none",

      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major = NULL,
      panel.grid.minor = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing = unit(half_line, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,

      strip.background = element_rect(fill = "grey80"),
      strip.text = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line /
                          2, half_line / 2,
                        half_line / 2)
      ),
      strip.text.x = NULL,
      strip.text.y = element_text(angle = -90),
      strip.placement = "inside",
      strip.placement.x = NULL,
      strip.placement.y = NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background = element_blank(),
      plot.title = element_text(
        face = "plain",
        size = rel(rel_large),
        hjust = 0.5,
        vjust = 0.5,
        margin = margin(b = half_line)
      ),
      plot.subtitle = element_text(
        size = rel(rel_small),
        hjust = 0,
        vjust = 1,
        margin = margin(b = font_size)
      ),
      plot.caption = element_text(
        size = rel(rel_tiny),
        hjust = 1,
        vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag = element_text(
        face = "bold",
        hjust = 0,
        vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin = margin(half_line,
                           half_line, half_line, half_line),

      aspect.ratio = 1.618,
      complete = TRUE
    )
}




#' Write a data frame to an xlsx file (TableOne)
#'
#' @param mat a matrix, i. e. from TableOne
#' @param file destination file
#' @param font_size
#'
#' @return an Excel file with Table
#' #'
#' @examples
#' @export
write_df_to_xlsx <- function(mat, file, font_size=9) {
  ## Create a workbook object with one sheet
  ## https://rdrr.io/cran/openxlsx/man/setColWidths.html
  df <- bind_cols(data_frame(Variable = rownames(mat)),
                  as_tibble(mat))
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "1")

  ## Write data frame data to the workbook object
  openxlsx::writeData(wb, sheet = 1, x = df)

  ## Format the variable name column
  ## https://rdrr.io/cran/openxlsx/man/createStyle.html
  varname_style <- openxlsx::createStyle(fontSize = font_size, halign = "left", valign = "center")
  openxlsx::addStyle(wb, sheet = 1, style = varname_style,
                     rows = seq_len(nrow(df) + 1),
                     cols = 1,
                     gridExpand = TRUE)

  ## Format all other columns
  varval_style <- openxlsx::createStyle(fontSize = font_size, halign = "center", valign = "center")
  openxlsx::addStyle(wb, sheet = 1, style = varval_style,
                     rows = seq_len(nrow(df) + 1),
                     cols = seq_len(ncol(df))[-1],
                     gridExpand = TRUE)

  ## Fix column width automatically
  openxlsx::setColWidths(wb, sheet = 1, cols = seq_len(ncol(df)), widths = "auto")

  # Save to a file
  if (grepl('\\.xlsx$', file)) {
    openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  } else {
    openxlsx::saveWorkbook(wb, file = paste0(file, ".xlsx"), overwrite = TRUE)
  }
}



#' Get proper axis label for outcome variable
#' Named vector as look-up table
#' @param var "fida", "fidv", "fica", "avr", "crae", "crve", "fmd", "gtn", "pwv" or "aix"
#' @param lookupvector names_long is saved in fun
#'
#' @return axis label for plot
#' @export
#'
#' @examples
get_axislabel <- function(var, lookupvector = names_long){
  names_long <- c("Arterial Flicker-induced Dilatation (%)",
                  "Venous Flicker-induced Dilatation (%)",
                  "Arterial Flicker-induced max. Contraction (%)", "AVR",
                  "CRAE (eq.)", "CRVE (eq.)", "Flow-Mediated Dilatation (%)",
                  "GTN (%)", "Pulse Wave Velocity (%)", "Augmentation Index")
  vars_outc <- c("fida", "fidv", "fica", "avr", "crae", "crve",
                 "fmd", "gtn", "pwv", "aix")
  names(names_long) <- vars_outc
  myvalue <- lookupvector[var]
  myvalue <- unname(myvalue)
  return(myvalue)
}






#' Plot survey bar plots
#'
#' @param df a survey design obsject (survey or srvyr)
#' @param out a single variable of interest
#' @param upper custom max value of y axis
#' @param fillncolor any palette from \code{\link[jenshelper]{retinal_palettes}}
#'
#' @return a ggplot dynamite plot with errorbars
#' @examples
#' jenshelper::retinal_palettes # for choice of palettes
#' @export
jb_figure <- function(df = des_amy, out = fida, upper = NA, pal = "tol3" ) {
  # check that df is a survey object
  stopifnot("You need to provide a survey or svyr design object."=
              rlang::inherits_any(df, c("survey.design", "survey.design2", "tbl_svy")) &&
              !rlang::is_empty(df))
  # set axis labels with custom function
  laby = get_axislabel(paste(substitute(out)))
  # create errorbar limits and calculate ipw mean
  df_m <- {{df}} %>%
    group_by(group) %>%
    select({{out}}, group) %>%
    summarise(mean = survey_mean({{out}}, na.rm = T)) %>%
    transmute(mean = mean,
              se = mean_se,
              low = mean - mean_se,
              high = mean + mean_se,
              group = group)
  # find reasonable default y_max
  roundup <- if (is.numeric(upper)) upper else max(ceiling(1.4 * df_m$mean), na.rm = T)

  plot <- ggplot2::ggplot(df_m, aes(x = group, y = mean, fill = group, color = group)) +
    ggplot2::geom_bar(stat = "identity", width = 0.8) +
    ggplot2::geom_errorbar(aes(ymin = low, ymax = high), width = 0.27, size = 1) +
    ggplot2::ylab(laby) +
    ggplot2::expand_limits(x = 1, y = roundup) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +  # c(0.002,0) if color==black
    # scale_fill_grey(start = .92, end = .25), # Andreas' favorite
    jenshelper::scale_fill_retinal(substitute(pal)) +
    jenshelper::scale_color_retinal(substitute(pal)) +
    theme_jb()
  plot
}


#' checkPacks
#'
#' Using NCmisc this fun extracts R code of all files under "path" and looks for all used funs and package they are from.
#' @param path
#'
#' @return table with used packages (only direct!) and count of number of uses
#' @export
checkPacks <- function(path){

  ## get all R files in your directory
  ## by the way, extract R code from Rmd: http://felixfan.github.io/extract-r-code/
  files <- list.files(path)[str_detect(list.files(path), ".R$")]

  ## extract all functions and which package they are from
  ## using NCmisc::list.functions.in.file
  funs <- unlist(lapply(paste0(path, "/", files), NCmisc::list.functions.in.file))
  packs <- funs %>% names()

  ## "character" functions such as reactive objects in Shiny
  characters <- packs[str_detect(packs, "^character")]

  ## user defined functions in the global environment
  globals <- packs[str_detect(packs, "^.GlobalEnv")]

  ## functions that are in multiple packages' namespaces
  multipackages <- packs[str_detect(packs, ", ")]

  ## get just the unique package names from multipackages
  mpackages <- multipackages %>%
    str_extract_all(., "[a-zA-Z0-9]+") %>%
    unlist() %>%
    unique()
  mpackages <- mpackages[!mpackages %in% c("c", "package")]

  ## functions that are from single packages
  packages <- packs[str_detect(packs, "package:") & !packs %in% multipackages] %>%
    str_replace(., "[0-9]+$", "") %>%
    str_replace(., "package:", "")

  ## unique packages
  packages_u <- packages %>%
    unique() %>%
    union(., mpackages)

  return(list(packs = packages_u, tb = table(packages)))
}


#' Round format
#'
#' @param x a numeric
#' @param digits to be rounded to
#' @param ... handed over to base fun format
#'
#' @return quoted "pretty print" character vector
#' @export
#'
#' @examples
rndformat <- function(x, digits = 2, ...){ format(round(x, digits), nsmall = digits, ...) }




#' kable_styling wrapper to ensure all tables are consistently styled
#' e.g. summaryM object from Hmisc
#' @param obj
#' @param stripes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
mykablestyle <- function(obj, stripes = FALSE, ...) {
  boptions <- c("hover", "responsive", "condensed", "bordered")
  if (stripes) {boptions <- c(boptions, "striped")}
  kable_styling(obj,
                bootstrap_options = boptions,
                full_width = FALSE,
                ...) %>%
    row_spec(0, bold = TRUE, background = palette_colors[["lgray"]])
}


#' Format p-values per NEJM style
#' uses rndformat() from this pckg <0.0001, <0.001
#' @param p
#'
#' @return pretty print character vector, rounded p
#' @export
#'
#' @examples
formatp_nejm <- function(p){
  ifelse(p < 0.0001, '<0.0001',
         ifelse(p < 0.001, '<0.001',
                ifelse(p < 0.01, rndformat(p, digits = 3),
                       rndformat(p, digits = 2))))
}


#' @title Adaptive rounding for tables
#' This is an unvectorized form that works and gives single value (as opposed to the original function).
#' @name adapt_round
#' @param x a numeric constant
#' @return an adaptively rounded constant.
#' @examples
#' adapt_round(0.3454)
#' @export
adapt_round <- function(x) {
  if (vctrs::vec_is_empty(x))
    return("NA")

  if (all(is.na(x)))
    return(NA)

  if (!is.numeric(x))
    stop("x should be numeric", call. = FALSE)

  x_abs <- abs(x)


    if (x_abs < 1) {
      dig = 3
    } else if (x_abs < 10) {
      dig = 1
    }
    else if (x_abs < 100) {
      dig = 1
    } else {
      dig = 0
    }

    round(x, dig)
}


#' Format p-values conventionally
#' uses rndformat() from this pckg <0.001, <0.01.
#' @param p
#'
#' @return pretty print character vector, rounded p
#' @export
#'
#' @examples
formatp <- function(p){
  ifelse(p < 0.001, '<0.001',
                ifelse(p < 0.01, rndformat(p, digits = 3),
                       rndformat(p, digits = 2)))
  }


#' RMS result table
#' by Frank Harrell
#'
#' @param rmsObj
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rms_results_kable <- function(rmsObj, ...){
  ## Label for effect size column
  output_type <- case_when(
    inherits(rmsObj, "lrm") ~ "Odds Ratio",
    inherits(rmsObj, "orm") ~ "Odds Ratio",
    inherits(rmsObj, "cph") ~ "Hazard Ratio",
    inherits(rmsObj, "ols") ~ "Difference",
    TRUE                    ~ "Effect"
  )

  rms_model_results(rmsObj, ...) %>%
    ## Remove rows that give number of observations at every outcome level;
    ##  we already describe the distribution of ordinal outcomes
    filter(is.na(as.numeric(label))) %>%
    kable(
      format = "html",
      align = c("l", rep("r", 6)),
      col.names = c(
        "Variable", "Reference", "Comparison",
        sprintf("%s (95%% CI)", output_type),
        "X^2^", "df", "P"
      )
    ) %>%
    mykablestyle()
}

#' ggplotl
#' Wrapper around ggplot() to use labels as axis labels
#' @param ... ggplot parameters as usual
#'
#' @return ggplot with attr-label as axis labels
#' @export
ggplotl = function(...) {
  plot = ggplot(...)
  dat = plot$data
  for (m in names(plot$mapping)) {
    char = paste0(plot$mapping[m])
    ml = attr(dat[, char], "label")
    plot$labels[m] = ml
  }
  plot
}


#' Using
#' Loads all the packages, then goes back and installs all the missing packages.
#' Does not make use of installed.packages as suggested in R documentation (slow!).
#' I run it before I install pacman.
#' Code from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
#' @param ... package (CRAN) in quotes
#'
#' @return attach or install & attach library
#' @export
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs, require, character.only = TRUE))
  need <- libs[req == FALSE]
  n <- length(need)
  if (n > 0) {
    libsmsg <-
      if (n > 2)
        paste(paste(need[1:(n - 1)], collapse = ", "), ",", sep = "")
    else
      need[1]
    print(libsmsg)
    if (n > 1) {
      libsmsg <- paste(libsmsg, " and ", need[n], sep = "")
    }
    libsmsg <-
      paste(
        "The following packages could not be found: ",
        libsmsg,
        "\n\r\n\rInstall missing packages?",
        collapse = ""
      )
    if (winDialog(type = c("yesno"), libsmsg) == "YES") {
      install.packages(need)
      lapply(need, require, character.only = TRUE)
    }
  }
}


#' Standard error of the mean
#'
#' @param x
#' @param sd true population sd if known, else sample sd autom. calculated
#' @param na.rm default is TRUE!
#'
#' @return the standard error (sd/root n), not the Student's approx.!
#' @export
se <- function(x, sd = NULL, na.rm = TRUE)
{
  if (na.rm)
    x <- na.omit(x)
  if (is.null(sd))
    s <- sd(x)
  s/sqrt(length(x))
}

# simplifying summary functions ---------------------------------------------------

#' @title Combine aggregate functions and s
#' @name wrapper - s and summary funs
#' @aliases sum_
#' @aliases mean_
#' @aliases max_
#' @aliases min_
#' @aliases sd_
#' @aliases var_
#' @aliases first_
#' @aliases last_
#'
#' @description
#' \code{[summary function_*]} functions are simple wrappers of aggregate function
#' and the \code{s} function. \code{s} removes all non-values,
#' i.e. \code{NA,Inf,NaN}  from a vector.
#' However, if the length is 0 it returns NA. The result is then passed to the
#' corresponding aggregation function. For example,
#' \code{min_(x)} is identical to \code{min(s(x))}. Please read \code{vignette("s")} for more information.
#'
#' @param .x a single vector
#' @param ignore_na if false missing values are not omitted.
#'
#' @details 'first_non_na' is a faster version of 'first' since it only search for a non NA value until it finds one.
#' 'squeeze' on the other hand checks if all elements are equal and then returns only that value.
#'
#' @return a single aggregated value
#'
#' @seealso \code{vignette("convert")}, \code{vignette("hablar")}
#'
#' @examples
#' ## sum_ on non-rational numeric vector
#' vector <- c(7, NaN, -Inf, 4)
#' sum_(vector)
#'
#' ## Min of vector with length 0
#' vector <- c()
#' # With a wrapped s
#' min_(vector)
#'
#' ## Max of vector with only NA
#' # With a wrapped s
#' max_(vector)
#'
#' ## Use of s when NA should not be removed
#' vector <- c(7, Inf, NA, 4)
#' # With a wrapped s
#' sum_(vector, ignore_na = FALSE)
#'
#' @rdname aggregators
#' @export

max_ <- function(.x, ignore_na = TRUE) {
  max(s(.x, ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
min_ <- function(.x, ignore_na = TRUE) {
  min(s(.x, ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
sum_ <- function(.x, ignore_na = TRUE) {
  sum(s(.x, ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
mean_ <- function(.x, ignore_na = TRUE) {
  mean(s(.x, ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
median_ <- function(.x, ignore_na = TRUE) {
  stats::median(s(.x, ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
sd_ <- function(.x, ignore_na = TRUE) {
  stats::sd(s(.x, ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
var_ <- function(.x, ignore_na = TRUE) {
  stats::var(s(.x, ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
first_ <- function(.x, ignore_na = TRUE) {
  dplyr::first(s(.x, ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
last_ <- function(.x, ignore_na = TRUE) {
  dplyr::last(s(.x, ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
first_non_na <- function(.x) {
  .x <- rationalize(.x)
  .x[base::Position(function(..x)!is.na(..x), .x)]
}

#' @rdname aggregators
#' @export
squeeze <- function(.x, ignore_na = FALSE) {
  .uniques <- unique(rationalize(.x))
  if(ignore_na == FALSE & length(.uniques) > 1) {
    stop("More than one unique value")
  }
  if(ignore_na == FALSE & length(na.omit(.uniques)) == 0) {
    stop("No non missing values, to ignore missing use 'squeeze_'")
  }
  if(ignore_na == TRUE & length(na.omit(.uniques)) > 1) {
    stop("More than one unique non missing value")
  }
  if(length(na.omit(.uniques)) == 0) {
    return(.uniques[1])
  }
  .uniques[!is.na(.uniques)]
}

#' @rdname aggregators
#' @export
squeeze_ <- function(.x, ignore_na = TRUE) {
  squeeze(.x, ignore_na = ignore_na)
}

# from package davidsjoberg/hablar
# simplifying math functions ---------------------------------------------------
#' @title Ignore NA in math
#' @name math ignore NA in math funs
#' @aliases %minus_%
#' @aliases %plus_%
#'
#' @description
#' Simplifying math functions are simple wrappers of math function (- +).
#' If any of the left-hand side or right-hand side is NA, Inf or NaN it
#' returns any rational value, if there is any.
#'
#' However, if the both values are irrational it returns NA.
#' The result is then passed to the
#' corresponding math function.
#'
#' @param .x numeric or integer element
#' @param .y numeric or integer element
#'
#' @return a single value
#'
#' @seealso \code{vignette("s")}, \code{vignette("hablar")}
#'
#' @examples
#' \dontrun{# The simplest case
#' 3 %minus_% 2
#'
#' # But with NA it returns 3 as if the NA were zero
#' 3 %minus_% NA
#'
#' # It doesnt matter if the irrational number is on left- or right-hand.
#' NA %plus_% 5
#' }
#'
#' @rdname math
#' @export
`%minus_%` <- function(.x, .y) {
  if(!all(c(class(.x), class(.y)) %in% c("integer",
                                         "numeric"))){
    stop("Input must be of type 'numeric' or 'integer'")
  }
  if(length(.x) != length(.y) & (length(.x) != 1 & length(.y) != 1)) {
    stop("LHS need to have the same length as RHS or length 1")
  }

  ifelse(is.na(.x), 0, .x) - ifelse(is.na(.y), 0, .y)
}

#' @rdname math
#' @export
`%plus_%` <- function(.x, .y) {
  if(!all(c(class(.x), class(.y)) %in% c("integer",
                                         "numeric"))){
    stop("Input must be of type 'numeric' or 'integer'")
  }
  if(length(.x) != length(.y) & (length(.x) != 1 & length(.y) != 1)) {
    stop("LHS need to have the same length as RHS or length 1")
  }

  ifelse(is.na(.x), 0, .x) + ifelse(is.na(.y), 0, .y)
}
