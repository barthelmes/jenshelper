#' ESC colors
#' @export
col_jb <- c(
  "escred"        = "#d71440",
  "escgreen"      = "#008393",
  "hfayellow"     = "#F0BC00",
  "bg"            = "#f0efe3",
  "escred_a"      = "#0083934D",
  "escgreen_a"    = "#F0BC004D",
  "hfayellow_a"   = "#D714404D",
  "bg"            = "#f0efe3DA",
  "jaccblue"      = "#1D3056",
  "uszblue"       = "#0558A2"
)

#' Amyloidosis paper 4-5-color scheme
#' @export
amy_pal <- c("#415A77", "#B24745", "#ED9B40", "#628395", "#F27059", "#EFA8B8")


#' Queen's tri-color palette
#' @export
tricolour_pal <- c("#9d1939", "#eebd31", "#11335d")

#' Semiotic
#' @export
semiotic_pal <- c("#ffd700",
                  "#ffb14e",
                  "#fa8775",
                  "#ea5f94",
                  "#cd34b5",
                  "#9d02d7",
                  "#0000ff")

#' Red as highlight
#' @export
redfocus <- c("#CB181D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

#' Green as highlight
#' @export
greenfocus <- c("#41AB5D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

#' Blue as highlight
#' @export
bluefocus <- c("#0033FF", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")



#' @title Nord and northern-themed color palettes
#'
#' Nord: An arctic, north-bluish color palette.
#' Created for the clean- and minimal flat design pattern to achieve a optimal focus and readability for code syntax highlighting and UI. It consists of four palettes utilizing a total of sixteen, carefully selected, dimmed pastel colors for a eye-comfortable, but yet colorful ambiance.
#'
#' The available palettes are:
#' polarnight
#' snowstorm
#' frost
#' aurora
#'
#' There are also 11 Colour palettes extracted from the works of the [Group of Seven](https://en.wikipedia.org/wiki/Group_of_Seven_(artists)) and one color palette drawn from [Lumina Borealis](https://www.luminaborealis.com/)
#' and 11 palettes for colors by Paul Tol
#' @export
retinal_palettes <- list(#Nord Palette
  esc2 = c("#008393", "#D71440"),
  esc3 = c("#008393", "#F0BC00", "#D71440"),
  polarnight = c("#2E3440", "#3B4252", "#434C5E", "#4C566A"),
  snowstorm = c("#D8DEE9", "#E5E9F0", "#ECEFF4"),
  frost = c("#8FBCBB", "#88C0D0", "#81A1C1", "#5E81AC"),
  aurora = c("#A3BE8C", "#EBCB8B", "#D08770", "#BF616A", "#B48EAD"),
  #Lumina Palette
  lumina = c("#EDDAEB", "#AD8CAE", "#4F93B8", "#306489", "#222B4C"),
  #G7 Palettes
  mountain_forms = c("#184860", "#486078", "#d8d8d8", "#484860", "#181830"),
  silver_mine = c("#4B644B","#647D4B","#E1E1E1","#7D96AF","#647D96"),
  lake_superior = c("#7D4B19","#C89664","#C87d4B","#4B647D","#324B64","#19324B"),
  victory_bonds = c("#AF1900","#C83200","#E19600","#193264","#001964"),
  halifax_harbor = c("#E1C8AF","#C8AF96","#AF967D","#967D7D","#644B64","#4B324b"),
  moose_pond = c("#4B3232","#7D4B32","#966432","#AF7D32","#E19632","#E1AF4B","#C8C896","#4B4B4B"),
  algoma_forest = c("#4B4B4B","#967D4B", "#AFAF7D", "#C89632", "#647D64","#96AFAF","#7D96AF"),
  rocky_mountain = c("#BEBEBE", "#C8C8C8", "#DCD2C8","#D2C8C8", "#BEBEC8", "#B4B4BE"),
  red_mountain = c("#7D3232", "#7D4B4B", "#7D6464", "#AF967D", "#FAC87D", "#E1AF64","#C8964B","#32324B"),
  baie_mouton = c("#304890", "#7890A8", "#90A8C0", "#A8A8A8", "#C0C0A8", "#6A7E4F","#304848"),
  afternoon_prarie = c("#486090", "#6078A8", "#7890A8","#90A8C0","#F0D8C0","#D6BBCF", "#A8C0C0","#C0D8D8","#A8A890"),
  # Paul Tol Palettes
  tol2 = c("#4477AA", "#CC6677"),
  tol3 = c("#4477AA", "#DDCC77", "#CC6677"),
  tol4 = c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
  tol5 = c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
  tol6 = c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499"),
  tol7 = c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
  tol8 = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"),
  tol9 = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
  tol10 = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
  tol12 = c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"),
  bluefocus = c("#0033FF", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0"),
  # Semiotic (D3)
  semiotic_pal <- c("#ffd700", "#ffb14e", "#fa8775", "#ea5f94", "#cd34b5", "#9d02d7", "#0000ff"),
  # Amyloidosis intended palette
  bad = c("#008393", "#D71440"),
  sub = c("FF9B54", "#463F3A"),
  medium = c("#008393", "#F0BC00"),
  amy2 = c("#415A77", "#B24745"),
  amy5 = c("#415A77", "#B24745", "#F27059", "#ED9B40", "#EFA8B8"),
  amy6 = c("#415A77", "#B24745", "#ED9B40", "#628395", "#F27059", "#EFA8B8"),
  # Fabio Crameri Scientific Color Maps "Batlow"
  batlow10 = c("#011959", "#103F60", "#1C5A62", "#3C6D56", "#687B3E", "#9D892B", "#D29343", "#F8A17B", "#FDB7BC", "#FACCFA"),
  queen2 = c("#11335d", "#9d1939"),
  queen3 = c("#11335d", "#eebd31", "#9d1939")
  )
