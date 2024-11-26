#' present_bgg_text: clean bgg text/variables for presentation in plots, tables
#'
#' @description
#'
#' takes bgg string and applies standardized preprocessing for presentation
#'
#' @param text text to be cleaned
#' @param minlength minimum length of abbreviated ext returned for presentation
#'
#' @import stringr
#'
#' @export
#'
#' @examples
#' present_bgg_text("fantasy_flight_games")
present_bgg_text <- function(text,
                             minlength = 50) {
  suppressWarnings({
    text |>
      str_replace_all("_", " ") |>
      str_to_title() |>
      str_replace("Minage", "Min Age") |>
      str_replace("Minplayers", "Min Players") |>
      str_replace("Maxplayers", "Max Players") |>
      str_replace("Minplaytime", "Min Play Time") |>
      str_replace("Maxplaytime", "Max Play Time") |>
      str_replace("Playingtime", "Play Time") |>
      str_replace("Usa", "USA") |>
      str_replace("Averageweight", "Average Weight") |>
      str_replace("Usersrated", "Users Rated") |>
      str_replace("Rockpaperscissors", "Rock Paper Scissors") |>
      str_replace("Collectible Collectible", "Collectible") |>
      str_replace("Murdermystery", "Murder Mystery") |>
      str_replace("World War Ii", "World War II") |>
      str_replace("Gemscrystals", "Gems & Crystals") |>
      str_replace("Gmt", "GMT") |>
      str_replace("Cmon", "CMON") |>
      str_replace("Zman", "ZMan") |>
      str_replace("Movies Tv", "Movies TV") |>
      str_replace("Auctionbidding", "Auction Bidding") |>
      str_replace("Postnapoleonic", "Post Napoleonic") |>
      str_replace("Paperandpencil", "Paper And Pencil") |>
      str_replace("Digital Hybrid Appwebsite Required", "Digital Hybrid App") |>
      str_replace("Components 3 Dimensional", "Components") |>
      str_replace("3d", "3D") |>
      str_replace("Usa", "USA") |>
      str_replace("3D", "3D Components") |>
      str_replace("Wizkids I", "WizKids") |>
      str_replace("Decision Kids I", "Decision Kids") |>
      str_replace("^Families", "Fam:") |>
      str_replace("^Mechanics", "Mech:") |>
      str_replace("^Categories", "Cat:") |>
      str_replace("^Publishers", "Pub:") |>
      str_replace("^Designers", "Des:") |>
      str_replace("^Artists", "Art:") |>
      str_replace("^Fam:Components", "Components") |>
      abbreviate(minlength = minlength) |>
      as.character()
  })
}
