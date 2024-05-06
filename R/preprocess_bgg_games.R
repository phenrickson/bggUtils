#' preprocess_bgg_games: standardized preprocessing for bgg api requests for use in modeling
#'
#' @description
#'
#' takes output of `get_bgg_games()` and tidies output for modeling. this includes replacing 0s with NAs for numeric values,
#' filtering to include/exclude selected categorical features, and collapsing categorical features for use with tokenization
#'
#' @param data data from `get_bgg_games()` with simplify=T
#' @param publisher_allow list of allowed publisher ids in modeling
#' @param families_allow allow list of permitted bgg family variables for use in modeling
#' @param families_remove list of bgg family variables to be removed for modeling
#'
#' @import tidyr dplyr
#' @export preprocess_bgg_games
#'
#' @examples
#' api_resp = get_bgg_games(c(12, 7), simplify = TRUE)
#'
#' api_resp %>%
#' unnest(data) %>%
#' preprocess_bgg_games()
#'
preprocess_bgg_games = function(data,
                                publisher_allow = publishers_allow_list(),
                                families_allow = families_allow_list(),
                                families_remove = families_remove_list()
) {

        keep_links = c("category", "mechanic", "designer", "artist", "publisher")

        # outcomes =
        outcomes = data %>%
                unnest_outcomes()

        # get info needed from games
        info = data %>%
                unnest_info()

        # get categories
        categories =
                data %>%
                unnest_categories() %>%
                collapse_categorical(name = categories)

        # publishers
        publishers =
                data %>%
                unnest_publishers() %>%
                filter(id %in% publisher_allow) %>%
                collapse_categorical(name = publishers)

        # mechanics
        mechanics =
                data %>%
                unnest_mechanics() %>%
                collapse_categorical(name = mechanics)

        # artists
        artists =
                data %>%
                unnest_artists() %>%
                collapse_categorical(name = artists)

        # designers
        designers =
                data %>%
                unnest_designers() %>%
                collapse_categorical(name = designers)

        # now extract bgg families
        # general families
        families =
                data %>%
                unnest_families() %>%
                filter(!grepl(families_remove, value)) %>%
                filter(grepl(families_allow, value)) %>%
                collapse_categorical(name = families)

        # themes
        themes =
                data %>%
                unnest_themes()  %>%
                collapse_categorical(name = themes)

        # specific families
        components =
                data %>%
                unnest_components()  %>%
                collapse_categorical(name = components)

        # mechanisms
        mechanisms =
                data %>%
                unnest_mechanisms() %>%
                collapse_categorical(name = mechanisms)

        outcomes %>%
                left_join(.,
                          info,
                          by = join_by(game_id)) %>%
                left_join(.,
                          categories,
                          by = join_by(game_id)) %>%
                left_join(.,
                          mechanics,
                          by = join_by(game_id)) %>%
                left_join(.,
                          publishers,
                          by = join_by(game_id)) %>%
                left_join(.,
                          designers,
                          by = join_by(game_id)) %>%
                left_join(.,
                          artists,
                          by = join_by(game_id)) %>%
                left_join(.,
                          families,
                          by = join_by(game_id)) %>%
                left_join(.,
                          mechanisms,
                          by = join_by(game_id)) %>%
                left_join(.,
                          components,
                          by = join_by(game_id)) %>%
                left_join(.,
                          themes,
                          by = join_by(game_id)) %>%
                select(any_of(bgg_ids()), everything())

}

bgg_ids = function() {
        c("game_id", "name", "yearpublished")
}

bgg_outcomes = function() {
        c("average", "averageweight", "bayesaverage", "usersrated")
}

keep_links = function() {
        paste0("boardgame", c("category", "mechanic", "designer", "artist", "publisher"))
}


publishers_allow_list = function() {

        # list of ids allowed to enter model for publisher
        c(
                51 # Hasbo
                ,10 # Mayfair Games
                ,102 # Decision Games
                ,196 # Multi-Man Publishing
                ,396 # Alderac Entertainment Group aka AEG
                ,1027 # Days of Wonder
                ,21847 # Pandasaurus Games
                ,1001 # (web published)
                ,4 # (Self-Published)
                ,140 # Splotter Spellen
                ,157 # Asmodee
                ,34 # Ravensburger
                ,28 # Parker Brothers
                ,39 # Pegasus Speile
                ,37 # KOSMOS
                ,20 # Milton Bradley
                ,3 # Rio Grande Games
                ,538 # Z-Man Games
                ,52 # GMT Games
                # ,8923 # IELLO
                ,17 # Fantasy Flight Games
                ,5 # Avalon Hill
                ,3320 # (Unknown)
                ,597 # Eagle-Gryphon Games
                ,5400 # Matagot
                ,26 # Games Workshop Ltd
                ,47 # Queen Games
                ,11652 # Stronghold Games
                ,19 # Steve Jackson Games
                ,13 # Wizards of the Coast
                ,12024 # Cryptozoic Entertainment
                ,10754 # Plaid Hat Games
                ,21608 # CMON Global Limited
                ,108 # Gamewright
                ,221 # WizKids
                ,171 # (Public Domain)
                ,93 # Mattel, Inc
                ,25842 # Space Cowboys
                ,23202 # Stonemaier
                ,34188 # Plan  B
                ,30958 # Capstone Games
                ,22593 # Chip Theory Games
                ,17917 # Ares Games
                ,17543 # Greater Than Games
                ,28072 # Renegade Games
                ,34846 # Restoration Games
                ,29313 # Osprey Games
                ,21765 # Roxley
                ,7345 # Czech Games Edition
                ,29412 # Awaken Realms
                ,3929 # Compass Games
                ,26991 # Button Shy
                ,2456 # The Game Crafter
                ,12 # Cheapass Games
                ,9 # alea
                ,2164 # NorthStar Game Studio
                ,5774 # Bézier Games
                ,18617 #Red Raven Games
                ,102 #Decision Games (I)
                , 489# 3W (World Wide Wargames)
        )

}

families_remove_list = function() {

        c("^Admin:",
          "^Misc:",
          "^Promotional:",
          "^Digital Implementations:",
          "^Crowdfunding: Spieleschmiede",
          "^Crowdfunding: Verkami",
          "^Crowdfunding: Indiegogo",
          "^Contests:",
          "^Game:",
          "^Players: Expansions",
          "^Players: Games with expansions"
        ) %>%
                paste(., collapse = "|")
}

families_allow_list = function() {

        c("^Series: Monopoly-Like",
          "^Series: 18xx",
          "^Series: Cards Against Humanity-Like",
          "^Series: Exit: The Game",
          "^Players: Games with Solitaire Rules",
          "^Players: Wargames with Solitaire Rules",
          "^Players: One versus Many",
          "^Players: Solitaire Only Games",
          "^Players: Solitaire Only Wargames",
          "^Players: Two-Player Only Games",
          "^Players: Three Players Only Games",
          "^Players: Wargames with Rules Supporting Only Two Players",
          "^Players: Solitaire Only Card Games",
          "^Country:",
          "^Animals",
          "^History",
          "^Sports",
          "^Category",
          "^Cities",
          "^Traditional",
          "^Creatures",
          "^TV",
          "^Region",
          "^Card",
          "^Comic",
          "^Ancient",
          "^Brands",
          "^Versions & Editions",
          "^Food",
          "^Movies",
          "^Setting",
          "^Card Games",
          "^Collectible",
          "^Containers",
          "^Crowdfunding: Kickstarter",
          "^Crowdfunding: Gamefound",
          "^Authors",
          "^Characters",
          "^Religious",
          "^Holidays",
          "^Space",
          "^Folk",
          "^Word",
          "^Mythology",
          "^Occupation",
          "^Celebrities",
          "^Toys") %>%
                paste(., collapse = "|")

}

collapse_categorical = function(data, name) {

        data %>%
                clean_value() %>%
                group_by(game_id) %>%
                summarize({{name}} := paste(value, collapse = ", "),
                          .groups = 'drop')

}

# unnest ids
unnest_ids = function(data) {

        # unnest ids (game_id, name, yearpublished)
        unnest_bgg_ids = function(data) {

                data %>%
                        unnest_info() %>%
                        nest(info = -any_of(bgg_ids()))

        }

        # get ids with info
        ids =
                data %>%
                unnest_bgg_ids()

        ids |>
                left_join(
                        data %>%
                                select(-info),
                        by = join_by(game_id)) |>
                select(
                        any_of(bgg_ids()),
                        any_of(names(data))
                )
}

# preprocess games info
unnest_info = function(data) {

        info =
                data %>%
                select(game_id, info) %>%
                unnest(c(info), keep_empty = T) %>%
                mutate(across(
                        c(yearpublished,
                          minplayers,
                          maxplayers,
                          playingtime,
                          minplaytime,
                          maxplaytime,
                          minage),
                        ~ ifelse(. == 0, NA, .))
                )

        names =
                data %>%
                select(game_id, names) %>%
                unnest(c(names), keep_empty = T) %>%
                filter(type == 'primary') %>%
                select(game_id,
                       name = value)

        info |>
                left_join(names,
                          by = join_by(game_id)) |>
                select(
                        any_of(bgg_ids()),
                        everything()
                )
}

# extract mechanics
unnest_names = function(data,
                        name_types = c("primary")) {

        data %>%
                select(any_of(bgg_ids()), names) %>%
                unnest(c(names), keep_empty = T) %>%
                filter(type %in% name_types) %>%
                select(
                        any_of(bgg_ids()),
                        type,
                        name = value
                )
}

# unnest primary names
unnest_primary_names = function(data) {

        data %>%
                select(game_id, names) %>%
                unnest(c(names), keep_empty = T) %>%
                filter(type == 'primary') %>%
                select(game_id,
                       name = value)
}

# extract links
unnest_links = function(data,
                        category = c("category", "mechanic", "designer", "artist", "publisher")
) {

        keep = paste0("boardgame", category)

        # extract links
        data %>%
                select(
                        any_of(bgg_ids()),
                        links
                ) %>%
                unnest(c(links), keep_empty = T) %>%
                filter(type %in% keep)
}

# use clean_bgg_text to clean value
clean_value = function(data) {

        data %>%
                mutate(value = clean_bgg_text(value))
}

# remove "boardgame" from start of link type
clean_link = function(data) {

        data |>
                mutate(type = gsub("^boardgame", "", type))
}

# get statistics
unnest_statistics = function(data) {

        data %>%
                select(
                        any_of(bgg_ids()),
                        statistics
                ) %>%
                unnest(c(statistics), keep_empty = T)
}

# get outcomes
unnest_outcomes = function(data) {

        data %>%
                unnest_statistics() %>%
                select(any_of(bgg_ids()), averageweight, usersrated, average, bayesaverage, numweights) %>%
                mutate(across(
                        c(averageweight,
                          average,
                          bayesaverage),
                        ~ ifelse(. == 0, NA, .))
                )
}

# extract mechanics
unnest_mechanics = function(data) {

        data %>%
                unnest_links(category = 'mechanic') %>%
                clean_link()
}

# extract categories
unnest_categories = function(data) {

        data %>%
                unnest_links(category = 'category') %>%
                clean_link()

}

# extract
unnest_artists = function(data) {

        data %>%
                unnest_links(category = 'category')

}

# extract families
unnest_families = function(data) {

        data %>%
                unnest_links(category = 'family') %>%
                clean_link()

}

# extract families
unnest_families = function(data) {

        data %>%
                unnest_links(category = 'family') %>%
                clean_link()

}

# extract artists
unnest_artists = function(data) {

        data %>%
                unnest_links(category = 'artist') %>%
                clean_link()

}

# extract designers
unnest_designers = function(data) {

        data %>%
                unnest_links(category = 'designer') %>%
                clean_link()

}

# extract publishers
unnest_publishers = function(data) {

        data %>%
                unnest_links(category = 'publisher') %>%
                clean_link()

}

# extract implementation
unnest_implementations = function(data) {

        data %>%
                unnest_links(category = 'implementation') %>%
                clean_link()

}

# extract accessory
unnest_accessories = function(data) {

        data %>%
                unnest_links(category = 'accessory') %>%
                clean_link()

}

# extract compilation
unnest_compilations = function(data) {

        data %>%
                unnest_links(category = 'compilation') %>%
                clean_link()

}

# extract integration
unnest_integrations = function(data) {

        data %>%
                unnest_links(category = 'integration') %>%
                clean_link()

}

# extract expansion
unnest_expansions = function(data) {

        data %>%
                unnest_links(category = 'expansion') %>%
                clean_link()

}

# extract themes
unnest_themes = function(data) {

        data %>%
                unnest_families() %>%
                filter(grepl("^Theme", value))

}

# extract components
unnest_components = function(data) {

        data %>%
                unnest_families() %>%
                filter(grepl("^Component", value)) %>%
                mutate(value = gsub("^Components: ", "", value))

}

# extract mechanisms
unnest_mechanisms = function(data) {

        data %>%
                unnest_families() %>%
                filter(grepl("^Mechanism", value)) %>%
                mutate(value = gsub("^Mechanism: ", "", value))

}

# extract polls
unnest_polls = function(data) {

        data %>%
                select(
                        any_of(bgg_ids()),
                        polls
                ) %>%
                unnest(c(polls), keep_empty = T)

}

# same as polls, but im probably going to remember playercounts...
unnest_playercounts = function(data) {

        data %>%
                unnest_polls()
}
