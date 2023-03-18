#' get_bgg_games: pulling data for games on BoardGameGeek
#'
#' @description This function submits game ids to BGG's API and returns data associated with a game.
#'
#' @param game_ids vector of game_ids
#' @param batch_size number of game_ids to submit to BGG in one batch; defaults to maximum value of 500
#' @param tidy return tidied dataframe or raw xml; defaults to T
#' @param toJSON convert results to json; defaults to F
#'
#' @import dplyr tibble magrittr foreach jsonlite RcppSimdJson XML rvest httr2
#' @importFrom data.table rbindlist
#' @importFrom purrr map pluck
#' @importFrom furrr future_map
#' @importFrom foreach foreach
#' @importFrom tidyr nest unnest
#' @importFrom zoo na.locf
#' @importFrom xml2 xml_text read_xml
#' @export
#'
#' @examples
#' get_bgg_games(c(12, 7))
get_bgg_games = function(game_ids,
                         batch_size = 500,
                         tidy = T,
                         toJSON = F) {

        # message number of games submitted
        message(paste(length(game_ids), "game(s) to submit to bgg api"))

        # if length of request is less than 500, then submit in one go
        if (length(game_ids) <500) {

                # push ids to api to get xml
                bgg_games_xml = get_bgg_games_xml(game_ids)

                # if tidy == F, then return input ids, problem ids, and the raw xml from bgg
                if (tidy == F) {

                        out = list("game_ids" = bgg_games_xml$game_ids,
                                   "problem_game_ids" = bgg_games_xml$missing_game_ids,
                                   "bgg_games_xml" = bgg_games_xml$bgg_games_xml)

                        # if tidy == T, then return tidied data
                } else if (tidy == T) {


                        out =   suppressMessages({
                                tidy_bgg_data_xml(bgg_games_xml,
                                                  toJSON = F)
                        })
                }


        } else {

                message("splitting game ids into batches")

                if (batch_size > 500) {
                        error("batch size must not exceed 500")
                }

                # convert ids to batches to submit to api
                id_batches = split(game_ids,
                                   ceiling(seq_along(game_ids)/batch_size))


                message(paste("requesting game ids in batches of", batch_size))
                message(paste(length(id_batches), "batch(es) to request from api"))

                id_batches = split(game_ids,
                                   ceiling(seq_along(game_ids)/batch_size))

                # submit batches to bgg api
                # loop over batches
                resp_batches =
                        foreach(b = 1:length(id_batches),
                                .errorhandling = 'pass') %do%
                        {

                                message(paste("submitting batch", b, "of", length(id_batches)))

                                # submit batch to function
                                out = get_bgg_games_xml(id_batches[[b]])

                                # slight pause to avoid taxing the API
                                #    Sys.sleep(2)

                                # print
                                #  print(paste("batch", b, "of", length(batches), "complete"))
                                cat(message(paste("batch", b, "of", length(id_batches), "complete."), sep="\n"))

                                # get to list
                                bgg_games_xml_obj = list("game_ids" = out[['game_ids']],
                                                         "returned_game_ids" = out[['returned_game_ids']],
                                                         "missing_game_ids" = out[['missing_game_ids']],
                                                         "bgg_games_xml" = out[['bgg_games_xml']])

                                # if tidy, convert each batch to a data frame
                                if(tidy == T) {
                                        suppressMessages({
                                                out = tidy_bgg_data_xml(bgg_games_xml_obj,
                                                                        toJSON = F)
                                        })

                                }

                                # return
                                out

                        }

                # then, return batches into one list (as data frame or xml or json, depending on selected options)
                if (tidy == F) {

                        game_ids_batches = map(resp_batches, ~ .x[['game_ids']]) %>% unlist()
                        # returned_game_ids_batches = map(resp_batches, ~ .x[['returned_game_ids']]) %>% unlist()
                        problem_game_ids_batches = map(resp_batches, ~ .x[['missing_game_ids']]) %>% unlist()
                        bgg_games_xml_batches = map(resp_batches, ~ .x[['bgg_games_xml']]) %>% unlist

                        out = list("game_ids" = game_ids_batches,
                                   "problem_game_ids" = problem_game_ids_batches,
                                   "bgg_games_xml" = bgg_games_xml_batches)

                } else if (tidy == T & toJSON == F) {

                        out = list("game_ids" = map(resp_batches, ~ .x[['game_ids']]) %>% unlist(),
                                   "problem_game_ids" = map(resp_batches, ~ .x[['problem_game_ids']]) %>% unlist(),
                                   "bgg_games_data" = map(resp_batches, ~ .x[['bgg_games_data']]) %>% bind_rows)

                } else if (tidy ==T & toJSON == T) {

                        out = list("game_ids" = map(resp_batches, ~ .x[['game_ids']]) %>% unlist(),
                                   "problem_game_ids" = map(resp_batches, ~ .x[['problem_game_ids']]) %>% unlist(),
                                   "bgg_games_data" = map(resp_batches, ~ .x[['bgg_games_data']] %>% bind_rows %>% toJSON))

                }

                message("all batches completed")

        }

        return(out)


}

# helper functions used in main function
quick_df <- function(l) {
        class(l) <- "data.frame"
        attr(l, "row.names") <- .set_row_names(length(l[[1]]))
        l
}

# function for getting data out of node
simple_node_parse = function(node, var) {

        # faster way
        #    out = list()
        data = map(node[var], ~ .x %>%
                           xmlToList %>%
                           as.list) %>%
                rbindlist

        # # set name if only one column
        if(ncol(data) == 1) {
                data = set_names(data, var)
        }

        # if empty then insert NA_character
        if (length(data) == 0 | nrow(data) == 0)  {
                data = tibble(NA_character_) %>%
                        set_names(., var)
                #%>%
                #       set_names(var)
        }

        # return out
        return(as_tibble(data))

}

# clean up html text
unescape_html <- function(str){
        xml_text(read_html(paste0("<x>", str, "</x>")))
}

# request games from bgg api app
req_bgg_api = function(game_ids) {

        # stop if more than 500
        if (length(game_ids) > 500) {
                stop(paste("requesting", length(game_ids), "too many games in one request may lead to issues. reduce number of games in request below", 500))
        }

        # if duplicated
        if (identical(game_ids,  unique(game_ids))==F) {
                # warning
                warning("duplicated game ids in request. submitting only unique game ids")
                # deduplicate
                game_ids = unique(game_ids)
        }

        # request to bgg api
        # submit game ids in comma delimited to api
        req = request(paste('https://www.boardgamegeek.com/xmlapi2/thing?id=', paste(game_ids, collapse=","), '&stats=1', sep=""))

        # submit request and get response
        resp =
                req %>%
                # throttle rate of request
                req_throttle(15/60) %>%
                # perform request
                req_perform()

        return(resp)

}

# parse xml from bgg api response
parse_bgg_api = function(input_resp_bgg_api) {

        # get game info from bgg api response
        # get xml from response
        resp_xml =
                input_resp_bgg_api %>%
                resp_body_xml()

        # parse
        resp_parsed =
                resp_xml %>%
                xmlInternalTreeParse(useInternalNodes = T)

        # get nodes for individual games
        resp_nodes <-
                getNodeSet(resp_parsed, "//item[@id]")

        # get attributes of xml to get game type and ids
        resp_ids =
                map(resp_nodes,
                    ~ .x %>%
                            xmlToList %>%
                            pluck(".attrs")) %>%
                bind_rows

        # create new object, game nodes, which holds ids of games
        game_nodes = resp_nodes
        names(game_nodes) = resp_ids$id

        return(game_nodes)

}

# function for getting desired data from parsed response
extract_bgg_data = function(input_parsed_bgg_api) {

        # create empty list to hold
        bgg_data = list()

        ### game id and name
        # game id and name
        attrs = xmlAttrs(input_parsed_bgg_api) %>%
                as.vector

        bgg_data[['type']] = data.frame("type" = attrs[1])
        bgg_data[['ids']] = data.frame("game_id" = as.integer(attrs[2]))

        ### simple parse
        # names
        bgg_data[['names']] =
                simple_node_parse(input_parsed_bgg_api,
                                  'name')

        # description
        bgg_data[['description']] =
                unescape_html(simple_node_parse(input_parsed_bgg_api,
                                                'description'))

        # image
        bgg_data[['image']] =
                simple_node_parse(input_parsed_bgg_api,
                                  'image')

        # thumbnail
        bgg_data[['thumbnail']] =
                simple_node_parse(input_parsed_bgg_api,
                                  'thumbnail')
        # yearpublished
        bgg_data[['yearpublished']] =
                as.integer(
                        simple_node_parse(input_parsed_bgg_api,
                                          'yearpublished')
                )


        # minplayers
        bgg_data[['minplayers']] =
                as.integer(
                        simple_node_parse(input_parsed_bgg_api,
                                          'minplayers')
                )

        # maxplayers
        bgg_data[['maxplayers']] =
                as.integer(
                        simple_node_parse(input_parsed_bgg_api,
                                          'maxplayers')
                )

        # playingtime
        bgg_data[['playingtime']] =
                as.integer(
                        simple_node_parse(input_parsed_bgg_api,
                                          'playingtime')
                )

        # minplaytime
        bgg_data[['minplaytime']] =
                as.integer(
                        simple_node_parse(input_parsed_bgg_api,
                                          'minplaytime')
                )

        # maxplaytime
        bgg_data[['maxplaytime']] =
                as.integer(
                        simple_node_parse(input_parsed_bgg_api,
                                          'maxplaytime')
                )

        # min age
        bgg_data[['minage']] =
                as.integer(
                        simple_node_parse(input_parsed_bgg_api,
                                          'minage')
                )

        ### custom parse
        # links: this is all categories, designers, publishers
        bgg_data[['links']] =
                map(input_parsed_bgg_api['link'],
                    ~ .x %>%
                            xmlToList) %>%
                bind_rows %>%
                select(one_of("type", "id", "value"))

        # statistics
        bgg_data[['statistics']] =
                input_parsed_bgg_api[['statistics']] %>%
                .[['ratings']] %>%
                xmlToList %>%
                .[c(
                        'averageweight',
                        'usersrated',
                        'average',
                        'bayesaverage',
                        'stddev',
                        'median',
                        'owned',
                        'trading',
                        'wanting',
                        'wishing',
                        'numcomments',
                        'numweights'
                )] %>%
                quick_df

        # ranks
        bgg_data[['ranks']] =
                input_parsed_bgg_api[['statistics']] %>%
                .[['ratings']] %>%
                .[['ranks']] %>%
                xmlToList %>%
                bind_rows

        # polls
        # this one is fiddly due to the xml structure
        polls_list =
                map(input_parsed_bgg_api[['poll']] %>% .['results'], ~ .x %>%
                            xmlToList %>%
                            bind_rows)

        # check the names of the columns in the resulting object
        polls_names = map(polls_list, ~ names(.x)) %>%
                unlist %>%
                unique

        # if these are the right structure, then grab the votes
        if (setequal(polls_names, c("value", "numvotes", "numplayers"))) {

                # extract votes
                bgg_data[['polls']] = map(polls_list, ~ .x %>%
                                                  # backfill missingness on numplayers
                                                  mutate(numplayers = na.locf(numplayers)) %>%
                                                  # remove any records that are NA on value
                                                  filter(!is.na(value))) %>%
                        bind_rows

        } else {

                bgg_data[['polls']] = tibble("value" = NA,
                                             "numvotes" = NA,
                                             "numplayers" = NA)
        }

        # return
        return(bgg_data)

}

# apply previous functions in one function
get_bgg_games_xml = function(game_ids) {

        message(paste("requesting data for", length(game_ids), "game(s) from bgg api..."))

        # send request
        resp_bgg_api = req_bgg_api(game_ids)

        if(resp_bgg_api$status_code == 200) {message("request status ok...")} else {stop("request status not ok")}

        # parse response
        parsed_bgg_api = parse_bgg_api(resp_bgg_api)

        message("returning xml for requested games")

        # output
        out = list("game_ids" = game_ids,
                   "returned_game_ids" = as.numeric(names(parsed_bgg_api)),
                   "missing_game_ids" = game_ids[!(game_ids %in% as.numeric(names(parsed_bgg_api)))],
                   "bgg_games_xml" = parsed_bgg_api)

        return(out)

}

# tidy
tidy_bgg_data_xml = function(input_bgg_games_xml_obj,
                             toJSON = F) {

        message("tidying data from bgg xml...")

        # pull out pieces needed
        bgg_games_xml = input_bgg_games_xml_obj$bgg_games_xml
        game_ids = input_bgg_games_xml_obj$game_ids

        # extract tidy data
        game_data = future_map(bgg_games_xml, extract_bgg_data)

        # check if any games missing
        missing_game_ids = game_ids[!(game_ids %in% as.numeric(names(game_data)))]

        if (length(missing_game_ids) != 0) {
                warning("problems with the following game ids: ", paste(missing_game_ids, collapse = " "))
        }

        # to table
        game_info= lapply(game_data, '[',
                          c('type',
                            'ids',
                            'yearpublished',
                            'minplayers',
                            'maxplayers',
                            'playingtime',
                            'minplaytime',
                            'maxplaytime',
                            'minage',
                            'description',
                            'thumbnail',
                            'image')) %>%
                bind_rows %>%
                unnest(everything()) %>%
                nest(info = c(yearpublished,
                              minplayers,
                              maxplayers,
                              playingtime,
                              minplaytime,
                              maxplaytime,
                              minage,
                              description,
                              thumbnail,
                              image))

        # game names
        game_names = lapply(game_data, '[[', 'names') %>%
                bind_rows(., .id = "game_id") %>%
                as_tibble %>%
                mutate(game_id = as.integer(game_id),
                       sortindex = as.integer(sortindex)) %>%
                nest(names = one_of(c("type", "sortindex", "value")))

        # game links
        game_links = lapply(game_data, '[[', 'links') %>%
                bind_rows(., .id = "game_id") %>%
                mutate(game_id = as.integer(game_id),
                       id = as.integer(id)) %>%
                nest(links = one_of("type", "id", "value"))

        # game statistics
        game_statistics = lapply(game_data, '[[', 'statistics') %>%
                bind_rows(., .id = "game_id") %>%
                as_tibble %>%
                mutate_at(
                        c("game_id",
                          "usersrated",
                          "median",
                          "owned",
                          "trading",
                          "wanting",
                          "wishing",
                          "numcomments",
                          "numweights"),
                        as.integer) %>%
                mutate_at(
                        c("averageweight",
                          "average",
                          "bayesaverage",
                          "stddev"),
                        as.numeric) %>%
                nest(statistics = c(averageweight,
                                    usersrated,
                                    average,
                                    bayesaverage,
                                    stddev,
                                    median,
                                    owned,
                                    trading,
                                    wanting,
                                    wishing,
                                    numcomments,
                                    numweights))

        # game ranks
        game_ranks = lapply(game_data, '[[', 'ranks') %>%
                bind_rows(., .id = "game_id") %>%
                mutate_at(c("game_id",
                            "id"),
                          as.integer) %>%
                nest(ranks = c(type,
                               id,
                               name,
                               friendlyname,
                               value,
                               bayesaverage))

        # game polls
        game_polls = lapply(game_data, '[[', 'polls') %>%
                bind_rows(., .id = "game_id") %>%
                mutate(game_id = as.integer(game_id),
                       numvotes = as.integer(numvotes)) %>%
                nest(polls = c(value,
                               numvotes,
                               numplayers))

        # join up
        tidy_bgg_data =
                game_info %>%
                left_join(.,
                          game_names,
                          by = c("game_id")) %>%
                left_join(.,
                          game_links,
                          by = c("game_id")) %>%
                left_join(.,
                          game_statistics,
                          by = c("game_id")) %>%
                left_join(.,
                          game_ranks,
                          by = c("game_id")) %>%
                left_join(.,
                          game_polls,
                          by = c("game_id"))

        # return as a tibble or as json
        if (toJSON == T) {
                message("converting to JSON...")
                return_game_data = toJSON(tidy_bgg_data)

        } else {
                return_game_data = tidy_bgg_data
        }

        # out
        out = list("game_ids" = game_ids,
                   "problem_game_ids" = missing_game_ids,
                   "bgg_games_data" = return_game_data,
                   "timestamp" = Sys.time())

        # return
        return(out)

}
