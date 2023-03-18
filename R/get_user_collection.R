#' get_user_collection: pulling collection data for a boardgamegeek user
#'
#' @description
#'
#' wrapper around a function from bggAnalytics for loading collection info.
#'
#' @param username a username on boardgamegeek; case sensitive
#' @import magrittr bggAnalytics dplyr tibble
#'
#' @return data frame with user collection
#' @export
#'
#' @examples
#' user_collection <- get_user_collection("mrbananagrabber")
#'
get_user_collection <-
        function(username) {

                message(paste('searching for bgg collection for', username))

                # search for collection up to 10 times; sleep for 10 seconds between requests
                collection_obj =
                        retry(bggCollection$new(username = username),
                              maxErrors = 15,
                              sleep = 10)

                # expand
                collection_obj$expand(variable_names = c("name",
                                                         "type",
                                                         "yearpublished",
                                                         "rating",
                                                         "numplays",
                                                         "own",
                                                         "preordered",
                                                         "prevowned",
                                                         "fortrade",
                                                         "want",
                                                         "wanttoplay",
                                                         "wanttobuy",
                                                         "wishlist",
                                                         "wishlistpriority"))

                #  message(paste(nrow(collection_obj$data), 'records found in bgg collection for', username))

                # convert to dataframe
                collection_data<-
                        collection_obj$data %>%
                        transmute(username = username,
                                  url = collection_obj$api_url,
                                  load_ts = collection_obj$timestamp,
                                  game_id = objectid,
                                  name,
                                  type,
                                  rating,
                                  own,
                                  preordered,
                                  prevowned,
                                  fortrade,
                                  want,
                                  wanttoplay,
                                  wanttobuy,
                                  wishlist,
                                  wishlistpriority) %>%
                        # convert logical to dummies
                        mutate_if(is.logical, ~ case_when(. == T ~ 1,
                                                          TRUE ~ 0)) %>%
                        # order by game id and then desc own
                        arrange(game_id, desc(own))

                message('removing duplicates from collection...')

                # check for duplicated game_ids
                dupes = which(duplicated(collection_data$game_id)==T)

                if (length(dupes) > 0) {
                        collection_data_out = collection_data[-dupes,]
                } else {
                        collection_data_out = collection_data
                }

                message(paste(nrow(collection_obj$data), 'records returned for', username))

                # convert to
                collection_data_out = collection_data_out

                return(collection_data_out)

        }

isError<-
        function(x) {
                "try-error" %in% class(x)
        }

retry <-
        function(expr, maxErrors=5, sleep=0) {

        attempts = 0

        retval = try(eval(expr), silent=T)

        while (isError(retval)) {

                attempts = attempts + 1

                if (attempts >= maxErrors) {
                        msg = paste("too many attempts")
                        stop(msg)
                } else {
                        msg = paste("retry: no response in attempt", attempts, "of",  maxErrors)
                        # flog.error(msg)
                        message(msg)
                }
                if (sleep > 0) Sys.sleep(sleep)

                suppressWarnings({
                        retval = try(eval(expr), silent = T)
                })
        }
        return(retval)


}
