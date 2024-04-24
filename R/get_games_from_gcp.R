#' load games data from gcp bucket
#'
#' @param bucket gcp bucket
#' @param object_name path to object in gcp bucket
#' @param generation version of object in bucket
#'
#' @import qs
#' @import googleCloudStorageR
#' @return games data in nested data frame
#' @export get_games_from_gcp
get_games_from_gcp = function(
                bucket = "bgg_data",
                object_name = games_object(),
                generation = NULL,
                ...) {

        if (is.null(generation)) {

                googleCloudStorageR::gcs_get_object(object_name = object_name,
                                                    bucket = bucket,
                                                    ...) |>
                        qs::qdeserialize()
        }

        else {
                googleCloudStorageR::gcs_get_object(object_name = object_name,
                                                    generation = generation,
                                                    bucket = bucket,
                                                    ...) |>
                        qs::qdeserialize()
        }
}

games_object = function() {
        "raw/objects/games"
}
