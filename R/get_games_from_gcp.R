#' load games data from gcp bucket
#'
#' @param bucket_name Name of the GCS bucket
#' @param object Name of the object to load
#' @param generation Optional generation to load
#' @import googleCloudStorageR
#' @return Data frame of games or NULL if not found
#' @export get_games_from_gcp
get_games_from_gcp <- function(
  bucket_name = "bgg_data",
  object = "raw/objects/games",
  generation = NULL
) {
  # Set bucket
  googleCloudStorageR::gcs_global_bucket(bucket = bucket_name)

  # Check if the object exists
  objects <- tryCatch(
    {
      googleCloudStorageR::gcs_list_objects(
        prefix = object
      )
    },
    error = function(e) {
      message("Error listing objects in bucket ", bucket_name, ": ", e$message)
      return(NULL)
    }
  )

  if (is.null(objects) || nrow(objects) == 0) {
    message(
      "No games data found in bucket ",
      bucket_name,
      " with prefix ",
      prefix
    )
    return(NULL)
  }

  # Get the object
  if (!is.null(generation)) {
    # Get specific generation
    data <- tryCatch(
      {
        googleCloudStorageR::gcs_get_object(
          object_name = object,
          generation = generation
        )
      },
      error = function(e) {
        message(
          "Error getting object with generation ",
          generation,
          ": ",
          e$message
        )
        return(NULL)
      }
    )
  } else {
    # Get latest
    data <- tryCatch(
      {
        googleCloudStorageR::gcs_get_object(
          object_name = object
        )
      },
      error = function(e) {
        message("Error getting latest object: ", e$message)
        return(NULL)
      }
    )
  }

  if (is.null(data)) {
    message("Failed to get games data from bucket ", bucket_name)
    return(NULL)
  }

  # Deserialize the data
  tryCatch(
    {
      games <- try_deserialize(data)
      return(games)
    },
    error = function(e) {
      message("Error deserializing data: ", e$message)
      return(NULL)
    }
  )
}
