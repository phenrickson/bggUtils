#' Try to deserialize data using either qs or qs2 package
#'
#' @param data Raw data to deserialize
#' @importFrom qs2 qs_deserialize qd_deserialize qd_read
#' @importFrom qs qdeserialize
#' @return Deserialized data
#' @keywords internal
try_deserialize <- function(data) {
    # Try qs2 first (newer format)
    tryCatch(
        {
            if (requireNamespace("qs2", quietly = TRUE)) {
                # Try qs2::qs_deserialize
                message("Trying qs2::qs_deserialize...")
                tryCatch(
                    {
                        return(qs2::qs_deserialize(data))
                    },
                    error = function(e) {
                        message("qs2::qs_deserialize failed: ", e$message)

                        # Try qs2::qd_deserialize
                        message("Trying qs2::qd_deserialize...")
                        tryCatch(
                            {
                                return(qs2::qd_deserialize(data))
                            },
                            error = function(e) {
                                message(
                                    "qs2::qd_deserialize failed: ",
                                    e$message
                                )
                                stop("Both qs2 deserialization methods failed")
                            }
                        )
                    }
                )
            }

            # If qs2 is not available, fall back to qs
            message("qs2 not available, trying qs deserialization...")
            qs::qdeserialize(data)
        },
        error = function(e) {
            message("qs2 deserialization failed: ", e$message)
            # If qs2 fails, try qs
            tryCatch(
                {
                    message("Trying qs deserialization...")
                    qs::qdeserialize(data)
                },
                error = function(e2) {
                    message("qs deserialization failed: ", e2$message)

                    # Try to save the raw data to a file and then read it
                    message("Trying to save and read data...")
                    temp_file <- tempfile(fileext = ".rds")
                    writeBin(data, temp_file)

                    tryCatch(
                        {
                            # Try to read as RDS
                            message("Trying to read as RDS...")
                            result <- readRDS(temp_file)
                            unlink(temp_file)
                            return(result)
                        },
                        error = function(e3) {
                            message("RDS read failed: ", e3$message)

                            # Try qs2::qs_read
                            message("Trying qs2::qs_read...")
                            tryCatch(
                                {
                                    result <- qs2::qs_read(temp_file)
                                    unlink(temp_file)
                                    return(result)
                                },
                                error = function(e4) {
                                    message("qs2::qs_read failed: ", e4$message)

                                    # Try qs2::qd_read
                                    message("Trying qs2::qd_read...")
                                    tryCatch(
                                        {
                                            result <- qs2::qd_read(temp_file)
                                            unlink(temp_file)
                                            return(result)
                                        },
                                        error = function(e5) {
                                            message(
                                                "qs2::qd_read failed: ",
                                                e5$message
                                            )
                                            unlink(temp_file)

                                            # As a last resort, try to read the first few bytes to diagnose
                                            message(
                                                "First 20 bytes of data: ",
                                                paste(
                                                    as.integer(data[
                                                        1:min(20, length(data))
                                                    ]),
                                                    collapse = " "
                                                )
                                            )

                                            stop(
                                                "Failed to deserialize data with all available methods"
                                            )
                                        }
                                    )
                                }
                            )
                        }
                    )
                }
            )
        }
    )
}
