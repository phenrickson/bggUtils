#' clean_text: standardize text variables for use in modeling
#'
#' @description
#'
#' takes bgg categorical information and applies standardized preprocessing for creating features for modeling
#'
#' @param text text to be cleaned
#'
#' @import stringr
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' clean_text("Fantasy Flight Games")
clean_text= function(text) {

        text %>%
                str_to_lower %>%
                str_replace(., "-", " ") %>%
                str_replace(., ":", " ") %>%
                str_replace(., "/", " ") %>%
                str_remove_all(., "[:punct:]") %>%
                str_remove_all(., "[^[:alnum:] ]") %>%
                str_remove_all(., "\\(") %>%
                str_remove_all(., "\\)") %>%
                str_replace_all(., "\\s+", "_") %>%
                str_squish
}
