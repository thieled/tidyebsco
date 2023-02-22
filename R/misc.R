

#' Filter list elements by name
#'
#' This function filters a named list by the elements with names matching a given character vector.
#'
#' @param l A named list to filter.
#' @param var A character vector with the names of the elements to keep.
#'
#' @return A list containing only the elements with names that match the values in the 'var' argument.
#'
#' @examples
#' my_list <- list(a = 1, b = 2, c = 3)
#' filter_list(my_list, c("a", "c"))
#'
#' @export
filter_list <- function(l, var){
  l[names(l) %in% var == TRUE]
}

#' Convert a list of values to a string
#'
#' This function takes a list of values and converts it to a string by applying the 'toString' function to each element of the list.
#'
#' @param x A list to be converted to a string.
#'
#' @return A character vector containing the values of the list as a string.
#'
#' @examples
#' list_to_string(list(1:3, "foo", TRUE))
#' #[1] "1, 2, 3" "foo" "TRUE"
#'
#' @export
list_to_string <- function(x) {
  r <- sapply(x, toString)
  return(r)
}


