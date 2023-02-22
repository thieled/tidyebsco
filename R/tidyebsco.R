#' Tidy EBSCOhost XML
#'
#' Takes an XML file exported from the literature search database EBSCOhost and extracts relevant data into a tidy data frame.
#'
#' @param file character string specifying the file path to the XML file.
#'
#' @return A tidy data frame with extracted author, affiliation, and publication information.
#'
#' @examples
#' # Load example data
#' xml_file <- system.file("extdata", "example.xml", package = "tidyebsco")
#'
#' # Extract data
#' tidy_ebsco_xml(xml_file)
#'
#' @export
tidy_ebsco_xml <- function(file){

  # Load xml file
  xml_data <- xml2::read_xml(file)

  # Parse using XML package
  XML_data <- XML::xmlParse(xml_data)

  # Get author and affiliation df (nested)
  aut_aff_df <- xml_data %>%
    xml2::xml_find_all("//aug") %>%
    xml2::as_list() %>%
    purrr::simplify() %>%
    tibble::enframe()


  # Extract author information as df
  auth_df <- purrr::map_depth(1, .x = aut_aff_df$value, .f = filter_list, "au") %>%
    tibble::enframe() %>%
    tidyr::unnest_wider(value, names_repair = 'universal')  %>%
    dplyr::mutate_if(is.list, list_to_string)  %>%
    replace(.=='', NA) %>%
    tidyr::unite(col = "authors", dplyr::starts_with("au"), sep = "; ", remove = T, na.rm = T) %>%
    replace(.=='', NA) %>%
    dplyr::rename(resultID = name)

  ## Affiliation df

  affil_df <- purrr::map_depth(1, .x = aut_aff_df$value, .f = filter_list, "affil") %>%
    tibble::enframe() %>%
    tidyr::unnest_wider(value, names_repair = 'universal')  %>%
    dplyr::mutate_if(is.list, list_to_string) %>%
    replace(.=='', NA) %>%
    tidyr::unite(col = "affiliation", dplyr::starts_with("affil"), sep = "; ", remove = T, na.rm = T) %>%
    replace(.=='', NA) %>%
    dplyr::rename(resultID = name)

  ## Get attributes and info from remaining colums
  record_df <- cbind(
    xml2::xml_attrs(xml2::xml_find_all(xml_data, "//rec")) %>% dplyr::bind_rows(),
    xml2::xml_attrs(xml2::xml_find_all(xml_data, "//dt")) %>% dplyr::bind_rows(),
    XML::xmlToDataFrame(XML::getNodeSet(XML_data, path='//jtl')),
    XML::xmlToDataFrame(XML::getNodeSet(XML_data, path='//pubinfo')),
    subset(XML::xmlToDataFrame(XML::getNodeSet(XML_data, path='//artinfo')), select = -c(aug, sug)),
    xml2::xml_attrs(xml2::xml_find_all(xml_data, "//header")) %>% dplyr::bind_rows()
  )

  # Bind
  auth_df <- dplyr::left_join(auth_df, affil_df)
  record_df <- dplyr::left_join(auth_df %>% dplyr::mutate(resultID = as.character(resultID)), record_df)

  # return
  return(record_df)

}

