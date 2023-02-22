#' Tidy EBSCOhost xml exports
#'
#' This function takes a link to an XML file exported from the EBSCOhost
#' literature database and transforms the information into tidy format with one row per document.
#'
#' @param link A character string specifying the link to the XML file
#'
#' @return A data frame with tidy format and one row per document.
#'
#' @importFrom xml2 read_xml xml_find_all as_list
#' @importFrom XML xmlParse xmlAttrs getNodeSet xmlToDataFrame
#' @importFrom utils download.file unzip
#' @importFrom purrr map_depth simplify enframe
#' @importFrom tibble enframe unnest_wider
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr left_join mutate_if select replace bind_rows
#'
#' @examples
#' tidy_ebsco_xml("https://www.example.com/ebsco.xml")
#'
#' @export
tidy_ebsco_xml <- function(link){

  # Download xml file from link
  temp <- tempfile()
  utils::download.file(link, temp)
  xml_data <- xml2::read_xml(utils::unzip(temp))
  unlink(temp)

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
    dplyr::mutate_if(is.list, list_to_string) %>%
    replace(.=='', NA) %>%
    dplyr::mutate(aut_1 = .[[2]],
           aut_2 = .[[3]],
           aut_3 = .[[4]],
           aut_4 = .[[5]],
           aut_5 = .[[6]],
           aut_6 = .[[7]],
           aut_7 = .[[8]],
           aut_1 = ifelse(is.na(aut_1) & !is.na(aut_2), aut_2, aut_1),
           aut_1 = ifelse(is.na(aut_1) & !is.na(aut_3), aut_3, aut_1),
           aut_1 = ifelse(is.na(aut_1) & !is.na(aut_4), aut_4, aut_1),
           aut_1 = ifelse(is.na(aut_1) & !is.na(aut_5), aut_5, aut_1),
           aut_1 = ifelse(is.na(aut_1) & !is.na(aut_6), aut_6, aut_1),
           aut_1 = ifelse(is.na(aut_1) & !is.na(aut_7), aut_7, aut_1)
    ) %>%
    dplyr::select(resultID = name, aut_1, aut_2, aut_3)

  ## Affiliation df

  affil_df <- purrr::map_depth(1, .x = aut_aff_df$value, .f = filter_list, "affil") %>%
    tibble::enframe() %>%
    tidyr::unnest_wider(value, names_repair = 'universal')  %>%
    dplyr::mutate_if(is.list, list_to_string) %>%
    replace(.=='', NA) %>%
    dplyr::mutate(aff_1 = .[[2]],
           aff_2 = .[[3]],
           aff_3 = .[[4]],
           aff_4 = .[[5]],
           aff_1 = ifelse(is.na(aff_1) & !is.na(aff_2), aff_2, aff_1),
           aff_1 = ifelse(is.na(aff_1) & !is.na(aff_3), aff_3, aff_1),
           aff_1 = ifelse(is.na(aff_1) & !is.na(aff_4), aff_4, aff_1)
    ) %>%
    dplyr::select(resultID = name, aff_1, aff_2, aff_3)

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
