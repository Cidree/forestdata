#' Species list - Chorological Maps
#'
#' A \code{tibble} with the names of the tree species that have a
#' chorological map available
#'
#' @format ## `chorological_species`
#' A tibble with 104 rows and 3 columns:
#' \describe{
#'   \item{Species}{Latin name of tree species for \code{get_chorological}}
#'   \item{DOI}{Url to the source of the species' data}
#' }
#' @source <https://forest.jrc.ec.europa.eu/en/european-atlas/atlas-data-and-metadata/>
"chorological_species"

#' Species list - EU-Trees4F Database
#'
#' A \code{character} vector with the names of the tree species that are
#' available for `forestdata::fd_forest_eutrees4f` function
#'
#' @format ## `eutrees4f_species`
#' A vector with 67 rows:
#' \describe{
#'   The latin name of tree species to use in `fd_forest_eutrees4f()`
#' }
#' @source \url{https://springernature.figshare.com/collections/EU-Trees4F_A_dataset_on_the_future_distribution_of_European_tree_species_/5525688}
"eutrees4f_species"

