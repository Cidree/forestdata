# fdi_fix_names ----

#' (Internal) Fix non Latin-ASCII names
#'
#' A function that fixes names with strange characters, spaces, and
#' also convert to title
#'
#' @param name String to fix
#'
#' @return A character vector of same length as name
#' @keywords internal
#'
#' @importFrom stringi stri_trans_general
fdi_fix_names <- function(name) {
  name |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_title() |>
    stringr::str_trim()
}

# fdi_download ----

#'  (Internal) Downloads data to tempdir
#'  Download data to tempdir
#'
#' @param download_url Url of data to download
#' @param destfile Path of the downloaded data
#' @param timeout Time to stop downloading
#'
#' @return TRUE or FALSE
#' @keywords internal
fdi_download <- function(download_url, destfile, timeout = 100000) {

  # 1. Download file
  ## 1.1. Url and file destination
  download_url <- download_url
  destfile     <- destfile
  ## 1.2. Download to tempdir
  tryCatch(
    {
      if (!file.exists(destfile)) {
        ## Check for user's timeout
        old_timeout <- getOption("timeout")
        on.exit(options(timeout = old_timeout))
        ## Download file
        options(timeout = max(timeout, getOption("timeout")))
        download.file(
          url      = download_url,
          destfile = destfile,
          quiet    = TRUE,
          mode     = "wb"
        )
      }
      return(invisible(TRUE))
    },
    error = function(e) {
      return(invisible(FALSE))
    }
  ) |> suppressWarnings()

}

# fdi_download_unzip ----

#'  (Internal) Downloads data to tempdir
#'  Download data to tempdir
#'
#' @param download_url Url of data to download
#' @param dir_zip Path of the zipped downloaded data
#' @param dir_unzip Path of the unzipped downloaded data
#' @param timeout Time to stop downloading
#'
#' @return Unzipped file
#' @keywords internal
fdi_download_unzip <- function(download_url, dir_unzip,
                               dir_zip, timeout = 100000) {

  # 1. Download file
  ## 1.1. Url and file destination
  download_url <- download_url
  dir_unzip    <- dir_unzip
  dir_zip      <- dir_zip
  ## 1.2. Download to tempdir
  tryCatch(
    {
      if (!file.exists(dir_unzip)) {
        ## Check for user's timeout
        old_timeout <- getOption("timeout")
        on.exit(options(timeout = old_timeout))
        ## Download file
        options(timeout = max(timeout, getOption("timeout")))
        download.file(
          url      = download_url,
          destfile = dir_zip,
          quiet    = TRUE,
          mode     = "wb"
        )
        ## 1.3. Unzip the file
        try({
          unzip(zipfile = dir_zip, exdir = dir_unzip)
        }, silent = TRUE)

        if (!file.exists(dir_unzip) || length(list.files(dir_unzip)) == 0) {
          system(paste("unzip", shQuote(dir_zip), "-d", shQuote(dir_unzip)))
        }
        ## 1.4. Remove zip to release space
        file.remove(dir_zip)
      }
      return(invisible(TRUE))
    },
    error = function(e) {
      return(invisible(FALSE))
    }
  ) |> suppressWarnings()

}

# fdi_download_7zip ----

#'  (Internal) Downloads data to tempdir
#'  Download data to tempdir
#'
#' @param download_url Url of data to download
#' @param dir_zip Path of the zipped downloaded data
#' @param dir_unzip Path of the unzipped downloaded data
#' @param timeout Time to stop downloading
#'
#' @return Unzipped file
#' @keywords internal
fdi_download_7zip <- function(download_url, dir_unzip, dir_zip,
                               timeout = 10000) {

  # 1. Download file
  ## 1.1. Url and file destination
  download_url <- download_url
  dir_unzip    <- dir_unzip
  dir_zip      <- dir_zip
  ## 1.2. Download to tempdir
  tryCatch(
    {
      if (!file.exists(dir_unzip)) {
        ## Check for user's timeout
        old_timeout <- getOption("timeout")
        on.exit(options(timeout = old_timeout))
        ## Download file
        options(timeout = max(timeout, getOption("timeout")))
        download.file(
          url      = download_url,
          destfile = dir_zip,
          quiet    = TRUE,
          mode     = "wb"
        )
        ## 1.3. Unzip the file
        try({
          archive::archive_extract(dir_zip, dir_unzip)
        }, silent = TRUE)

        ## 1.4. Remove zip to release space
        file.remove(dir_zip)
      }
      return(invisible(TRUE))
    },
    error = function(e) {
      return(invisible(FALSE))
    }
  ) |> suppressWarnings()
}



# fdi_download_raster

#'  Donwload a read a raster
#'  (Internal) Helper to download and read a raster from an URL
#'
#' @return A \code{SpatRaster}
#' @keywords internal
fdi_download_raster <- function(url, start = NULL, end = NULL, timeout = 5000) {

  ## 1. File name
  if (is.null(start) & is.null(end)) {
    url_path <- stringr::str_glue("{tempdir()}/{basename(url)}")
  } else {
    url_path <- stringr::str_glue("{tempdir()}/{basename(url) |> stringr::str_sub(start, end)}")
  }


  tryCatch(
    {
      if (!file.exists(url_path)) {
        ## Check for user's timeout
        old_timeout <- getOption("timeout")
        on.exit(options(timeout = old_timeout))
        ## Download file
        options(timeout = max(timeout, getOption("timeout")))
        download.file(
          url      = url,
          destfile = url_path,
          quiet    = TRUE,
          mode     = "wb"
        )
      }
      ## 1.3. Read file
      r <- terra::rast(url_path)
      return(r)
    },
    error = function(e) {
      return(invisible(NULL))
    }
  ) |> suppressWarnings()

}



# get_combined_raster

#'  Combines different raster tiles
#'  (Internal) Helper to combine rasters from forest extent GLAD
#'
#' @return A \code{SpatRaster}
#' @keywords internal
get_combined_raster <- function(year_i, url_table, area, crop, ...) {

  ## Filter urls within the year
  filtered_url <- dplyr::filter(url_table, year == year_i) |>
    dplyr::pull(url) |>
    as.character()

  ## Download all the rasters
  rast_lst <- lapply(filtered_url, terra::rast)

  ## Crop the rasters if required
  if (crop) {
    for (i in 1:length(rast_lst)) {
      rast_lst[[i]] <- terra::crop(rast_lst[[i]], area, ...)
    }
  }

  ## Combine all the raster
  if (length(rast_lst) == 1) {
    r_combined <- rast_lst[[1]]
  } else {
    r_combined <- do.call(terra::merge, rast_lst)
  }

  return(r_combined)
}

# get_combined_raster_2l

#'  Combines different raster tiles
#'  (Internal) Helper to combine rasters from Copernicus Global Land Cover.
#'
#' @return A \code{SpatRaster}
#' @keywords internal
get_combined_raster_2l <- function(year_i, layer_i, url_table) {

  ## Filter urls within the year
  filtered_url <- dplyr::filter(url_table, year == year_i, layer_shrt == layer_i) |>
    dplyr::pull(url) |>
    as.character()

  ## Download all the rasters
  rast_lst <- lapply(filtered_url, terra::rast)

  ## Combine all the raster
  if (length(rast_lst) == 1) {
    r_combined <- rast_lst[[1]]
  } else {
    r_combined <- do.call(terra::merge, rast_lst)
  }

  return(r_combined)
}





## IFN utils --------------

# nest_ifn_shrub

#'  Nest IFN3 or IFN 4 shrub data
#'  (Internal) Helper to nest data from IFN
#'
#' @return A \code{tibble}
#' @keywords internal
nest_ifn_shrub <- function(data, codes, agents, ifn = 4) {

  if (ifn == 4) {
    data$PCMatorral |>
      merge(codes, by.x = "Especie", by.y = "species_code") |>
      merge(agents, by.x = "Agente", by.y = "agent_code") |>
      dplyr::select(
        plot = Estadillo, species_name, fcover = Fcc, h_dm = Hm, agent_name
      ) |>
      tidyr::nest(shrub_data = species_name:agent_name)
  } else if (ifn == 3) {
    data$PCMatorral |>
      merge(codes, by.x = "Especie", by.y = "species_code") |>
      dplyr::select(
        plot = Estadillo, species_name, fcover = Fcc, h_dm = Hm
      ) |>
      tidyr::nest(shrub_data = species_name:h_dm)
  }

}

# nest_ifn_tree

#'  Level 2 process of tree data
#'  (Internal) Helper to process tree data of IFN
#'
#' @importFrom stats weighted.mean
#' @return A \code{tibble}
#' @keywords internal
process_pmayores <- function(data) {

  data |>
    ## Remove NA
    dplyr::filter(
      !is.na(d_mm),
      h_m > 0
    ) |>
    ## Get diametric classes
    dplyr::mutate(
      dclass = fdi_diametric_class(d_mm / 10)
    ) |>
    ## Calculate number of trees per plot and species
    dplyr::summarise(
      # Ht  = mean(Ht, na.rm = TRUE),
      n   = n(),
      .by = c(plot, species_name, dclass, h_m)
    ) |>
    ## Calculate average height
    dplyr::summarise(
      h_mean = weighted.mean(h_m, n),
      n      = sum(n),
      .by    = c(plot, species_name, dclass)
    ) |>
    ## Calculate trees/ha
    ## Concentric plots are used
    dplyr::mutate(
      n_ha = dplyr::case_when(
        dclass < 12.5                  ~ n * 10000 / (pi * 5^2),
        dclass >= 12.5 & dclass < 22.5 ~ n * 10000 / (pi * 10^2),
        dclass >= 22.5 & dclass < 42.5 ~ n * 10000 / (pi * 15^2),
        dclass >= 42.5                 ~ n * 10000 / (pi * 25^2),
        .default = NA
      )
    ) |>
    ## Dominant height by plot and species
    dplyr::mutate(
      h0  = fdi_dominant_height(dclass, h_mean, n_ha, which = "assman"),
      .by = c(plot, species_name)
    ) |>
    ## Basal area
    dplyr::mutate(
      g_ha  = fdi_basal_area(dclass, n_ha)
      # Especie = parse_number(Especie)
    ) |>
    dplyr::select(plot:h_mean, n_plot = n, n_ha:g_ha) |>
    tidyr::nest(tree_data = species_name:g_ha)

}



# nest_ifn_tree

#'  Nest IFN3 or IFN 4 tree data
#'  (Internal) Helper to nest data from IFN
#'
#' @return A \code{tibble}
#' @keywords internal
nest_ifn_tree <- function(data, codes, agents, which = "current", ifn = 4, process_level = 1) {
  ## PROCESS CURRENT IFN
  if (which == "current") {
    data <- data$PCMayores
    data["d_mm"] <- (data$Dn1 + data$Dn2) / 2
    if (ifn == 4) {
      ## PROCESS CURRENT IFN 4
      first_level <- data |>
        merge(codes, by.x = "Especie", by.y = "species_code") |>
        merge(agents, by.x = "Agente", by.y = "agent_code") |>
        dplyr::select(
          plot = Estadillo, tree_id = nArbol, ifn4_order = OrdenIf4, ifn3_order = OrdenIf3,
          species_name, d_mm, h_m = Ht, hcrown_m = Hcopa, agent_name
        )

    } else {
      ## PROCESS CURRENT IFN 3
      first_level <- data |>
        merge(codes, by.x = "Especie", by.y = "species_code") |>
        merge(agents, by.x = "Agente", by.y = "agent_code") |>
        dplyr::select(
          plot = Estadillo, tree_id = nArbol, ifn3_order = OrdenIf3, ifn2_order = OrdenIf2,
          species_name, d_mm, h_m = Ht, agent_name
        )
    }

    ## MANAGE PROCESS LEVEL FOR CURRENT IFN 4 AND IFN 3
    if (process_level == 1) {
      return(tidyr::nest(first_level, tree_data = tree_id:agent_name))
    } else if (process_level == 2) {
      return(process_pmayores(first_level))
    }

  } else {
    ## PROCESS PREVIOUS IFN
    ## PROCESS PREVIOUS IFN 4
    if (ifn == 4) {
      first_level <- data_lst$PCMayores3 |>
        dplyr::mutate(d_mm = (Dn1 + Dn2) / 2) |>
        merge(codes, by.x = "Especie", by.y = "species_code") |>
        merge(agents, by.x = "Agente", by.y = "agent_code") |>
        dplyr::select(
          plot = Estadillo, tree_id = nArbol, ifn3_order = Ordenif3,
          species_name, d_mm, h_m = Ht, agent_name
        )

      ## MANAGE PROCESS LEVEL FOR PREVIOUS IFN 4
      if (process_level == 1) {
        return(tidyr::nest(first_level, tree_ifn3_data = tree_id:agent_name))
      } else if (process_level == 2) {
        return(process_pmayores(first_level))
      }

    } else {
      ## PROCESS PREVIOUS IFN 3
      first_level <- data_lst$PCMayores2 |>
        dplyr::mutate(d_mm = (Diametro1 + Diametro2) / 2) |>
        merge(codes, by.x = "Especie", by.y = "species_code") |>
        dplyr::select(
          plot = Estadillo, tree_id = NumOrden,
          species_name, d_mm, h_m = Altura
        )

      ## MANAGE PROCESS LEVEL FOR PREVIOUS IFN 3
      if (process_level == 1) {
        return(tidyr::nest(first_level, tree_ifn3_data = tree_id:h_m))
      } else if (process_level == 2) {
        return(process_pmayores(first_level))
      }
    }

  }

}



# nest_ifn_regeneration

#'  Nest IFN3 or IFN 4 regeneration data
#'  (Internal) Helper to nest data from IFN
#'
#' @return A \code{tibble}
#' @keywords internal
nest_ifn_regeneration <- function(data, codes, process_level = 1) {
  if (process_level == 1) {
    data$PCRegenera |>
      merge(codes, by.x = "Especie", by.y = "species_code") |>
      dplyr::mutate(
        small_density = dplyr::case_when(
          Densidad == 1 ~ "1-4 trees",
          Densidad == 2 ~ "5-15 trees",
          Densidad == 3 ~ "> 15 trees"
        ),
        small_h_dm = dplyr::case_when(
          CatDes == 1 ~ "<3",
          CatDes == 2 ~ "3-13",
          CatDes == 3 ~ ">13"
        )
      ) |>
      dplyr::select(
        plot = Estadillo, species_name, small_density, small_h_dm,
        big_density = NumPies, big_h_dm = Hm
      ) |>
      tidyr::nest(regeneration_data = species_name:big_h_dm)
  } else if (process_level == 2) {
    data$PCRegenera |>
      merge(codes, by.x = "Especie", by.y = "species_code") |>
      dplyr::mutate(
        small_density_ha = dplyr::case_when(
          Densidad == 1 ~ "127-509 trees",
          Densidad == 2 ~ "510-1909 trees",
          Densidad == 3 ~ "> 1909 trees"
        ),
        small_h_dm = dplyr::case_when(
          CatDes == 1 ~ "<3",
          CatDes == 2 ~ "3-13",
          CatDes == 3 ~ ">13"
        ),
        big_density_ha = NumPies * 10000 / (pi * 5^2)
      ) |>
      dplyr::select(
        plot = Estadillo, species_name, small_density_ha, small_h_dm,
        big_density_ha, big_h_dm = Hm
      ) |>
      tidyr::nest(regeneration_data = species_name:big_h_dm)

  }

}



# process_ifn

#'  Process a list returned by fd_iventory_spain
#'  (Internal) Helper process IFN3 or IFN4 data
#'
#' @return An \code{sf} object
#' @keywords internal
process_ifn <- function(data, process_level = 1, ifn = 4) {
  ## process individually
  data_sf          <- data$PCDatosMap_sf |>
    dplyr::select(province = Provincia, plot = Estadillo) |>
    dplyr::mutate(province = province_fix)
  shrub_tbl        <- nest_ifn_shrub(data, ifn_codes_tbl, ifn_agent_tbl, ifn = ifn)
  tree_tbl         <- nest_ifn_tree(data, ifn_codes_tbl, ifn_agent_tbl, which = "current", ifn = ifn, process_level = process_level)
  tree_ifn3_tbl    <- nest_ifn_tree(data, ifn_codes_tbl, ifn_agent_tbl,  which = "prev", ifn = ifn, process_level = process_level)
  regeneration_tbl <- nest_ifn_regeneration(data, ifn_codes_tbl, process_level = process_level)

  ## merge all together
  dplyr::left_join(data_sf, tree_tbl) |>
    dplyr::left_join(tree_ifn3_tbl) |>
    dplyr::left_join(shrub_tbl) |>
    dplyr::left_join(regeneration_tbl) |>
    dplyr::relocate(geometry, .after = 7)
}



# crop_with_feedback

#'  Crop a list of SpatRasters
#'  (Internal) Crop using cli feedback
#'
#' @return An \code{SpatRaster}
#' @keywords internal
crop_with_feedback <- function(r, xwgs84, quiet) {
  ## user feedback
  if (!quiet) cli::cli_alert_info("Cropping {length(r)} tile{?s}...")
  if (!quiet) crop_pb <- cli::cli_progress_bar(
    "Cropped tiles",
    total       = length(r),
    type        = "tasks",
    format_done = "{.alert-success Crop completed {.timestamp {cli::pb_elapsed}}}",
    clear       = FALSE
  )
  ## do crop
  for (i in 1:length(r)) {
    r[[i]] <- terra::crop(r[[i]], xwgs84)
    if (!quiet) cli::cli_progress_update(id = crop_pb)
  }
  ## close user feedback
  if (!quiet) cli::cli_process_done(id = crop_pb)
  return(r)
}

# mask_with_feedback

#'  Masks a list of SpatRasters
#'  (Internal) Masks using cli feedback
#'
#' @return An \code{SpatRaster}
#' @keywords internal
mask_with_feedback <- function(r, xwgs84, quiet) {
  ## user feedback
  if (!quiet) cli::cli_alert_info("Masking {length(r)} tile{?s}...")
  if (!quiet) mask_pb <- cli::cli_progress_bar(
    "Masked tiles",
    total       = length(r),
    type        = "tasks",
    format_done = "{.alert-success Mask completed {.timestamp {cli::pb_elapsed}}}",
    clear       = FALSE
  )
  ## do mask
  for (i in 1:length(r)) {
    r[[i]] <- terra::mask(r[[i]], xwgs84)
    if (!quiet) cli::cli_progress_update(id = mask_pb)
  }
  ## close user feedback
  cli::cli_process_done(id = mask_pb)
  return(r)
}



# fdi_diametric_class

#'  Calculates diametric class
#'
#' @return A numeric \code{vector}
#' @keywords internal
fdi_diametric_class <- function(x,
                                 dmin             = 7.5,
                                 dmax             = NULL,
                                 class_length     = 5,
                                 include_lowest   = TRUE,
                                 return_intervals = FALSE) {

  # 0. Setup and handle errors
  ## 0.1. Handle data type
  if (!is.logical(return_intervals)) stop("The argument `return_intervals` must be TRUE or FALSE")
  if (!is.logical(include_lowest)) stop("The argument `include_lowest` must be TRUE or FALSE")
  if (!is.numeric(x)) stop("`x` must be a numeric vector")
  if (!is.numeric(dmin)) stop("`dmin` must be a numeric vector")
  if (!is.numeric(dmax) && !is.null(dmax)) stop("`dmax` must be a numeric vector or NULL")
  if (!is.numeric(class_length)) stop("`class_length` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(x < 0)) warning("Any value in `x` is less than 0. Review your data.")
  if (dmin <= 0) stop("`dmin` must be greater than 0")
  if (dmax <= 0 && !is.null(dmax)) stop("`dmax` must be greater than 0")
  if (class_length <= 0) stop("`class_length` must be greater than 0")
  ## 0.3. dmax must be > than dmin
  if (dmin >= dmax && is.numeric(dmax)) stop("`dmax` has to be greater than `dmin`")

  # 1. Create intervals depending on user input
  ## - If dmax is NULL, use max diameter from data
  if (is.null(dmax)) {
    cuts_vec <- seq(dmin, max(x, na.rm = TRUE) + class_length, class_length)
  } else {
    message(
      glue::glue("There are {length(x[x > dmax])} diameter values greater than `dmax = {dmax}`. They will be included in the greatest class.")
    )
    x[x > dmax] <- dmax
    cuts_vec <- c(seq(dmin, dmax, class_length), Inf)
  }

  # 2. Create intervals either ( ] or [ )
  if (include_lowest) {
    intervals_vec <- cut(
      x              = x,
      breaks         = cuts_vec,
      right          = FALSE,
      include.lowest = TRUE,
      dig.lab        = 10
    )
  } else {
    intervals_vec <- cut(
      x       = x,
      breaks  = cuts_vec,
      dig.lab = 10
    )
  }

  # 3. Return intervals or class center?
  if (!return_intervals) {
    intervals_vec <- cuts_vec[as.numeric(intervals_vec)] + (class_length / 2)
  } else {
    ## Drop redundant levels
    intervals_vec <- droplevels(intervals_vec)
  }

  # 4. Return object
  return(intervals_vec)

}




# calc_dominant_height

#'  Calculates dominant height
#'
#' @return A numeric \code{vector}
#' @keywords internal
calc_dominant_height <- function(nmax, ntress, height) {

  # initialize n and empty list
  n <- 0
  l <- list()

  # loop to accumulate biggest trees up to 100
  for (i in 1:nmax[1]) {
    ## sum previous trees plus new trees
    n <- n + ntress[i]
    ## are we over 100 trees already?
    if (n > 100) {
      new_trees <- ntress[i] - n + 100
      ## add to list and exit loop
      l[[i]] <- c(new_trees, height[i])
      break
    } else {
      ## add to list
      l[[i]] <- c(ntress[i], height[i])
    }
  }

  # Extract the first elements from each element of the list
  first_elements <- sapply(l, function(x) x[1])

  # Calculate the weighted sum
  weighted_sum <- sum(sapply(l, function(x) x[1] * x[2]), na.rm = TRUE)

  # Calculate the weighted mean
  weighted_sum / sum(first_elements, na.rm = TRUE)

}


# fdi_dominant_height

#' Calculates the dominant height
#'
#' @return A numeric \code{vector}
#' @keywords internal
fdi_dominant_height <- function(diameter,
                                 height,
                                 ntrees = NULL,
                                 which = "assman") {

  # 0. Handle errors and setup
  ## 0.1. Errors
  if (!tolower(which) %in% c("assman", "hart")) stop("`which` must be either <assman> or <hart>.")
  if (!is.numeric(diameter)) stop("`diameter` must be a numeric vector")
  if (!is.numeric(height)) stop("`height` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0)) warning("Any value in `diameter` is less than 0. Review your data.")
  if (any(height <= 0)) warning("Any value in `height` is less than 0. Review your data.")

  # 1. Create a data frame with input variables
  if (is.null(ntrees)) {
    data <- data.frame(
      d  = diameter,
      h  = height,
      nt = 1
    )
  } else {
    data <- data.frame(
      d  = diameter,
      h  = height,
      nt = ntrees
    )
  }


  # 2. Calculate dominant height
  if (tolower(which) == "assman") {
    d0 <- data |>
      ## sort descending by diameter class
      dplyr::arrange(dplyr::desc(d)) |>
      dplyr::mutate(
        .cumtrees = cumsum(nt),
        .nmax     = which(.cumtrees >= 100)[1],
        .nmax     = if (is.na(.nmax[1])) which.max(.cumtrees) else .nmax,
        .do       = calc_dominant_height(.nmax, nt, h)
      ) |>
      dplyr::pull(.do)
  } else {
    d0 <- data |>
      ## sort descending by height
      dplyr::arrange(dplyr::desc(h)) |>
      dplyr::mutate(
        .cumtrees = cumsum(nt),
        .nmax     = which(.cumtrees >= 100)[1],
        .nmax     = if (is.na(.nmax[1])) which.max(.cumtrees) else .nmax,
        .do       = calc_dominant_height(.nmax, nt, h)
      ) |>
      dplyr::pull(.do)
  }

  # 3. If it's not vectorized, retrieve just one value
  if (is.null(ntrees)) d0[1] else d0

}







# fdi_basal_area

#' Calculates Basal Area in square meters.
#'
#' @return A numeric \code{vector}
#' @keywords internal
fdi_basal_area <- function(diameter,
                            ntrees = NULL,
                            units = "cm") {

  # 0. Handle errors and set-up
  if (is.numeric(ntrees) && length(ntrees) != length(diameter)) stop("`ntrees` must have the same length as `diameter` or be NULL")
  if (!is.numeric(diameter)) stop("`diameter` must be a numeric vector")
  ## 0.2. Invalid values
  if (any(diameter <= 0)) warning("Any value in `diameter` is less than 0. Review your data.")
  ## 0.3. If ntrees = NULL, only one tree assumed
  if (is.null(ntrees)) ntrees <- rep(1, length(diameter))

  # 1. Calculate basal area
  switch(units,
         "cm" = (pi / 4) * (diameter / 100)**2 * ntrees,
         "mm" = (pi / 4) * (diameter / 1000)**2 * ntrees,
         "m"  = (pi / 4) * diameter**2 * ntrees,
         stop("Invalid `units`. Use <cm>, <mm>, or <m>")
  )

}
