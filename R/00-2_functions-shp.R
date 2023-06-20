check_crs <- function(to_check, check_against = st_crs(25832)) {
  st_crs(to_check) == check_against
}

correct_crs <- function(shp, target_crs = st_crs(25832)) {
  if (!check_crs(shp)) {
    cli_alert_info(c("Wrong CRS. Transforming to ", target_crs$input))
    shp <- st_transform(shp, crs = target_crs)
    cli_alert_success("Transformed")
  }

  return(shp)
}

st_collapse_features <- function(shp, anlaeg) {
  shp %>%
    st_union() %>%
    as_tibble() %>%
    mutate(anlaeg = anlaeg) %>%
    relocate(anlaeg) %>%
    st_as_sf()
}





has_holes <- function(shp) {
  n_subpoly <- shp %>%
    select(geometry) %>%
    # First cast everything to multipolygon, otherwise it sometimes only uses
    # the first polygon in the multipolygon?
    st_cast("MULTIPOLYGON") %>%
    # Turn everything into single polygons
    st_cast("POLYGON") %>%
    pull(geometry) %>%
    # If the length is 2+, it means there are interior holes
    map_int(length)

  if (any(n_subpoly > 1)) return(TRUE)

  return(FALSE)
}






st_fill_holes <- function(shp, threshold) {
  shp_filled <- smoothr::fill_holes(shp, units::set_units(threshold, "m^2"))

  size_original <- object.size(shp)
  size_filled <- object.size(shp_filled)

  area_original <- sum(st_area(shp), na.rm = TRUE)
  area_filled <- sum(st_area(shp_filled), na.rm = TRUE)

  cli_alert_success(
    glue("Filled holes smaller than {threshold} [m^2]. ",
         "Saved {saved} bytes, {saved_p}% smaller. ",
         "Area {bigger} [m^2] bigger ({bigger_p}%).",
         saved = size_original - size_filled,
         saved_p = round(saved / size_original * 100),
         bigger = round(area_filled - area_original),
         bigger_p = round(bigger / area_original * 100))
  )

  return(shp_filled)
}


auto_read_shp <- function(fp,
                          layer = NULL,
                          collapse = NULL,
                          anlaeg_col = anlaeg,
                          overwrite_crs = FALSE,
                          holes_threshold_m2 = 1000) {

  # If you haven't specified a layer to read, give an error if multiple exist
  if (is.null(layer)) {
    shp_layers <- st_layers(fp)

    n_layers <- length(shp_layers$name)

    if (n_layers > 1) {
      on.exit(print(shp_layers))
      stop("Need to specify a layer, there are more than 1",
           call. = FALSE)
    }

    raw_shp <- st_read(fp, quiet = TRUE)
  } else {
    raw_shp <- st_read(fp, layer, quiet = TRUE)
  }

  shp_zm <- st_zm(raw_shp)

  # Make sure shape is valid (and remove potential Z axis (height))
  if (!all(st_is_valid(shp_zm))) {
    cli_alert_info("Shape isn't valid. Fixing.")
  }
  valid_shp <- shp_zm %>%
    st_make_valid() %>%
    st_simplify(dTolerance = 0)

  # Fix CRS if necessary
  if (overwrite_crs) {
    cli_alert_warning(c("Adding CRS 25832 to the shape forcibly. ",
                        "Previous CRS: ",
                        st_crs(valid_shp)$input))
    valid_crs_shp <- st_set_crs(valid_shp, 25832)
  } else {
    valid_crs_shp <- valid_shp %>%
      correct_crs()
  }

  if (!is.null(collapse)) {
    if (!missing(anlaeg_col)) {
      cli_alert_info(
        "Ignoring `anlaeg_col` because you also specified `collapse`."
      )
    }

    shp <- valid_crs_shp %>%
      correct_crs() %>%
      st_collapse_features(collapse)

  } else {

    anlaeg_col_str <- rlang::quo_name(enquo(anlaeg_col))
    if (!anlaeg_col_str %in% names(valid_crs_shp)) {
      glimpse(valid_crs_shp)
      stop(glue("`{anlaeg_col_str}` isn't in the names of your shapefile"),
           call. = FALSE)
    }

    shp <- valid_crs_shp %>%
      st_zm() %>%
      correct_crs() %>%
      select(anlaeg = {{anlaeg_col}}) %>%
      group_by(anlaeg) %>%
      summarise()
  }


  assertthat::assert_that(identical(names(shp),
                                    c("anlaeg", "geometry")))

  n_features <- nrow(shp)

  if (n_features > 1) {
    cli_alert_info("There are multiple features. That's often OK, but check:")
    shp %>%
      as_tibble() %>%
      count(anlaeg) %>%
      print()

    assertthat::assert_that(
      n_distinct(shp$anlaeg) == n_features,
      msg = paste("There are multiple features in this shape.",
                  "That's fine if each feature has a different name in the",
                  "`anlaeg` column (for multiple plants). But they don't.")
    )
  }

  if (all(!st_is(shp, c("POLYGON", "MULTIPOLYGON")))) {
    cli_alert_warning(c("There are shapes other than polygons in here. ",
                        "Can't check for holes."))
  } else {
    if (has_holes(shp) & !is.null(holes_threshold_m2)) {
      cli_alert_info(glue(
        "This shape has holes. ",
        "Trying to fill the small ones (under {holes_threshold_m2} [m^2])."
      ))

      shp <- st_fill_holes(shp, holes_threshold_m2)
    }
  }

  shp
}








plot_anlaeg <- function(shp) {
  shp %>%
    mutate(area = st_area(geometry)) %>%
    arrange(desc(area)) %>%
    mutate(anlaeg = fct_inorder(anlaeg)) %>%
    select(-area) %>%
    ggplot() +
    geom_sf(aes(fill = anlaeg, colour = anlaeg)) +
    scale_colour_hue(l = 55) +
    theme_void()
}

st_save_cleaned <- function(shp, name = NULL, ...) {
  name <- name %||% deparse(substitute(shp))
  if (name == ".") stop("Got to give a name for the shapefile", call. = FALSE)
  st_write(shp, paste0("Oplande/cleaned/", name, ".geojson"),
           driver = "geojson", delete_dsn = TRUE,
           ...)

  invisible(shp)
}


str_rm_renseanlaeg <- function(x) {
  str_remove(x, regex("(central)?(rens|rense)?anl(ae|æ)g",
                      ignore_case = TRUE)) %>%
    str_squish()
}











#' Busy anlaeg map with labels
#'
#' @param shp
#' @param dk_kommuner
#' @param crop_coords_dk
#'
#' @return
#' @export
#'
#' @examples
map_anlaeg_busy <- function(shp,
                            dk_kommuner,
                            crop_coords_dk) {

  shp_anlaeg_bg <- shp %>%
    st_convex_hull()

  place_buffered <- shp %>%
    group_by(company) %>%
    filter(n() > 1) %>%
    summarise() %>%
    st_convex_hull() %>%
    st_buffer(1500)

  plot_all_anlæg <- ggplot() +
    # Denmark background
    geom_sf(data = dk_kommuner, colour = "grey85", fill = "grey90") +
    # Convex hull see-through backgrounds
    geom_sf(data = shp_anlaeg_bg, aes(fill = anlaeg), colour = "transparent",
            alpha = 0.2,
            show.legend = FALSE) +
    # Buffered border line
    geom_sf(data = place_buffered, colour = "grey40", fill = "transparent") +
    # Actual shapes
    geom_sf(data = shp,
            aes(fill = anlaeg), colour = "transparent",
            show.legend = FALSE) +
    # Darker shape outlines
    geom_sf(data = shp,
            aes(colour = anlaeg),
            fill = "transparent",
            show.legend = FALSE) +
    geom_sf_label(
      data = shp_anlaeg_bg %>%
        mutate(geometry = st_centroid(geometry)),
      aes(geometry = geometry,
          fill = anlaeg,
          label = anlaeg_display),
      colour = "white",
      size = 1.1,
      label.padding = unit(0.12, "lines"),
      show.legend = FALSE,
      alpha = 0.56) +
    coord_sf(xlim = crop_coords_dk$X,
             ylim = crop_coords_dk$Y,
             crs = 25832,
             datum = 25832,
             expand = FALSE) +
    scale_colour_hue(l = 50) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#ebf9ff",
                                         colour = "transparent"))


}




#' Simple anlaeg map with only blue oplande
#'
#' @param shp_simple_inlookup
#' @param dk_kommuner
#' @param crop_coords_dk
#'
#' @return
#' @export
#'
#' @examples
map_anlaeg_nolabels <- function(shp_simple_inlookup,
                                dk_kommuner,
                                crop_coords_dk) {
  ggplot() +
    # Denmark background
    geom_sf(data = dk_kommuner, colour = "grey85", fill = "grey90") +
    # Actual shapes
    geom_sf(data = shp_simple_inlookup,
            fill = "midnightblue",
            colour = "transparent",
            show.legend = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(
      fill = "#ebf9ff",
      colour = "transparent")
    ) +
    coord_sf(xlim = crop_coords_dk$X,
             ylim = crop_coords_dk$Y,
             crs = 25832,
             datum = 25832,
             expand = FALSE)
}
