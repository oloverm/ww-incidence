# Eurofins' first file had weird encoding, this is a manual fix
fix_first_encoding <- function(x) {
  x %>%
    str_replace_all("¿", "ø") %>%
    str_replace_all("¯", "Ø") %>%
    str_replace_all("¾", "æ") %>%
    str_replace_all("¡", "°") %>%
    str_replace_all("Œ", "å")

}

# Eurofins put units in the first row of data??? So we have to fix it
clean_names_add_firstrow <- function(df) {

  # First row of data, empty string if it's NA
  row1 <- df %>%
    head(1) %>%
    mutate_all(as.character) %>%
    unlist() %>%
    coalesce("")

  new_colnames <- names(row1) %>%
    # Paste the first row onto the end of the column names
    paste(row1) %>%
    # Squish extra spaces
    str_squish() %>%
    # Use janitor's name cleaning
    make_clean_names(
      replace = c("SARS-CoV-2" = "sarscov2",
                  "ø" = "oe",
                  "å" = "aa",
                  "æ" = "ae",
                  "\\+" = " pos ",
                  "-" = " neg ",
                  "µ" = "micro")
    )

  df %>%
    # Remove first row that contained units
    slice(-1) %>%
    # Apply new column names
    setNames(new_colnames)
}


#' Replace values that match a predicate with NA
#'
#' Like `na_if()`, but more flexible.
#'
#' @param x Values to apply a predicate function to. Some can turn into NA
#' @param fn Predicate function. Can be one-sided formula
#'
#' @return Original values, but those that match the predicate function are
#'   replaced with NA
na_predicate <- function(x, fn) {
  predicate <- rlang::as_function(fn)

  x[predicate(x)] <- NA

  x
}





fp_latest <- function(dir, pattern) {
  fp <- list.files(dir, pattern, recursive = FALSE, full.names = TRUE) %>%
    max()

  if (is.na(fp)) {
    cli_abort(c(
      "Can't find the latest file.",
      "x" = "Looking for a file that matches the pattern {.val {pattern}}.",
      "x" = "Looking in the folder {.file {dir}}.",
      "i" = "Try opening that folder on your computer, that sometimes helps."
    ))
  }

  return(fp)
}

read_latest_ww_human <- function() {
  fp_latest(here("S:/Spildevand/Lille OEU-sag/data/ww-human"),
            "ww-human\\.xlsx") %>%
    read_xlsx()
}






#' Read WW data (potentially cached)
#'
#' @param cache_limit FALSE if you don't want to use the cache. Otherwise a
#'   lubridate period, e.g. `lubridate::hours(1)`, in which case it'll read the
#'   cached data if it's younger than that period.
#'
#' @return
#' @export
#'
#' @examples
read_ww <- function(cache_limit = hours(1)) {
  assertthat::assert_that(
    isFALSE(cache_limit) | is.period(cache_limit)
  )


  fp_cached_latest <- fp_latest(here("data/cache"), "\\.RData$")

  dttm_cached_latest <- ymd_hm(fp_cached_latest, tz = NULL)


  if (is.period(cache_limit)) {
    if (exists("dttm_ww_read") && now() - dttm_ww_read <= cache_limit) {
      cli_alert_info("Data already loaded, not old enough to reread yet")
      return(invisible())
    }

    cache_expired <- (now() - dttm_cached_latest) > cache_limit

    if (isTRUE(cache_expired)) {
      cli_alert("Cache expired")

    } else if (isFALSE(cache_expired)) {
      # Read cached data
      cli_alert_info(glue("Reading cached data: {dttm_cached_latest}"))

      load(fp_cached_latest, envir = globalenv())

      cli_alert_success("Cached data read")

      # Marker for when data was last read, to skip rereading it within limit
      assign("dttm_ww_read", now(), envir = globalenv())

      return(invisible())
    }
  }

  # Read from scratch
  cli_alert_info("Reading data (not from cache)")

  source(here("R/01_read-data.R"), encoding = "utf-8")
  source(here("R/02_clean-data.R"), encoding = "utf-8")

  # Marker for when data was last read, to skip rereading it within limit
  assign("dttm_ww_read", now(), envir = globalenv())
}





#' Colour left and right axis titles and text
#'
#' @param left A colour or `NULL` (which gives leaves the colour unchanged, so
#'   probably black)
#' @param right A colour or `NULL` (which gives leaves the colour unchanged, so
#'   probably black)
#'
#' @return A `theme()` object specifying the left and right axis title/text
#'   colours. Or `NULL`, if both `left` and `right` are `NULL`
theme_axes_leftright <- function(left = "#e5c087", right = "#ED6553") {

  if (is.null(left) & is.null(right)) return(NULL)

  theme_left <- if (!is.null(left)) {
    theme(axis.title.y.left = element_text(colour = left),
          axis.text.y.left  = element_text(colour = left))
  } else {
    theme()
  }

  theme_right <- if (!is.null(right)) {
    theme(axis.title.y.right = element_text(colour = right),
          axis.text.y.right  = element_text(colour = right))
  } else {
    theme()
  }

  return(theme_left + theme_right)

}




# Not a function but still needs to be shared
grading_colours <- tibble(breaks = c("1", "2", "3", "4", "5"),
                          values = c("dodgerblue",
                                     "orange",
                                     "orangered",
                                     "magenta3",
                                     "purple4"))


save_plot <- function(plot, filename, ..., folder = "outputs",
                      width = 9, height = 5, scale = 1, dpi = 200) {
  assert_that(is.ggplot(plot),
              is.string(filename),
              is.string(folder))

  dir.create(folder,
             showWarnings = FALSE,
             recursive = TRUE)

  fp <- glue("{folder}/", filename, ...)

  ggsave(fp, plot = plot, width = width, height = height, scale = scale,
         dpi = dpi)

  invisible(plot)
}

# save_plot1 <- function(plot, filename, ..., folder = "outputs",
#                       width = 13, height = 9, scale = 1, dpi = 200) {
#   assert_that(is.ggplot(plot),
#               is.string(filename),
#               is.string(folder))
#
#   dir.create(folder,
#              showWarnings = FALSE,
#              recursive = TRUE)
#
#   fp <- glue("{folder}/", filename, ...)
#
#   ggsave(fp, plot = plot, width = width, height = height, scale = scale,
#          dpi = dpi)
#
#   invisible(plot)
# }



save_xlsx <- function(dfs, filename, folder) {

  dir.create(folder,
             showWarnings = FALSE,
             recursive = TRUE)

  fp <- glue("{folder}/{today()}_{filename}.xlsx",
             filename = str_remove(filename, "\\.xlsx$"))

  write_xlsx(dfs, fp)

  invisible(dfs)
}



# Format log values as 10, 100 etc, with dot big mark separators
exp10_comma <- function(x, big.mark = ".", decimal.mark = ",",
                        accuracy = 1, ...) {
  scales::comma_format(accuracy = accuracy,
                       big.mark = big.mark,
                       decimal.mark = decimal.mark,
                       ...)(10^x)
}







# Calculate z-values
z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


# Turn outliers (> 1.5 IQRs away from q1 or q3) into NAs
na_outliers <- function(x) {

  quartile_1 <- quantile(x, 0.25, na.rm = TRUE)
  quartile_3 <- quantile(x, 0.75, na.rm = TRUE)

  iqr_x <- IQR(x, na.rm = TRUE)

  lower = quartile_1 - 1.5 * iqr_x
  upper = quartile_3 + 1.5 * iqr_x

  x <- na_predicate(x, ~ . < lower | . > upper)


  return(x)
}




# Clean the weird flow data we get from Eurofins
clean_flow <- function(vandmaengde_l, anlaeg) {
  df_flow <- tibble(anlaeg, vandmaengde_l) %>%
    mutate(digit = vandmaengde_l %>%
             # Anything with non-digits in it: NA
             na_predicate(~ str_detect(., "[^0-9]")) %>%
             # Anything with 4 or more 9s in a row: NA
             na_predicate(~ str_detect(., "9{3,}")) %>%
             # 999: NA
             na_if("999") %>%
             as.numeric()) %>%
    group_by(anlaeg) %>%
    mutate(
      n_flow = sum(!is.na(digit)),
      # Remove outliers based on IQR
      digit_rm_outliers = 10^na_outliers(log10(digit)),
      # Do everything on the log scale, because that's how the flow seems to be
      # distributed
      digit_log = log10(digit_rm_outliers),
      # Calculate z-scores of the flow
      z_flow = z_score(digit_log),
      # Get the lowest and highest ok values for flow, calculated as the
      # furthest observed values within 1.96 Z.
      lowest_ok = suppressWarnings(min(digit_log[z_flow >= -1.96], na.rm = TRUE)) %>% na_if(Inf),
      highest_ok = suppressWarnings(max(digit_log[z_flow <= 1.96], na.rm = TRUE)) %>% na_if(-Inf),
      # Bring it all together
      flow_clean_log = case_when(
        digit_log < 2 ~ NA_real_,  # flow should never be <100, I think
        n_flow < 10 ~ digit_log,  # if there are <10 observations, leave them be
        # Truncate extreme values at 1.96 SDs from mean
        z_flow < -1.96 ~ lowest_ok,
        z_flow > 1.96 ~ highest_ok,
        TRUE ~ digit_log
      ),
      # Go back to natural scale
      flow_clean = 10^flow_clean_log
    ) %>%
    ungroup()

  return(df_flow$flow_clean)
}




#' Clean LOD
#'
#' This gives you the numeric LOD. If it's missing, it gives you the mean LOD
#' for that anlæg.
#' TODO: mean for each anlæg is probably wrong.
#'
#' @param lod
#' @param anlaeg
#'
#' @return
#'
#' @examples
clean_lod <- function(lod, anlaeg) {
  tibble(anlaeg, lod) %>%
    mutate(lod = lod %>%
             na_if("NM") %>%
             as.numeric(),
           rn = row_number()) %>%
    group_by(anlaeg) %>%
    mutate(lod = coalesce(lod, mean(lod, na.rm = TRUE))) %>%
    ungroup() %>%
    arrange(rn) %>%
    pull(lod)
}






clean_påvist <- function(kopier_l,
                         analysis_failed,
                         kvalitativ) {

  kopier_l_numeric <- suppressWarnings(as.numeric(kopier_l))

  case_when(
    analysis_failed == TRUE   ~ NA_character_,
    kopier_l == "< LOQ"       ~ "<LOQ",
    kopier_l == "No Copies"   ~ "ikke påvist",
    kopier_l_numeric == 0     ~ "ikke påvist",
    kvalitativ == "Ej påvist" ~ "ikke påvist",
    kopier_l == "NM"          ~ NA_character_,
    kopier_l_numeric > 0      ~ "påvist",
  )
}



#' Combine multiple påvist columns
#'
#' We want one final påvist column, but there are different gene results. This
#' function looks across all of them. If any of them say "påvist", it returns
#' that. Then "<LOQ", then "ikke påvist".
#'
#' @param ... Character vectors with påvist/<LOQ/ikke påvist
#'
#' @return Character vector with påvist/<LOQ/ikke påvist
#'
#' @examples
#' all_ww %>%
#'   mutate(påvist = combine_påvist(påvist_rdrp,
#'                                  påvist_n,
#'                                  påvist_n1,
#'                                  påvist_n2,
#'                                  påvist_rdrp_2022),
#'          .keep = "used") %>%
#'   count(påvist_rdrp,
#'         påvist_n,
#'         påvist_n1,
#'         påvist_n2,
#'         påvist_rdrp_2022,
#'         påvist) %>%
#'   arrange_all()
combine_påvist <- function(...) {
  df <- tibble(...)

  bad_values <- df %>%
    pivot_longer(everything(), names_to = "column") %>%
    filter(!value %in% c("påvist", "<LOQ", "ikke påvist"),
           !is.na(value)) %>%
    count(column, value)

  if (nrow(bad_values) > 0) {
    print(bad_values)

    rlang::abort(paste0("The values in your columns can only be 'påvist', ",
                        "'<LOQ', or 'ikke påvist'. The bad values are shown ",
                        "above"))
  }


  df %>%
    mutate(påvist = if_any(everything(), ~ . == "påvist"),
           below_loq = if_any(everything(), ~ . == "<LOQ"),
           ikke_påvist = if_any(everything(), ~ . == "ikke påvist"),
           result = case_when(
             påvist ~ "påvist",
             below_loq ~ "<LOQ",
             ikke_påvist ~ "ikke påvist"
           )) %>%
    pull(result)
}





clean_rna <- function(kopier_l, påvist, loq, lod) {
  case_when(
    påvist == "<LOQ" ~ means(lod, loq, na.rm = TRUE),
    påvist == "ikke påvist" ~ lod / 2,
    påvist == "påvist" ~ suppressWarnings(as.numeric(kopier_l))
  )
}




#' Row means
#'
#' Wrapper around rowMeans, because rowMeans has annoying syntax. You give it
#' multiple vectors, and it gives you the means across the vectors.
#'
#' You can also give it a data frame, which can be helpful if you want to use
#' tidyselect.
#'
#' @param ...
#' @param na.rm
#'
#' @return
#'
#' @examples
means <- function(..., na.rm = FALSE) {
  rowMeans(tibble(...), na.rm = na.rm)
}




scale_shape_påvist <- function() {
  scale_shape_manual(
    breaks = c("påvist", "<LOQ", "ikke påvist"),
    labels = c("påvist og kvantificeret",
               "påvist (ikke kvantificerbar)",
               "ikke påvist"),
    values = c(19, 21, 4)
  )
}




comma_dk <- scales::comma_format(big.mark = ".",
                                 decimal.mark = ",")





#' Floor dates to Monday
#'
#' Explicitly writing the whole `floor_date(x, "week", week_start = 1)` call
#' every time is a bit verbose.
#'
#' @param x A date vector
#'
#' @return
floor_monday <- function(x) {
  assert_that(is.date(x))

  floor_date(x, "week", week_start = 1)
}





#' Reorder factor for nice ggplot legend fit
#'
#' For line graphs, this helps you order a factor by which you'll colour, so the
#' order of the legend matches the order of the lines, high to low.
#'
#' Specifically, it reorders a factor `fct` in order of descending `value`, at
#' the maximum observed `xvar` within each `fct` level.
#'
#' @param fct Factor/character vector to show in the legend
#' @param yvar Y-axis variable
#' @param xvar X-axis variable
#'
#' @return Factor variable with good ordering for ggplot line graph
#'
#' @examples
#' Prepare data
#' df <- txhousing %>%
#'   filter(city %in% c("Abilene", "Amarillo", "Arlington", "Bay Area")) %>%
#'   group_by(city, year) %>%
#'   summarise(sales = sum(sales, na.rm = TRUE),
#'             .groups = "drop")
#'
#' # Without reordering: legend looks bad
#' df %>%
#'   ggplot(aes(year, sales, colour = city)) +
#'   geom_line(size = 2)
#'
#' # With reordering: looks nicer
#' df %>%
#'   mutate(city = city %>% fct_reorder_legend(sales, year)) %>%
#'   ggplot(aes(year, sales, colour = city)) +
#'   geom_line(size = 2)
fct_reorder_legend <- function(fct, yvar, xvar) {
  df <- tibble(fct, yvar, xvar)

  ordered_fct <- df %>%
    filter(!is.na(yvar),
           !is.na(xvar)) %>%
    group_by(fct) %>%
    filter(xvar == max(xvar)) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(fct, yvar) %>%
    arrange(desc(yvar)) %>%
    pull(fct) %>%
    as.character()

  fct_relevel(fct, ordered_fct)
}




#' Read Danish CSVs
#'
#' Wrapper around `readr::read_csv2()` that incorporates the Danish locale
#'
#' @param file File path
#' @param ... Other arguments that get passed to `readr::read_csv2()`.
read_csv_dk <- function(file, ...) {
  read_csv2(
    file,
    locale = locale(encoding = "windows-1252",
                    decimal_mark = ",",
                    grouping_mark = "."),
    ...)
}
