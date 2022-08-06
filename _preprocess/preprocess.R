# this file preprocesses the gpkg files into a single list of data frames
# which is loaded by the shiny app
# the names of the list are the country IDs (e.g. BF, BRA, COL, etc.)
library(dplyr)
library(sf)
library(rmapshaper)

process <- function(ccode, pop) {
  message(ccode, pop)
  shp_path <- file.path("_preprocess", ccode,
    paste0("Final", ccode, pop , ".shp"))
  csv_path <- file.path("_preprocess", ccode,
    paste0(ccode, pop , ".csv"))
  shp <- sf::read_sf(shp_path)
  csv <- readr::read_csv(csv_path)
  names(csv) <- tolower(gsub(".*\\.(.*)", "\\1", names(csv)))
  idx <- which(names(csv) == "name_1")
  names(csv)[idx] <- "adm1_name"
  idx <- which(names(csv) == "name_2")
  names(csv)[idx] <- "adm2_name"
  idx <- which(names(csv) == "name_3")
  names(csv)[idx] <- "adm3_name"
  csv <- csv %>%
    rename(
      incidence = "mean",
      x_pdmean = "sum",
      area_km2 = "area_sqkm",
      km_target = "areat_sqkm",
      adm0_name = "country"
    )
  dat <- shp %>%
    select(TARGETAREA, geometry) %>%
    left_join(csv, by = c(TARGETAREA = "gid_2")) %>%
    rename(shape = "geometry") %>%
    select(-starts_with("gid"), -xcoord, -ycoord, -TARGETAREA)

  dat$x_pdmean <- as.numeric(dat$x_pdmean)
  dat$incidence <- as.numeric(dat$incidence)

  dat
}

res <- list(
  VNM = list(
    "0" = process("VNM", "0"),
    "250" = process("VNM", "250"),
    "500" = process("VNM", "500"),
    "750" = process("VNM", "750"),
    "1000" = process("VNM", "1000")
  )
)

saveRDS(res, file = "data.rds")


# old names:
# x_pdmean: population in target area
# km_target: total target area of smallest administrative district
# area_km2: area of smallest administrative district
# area_km: (same as area_km2)
# adm0_name, adm1_name, adm2_name

# new names:
# area_sqkm: area of smallest administrative area
# areat_sqkm: target area within administrative area
# sum: number of people in target area
# mean: dengue incidence in target area
# gid_0, gid_1, gid_2, country, name_1, name_2



# target area within administrative area


ff <- list.files("_preprocess", pattern = ".gpkg", full.names = TRUE)
nms <- unlist(lapply(strsplit(basename(ff), "_"), "[[", 1))

lookup <- c(
  "x_pdmean" = "sum",
  "km_target" = "areat_km",
  "area_km2" = "area_km",
  "adm0_name" = "name_0",
  "adm1_name" = "name_1",
  "adm2_name" = "name_2",
  "adm3_name" = "name_3",
  "shape" = "geom"
)

dataset <- lapply(seq_along(ff), function(ii) {
  message(nms[ii])
  d <- st_read(ff[ii], stringsAsFactors = TRUE, as_tibble = FALSE)
  d <- ms_simplify(d, keep = 0.001, keep_shapes = FALSE)

  if (nms[ii] == "BF") {
    d <- d %>%
      rename_all(tolower) %>%
      rename(any_of(lookup)) %>%
      select(any_of(unique(names(lookup))))
  } else {
    # some datasets have "area", others have "area_km"
    newlookup <- lookup
    if ("area" %in% names(d) && !"area_km" %in% names(d))
      newlookup["area_km2"] <- "area"
    d <- d %>%
      rename_all(tolower) %>%
      rename(any_of(newlookup)) %>%
      filter(area_km2 > 0) %>%
      select(any_of(unique(names(newlookup))))
  }
  d$area_km <- as.numeric(d$area_km)
  d$km_target <- as.numeric(d$km_target)
  d
})
names(dataset) <- nms

# make sure appropriate names match across
sort(table(unlist(lapply(dataset, names))))

# data.rds is what will be read by the shiny app
saveRDS(dataset, file = "data.rds")

# area_km2: "Total Area (KM2)"
# km_target: "Target Area (KM2)"
# prev_inc_m: "Prevalence"
# x_pdmean: "Population in Target Area"
# tot_ann_cost_km: "Total Annual Cost Per KM"
# tot_ann_cost_target: "Total Annual Cost for Target Area"
# cost_per_pers_cov: "Cost per person covered"
# cost_per_case_avert: "Cost per case averted"
# cost_per_death_avert: "Cost per death averted"
# cost_per_daly_avert: "Cost per DALY averted"
# daly_target_area: "Malaria DALYs in Target Area"
# case_target_area: "Malaria Cases in Target Area"
# death_target_area: "Malaria Deaths in Target Area"
# econ_loss_death: "Economic losses from malaria deaths"
# amb_cases: "Ambulatory Cases"
# hosp_cases: "Hospitalized Cases"
# amb_cost_avert: "Ambulatory costs averted"
# hosp_cost_avert: "Hospital costs averted"
# tot_health_sys_cost_avert: Total health system costs averted


tmp <- lapply(ff, function(f) {
  st_read(f, stringsAsFactors = TRUE, as_tibble = FALSE)
})

cat(paste(names(tmp[[1]]), collapse = "\n"))
cat(paste(names(tmp[[7]]), collapse = "\n"))

dnms <- lapply(tmp, names)
names(dnms) <- nms

# NOTE: plots not showing for BF
# NOTE: Sri Lanka used to have admin3 but now it doesn't seem to -- do we need to update country meta?

a <- readRDS("data_old.rds")
b <- readRDS("data.rds")

lapply(names(a), function(nm) {
  message(nm)
  message("Previous data:")
  message(paste(round(range(a[[nm]]$area_km2)), collapse = " - "))
  message("Current data:")
  message(paste(round(range(b[[nm]]$area_km2)), collapse = " - "))
  message()
}) -> nul

lapply(names(a), function(nm) {
  message(nm)
  message("Previous data:")
  message(sum(a[[nm]]$area_km2))
  message("Current data:")
  message(sum(b[[nm]]$area_km2))
  message()
}) -> nul

lapply(names(a), function(nm) {
  message(nm)
  message("Previous data:")
  message(paste(round(range(a[[nm]]$km_target)), collapse = " - "))
  message("Current data:")
  message(paste(round(range(b[[nm]]$km_target)), collapse = " - "))
  message()
}) -> nul

lapply(names(a), function(nm) {
  message(nm)
  message("Previous data:")
  message(sum(a[[nm]]$km_target))
  message("Current data:")
  message(sum(b[[nm]]$km_target))
  message()
}) -> nul

range(a$BRA$area_km2)
range(b$BRA$area_km2)

range(a$BRA$km_target)
range(b$BRA$km_target)

d <- st_read("_preprocess/BF_adm3_stats_final.gpkg",
  stringsAsFactors = TRUE, as_tibble = FALSE)
d <- st_read("_preprocess/BRA_adm3_stats_final.gpkg",
  stringsAsFactors = TRUE, as_tibble = FALSE)
d <- st_read("_preprocess/COL_adm3_stats_final.gpkg",
  stringsAsFactors = TRUE, as_tibble = FALSE)
d <- st_read("_preprocess/IDN_adm3_stats_final.gpkg",
  stringsAsFactors = TRUE, as_tibble = FALSE)
d <- st_read("_preprocess/LKA_adm3_stats_final.gpkg",
  stringsAsFactors = TRUE, as_tibble = FALSE)
d <- st_read("_preprocess/MEX_adm3_stats_final.gpkg",
  stringsAsFactors = TRUE, as_tibble = FALSE)
d <- st_read("_preprocess/VNM_adm3_stats_final.gpkg",
  stringsAsFactors = TRUE, as_tibble = FALSE)
lapply(d, function(x) {
  if (is.numeric(x))
    range(x)
})
