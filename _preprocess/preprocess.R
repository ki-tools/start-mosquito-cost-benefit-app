# this file preprocesses the gpkg files into a single list of data frames
# which is loaded by the shiny app
# the names of the list are the country IDs (e.g. BF, BRA, COL, etc.)
library(dplyr)
library(sf)
library(rmapshaper)

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
