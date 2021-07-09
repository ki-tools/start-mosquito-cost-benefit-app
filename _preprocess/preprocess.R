# this file preprocesses the gpkg files into a single list of data frames
# which is loaded by the shiny app
# the names of the list are the country IDs (e.g. BF, BRA, COL, etc.)
library(dplyr)
library(sf)
library(rmapshaper)

ff <- list.files("_preprocess", pattern = ".gpkg", full.names = TRUE)
nms <- unlist(lapply(strsplit(basename(ff), "_"), "[[", 1))

dataset <- lapply(seq_along(ff), function(ii) {
  message(nms[ii])
  d <- st_read(ff[ii], stringsAsFactors = TRUE, as_tibble = FALSE)
  d <- ms_simplify(d, keep = 0.001, keep_shapes = FALSE)
  if (nms[ii] == "BF")
    d <- rename(d, prev_inc_m = X_Incmean)
  if (nms[ii] == "LKA")
    d <- rename(d,
      prev_inc_m = X_PrevIncme, area_km2 = Area,
      adm0_name = ADM0_EN, adm1_name = ADM1_EN,
      adm2_name = ADM2_EN, adm3_name = ADM3_EN)
  d %>%
    rename_all(tolower) %>%
    filter(area_km2 > 0) %>%
    select(-any_of(c("km_high", "km_rural", "shape_area",
      "shape_leng", "adm1_pcode", "adm2_pcode", "adm3_pcode")))
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
