source("country_meta.R")

vars <- lapply(names(country_meta), function(nm) {
  data.frame(variable = sort(names(country_meta[[nm]])), country = nm)
}) %>%
  dplyr::bind_rows()

table(vars)

#                              country
# variable                      BGD BRA COL IDN IND LKA MEX NGA VNM
#   admin2_name                   1   1   1   1   1   1   1   1   1
#   anopheline                    1   1   1   1   0   0   1   1   1
#   cost_per_amb                  1   1   1   1   1   1   1   1   1
#   cost_per_hosp                 1   1   1   1   1   1   1   1   1
#   cost_per_nonmedical           1   1   1   1   1   1   1   1   1
#   daly_per_case                 1   1   1   1   1   1   1   1   1
#   indirectcost_per_amb          1   1   1   1   1   1   1   1   1
#   indirectcost_per_hosp         1   1   1   1   1   1   1   1   1
#   indirectcost_per_nonmedical   1   1   1   1   1   1   1   1   1
#   lat                           1   1   1   1   1   1   1   1   1
#   lng                           1   1   1   1   1   1   1   1   1
#   mortality                     1   1   1   1   1   1   1   1   1
#   pct_trt_amb                   1   1   1   1   1   1   1   1   1
#   pct_trt_hosp                  1   1   1   1   1   1   1   1   1
#   pct_trt_nonmedical            1   1   1   1   1   1   1   1   1
#   zoom                          1   1   1   1   1   1   1   1   1
