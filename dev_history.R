
# Document functions and dependencies
attachment::att_to_description()
attachment::att_amend_desc()

# Check the package
devtools::check()

#mit license
usethis::use_mit_license()

usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore('Clean')
usethis::use_git_ignore('Clean')


bgd_msna <- df |>
  select(`_uuid`,lon = `_gps_reading_longitude`,lat= `_gps_reading_latitude`,informed_consent,survey_date, end_survey,electricity_grid,
         solar_light,illness_HH_count,`cooking_fuel/collected_firewood`,
         `income_source/agricultural_production_sale`  ,
         agricultural_land ,
         `employment_source/agricultural_casual`,
         `employment_source/non_agricultural_casual`,
         `employment_source/fishing` ) |>
  filter(informed_consent=="yes")

usethis::use_data(bgd_msna,overwrite=T)

