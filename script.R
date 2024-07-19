# Hotspots Casos ####
# Step 1. extract the locality ####
loc <- rgeomex::extract_locality(cve_edo = "19", 
                                 locality = c("Ciudad General Escobedo", "Ciudad Apodaca",
                                              "Guadalupe", "Monterrey", "Ciudad Santa Catarina",
                                              "San Pedro Garza García", "Ciudad Benito Juárez",
                                              "San Nicolás de los Garza"))

loc <- rgeomex::extract_locality(cve_edo = "19",
                                 locality = "Ciudad Sabinas Hidalgo")

# Step 2. Extract the hotspots ####
hotspots <- denmex::den_hotspots[loc, ]
mapview::mapview(hotspots,
                 zcol = "intensity_gi",
                 layer.name = "Intensidad",
                 label = FALSE,
                 color = "white",
                 lwd = 0.5, 
                 col.regions =  rcartocolor::carto_pal(n = max(hotspots$intensity_gi), 
                                                       name = "OrYel"))

library(mapgl)
library(mapgl)
library(sf)

# Step 3. 
deneggs::map_eggs_hotspots(betas = denmex::eggs_betas_19_nuevo_leon,
                           locality = "Santiago",
                           cve_edo = "19",
                           palette = rcartocolor::carto_pal,
                           name = "SunsetDark",
                           static_map = FALSE)

denhotspots::risk_ageb(betas = denmex::eggs_betas_19_nuevo_leon,
                       hotspots = denmex::den_hotspots,
                       intensity_perc = 25,
                       locality = "Ciudad Sabinas Hidalgo",
                       cve_edo = "19") |>
    denhotspots::map_risk(staticmap = FALSE)


##
# Logistic Regression
hotspots <- denmex::den_hotspots[loc, ]
hotspots <- hotspots |>
    dplyr::mutate(DENV_2008_2023 = rowSums(dplyr::across(dplyr::starts_with("DENV")))) |>
    dplyr::mutate(DENV_2008_2010 = DENV_2008 + DENV_2009 + DENV_2010) |>
    plyr::mutate(DENV_2011_2014 = DENV_2011 + DENV_2012 + DENV_2013 + DENV_2014) |>
    dplyr::mutate(DENV_2015_2017 = DENV_2015 +  DENV_2016 + DENV_2017) |>
    dplyr::mutate(DENV_2018_2019 = DENV_2018 +  DENV_2019) |>
    dplyr::mutate(DENV_2020_2021 = DENV_2020 +  DENV_2021) |>
    dplyr::mutate(DENV_2022_2023 = DENV_2022 +  DENV_2023) 


# Logistic Regression
glm.fit <- glm(hotspots_gi ~ DENV_2008_2010 + DENV_2011_2014 +
                   DENV_2015_2017 + DENV_2018_2019 +
                   DENV_2020_2021 + DENV_2022_2023,
               data = hotspots, 
               family = binomial)

ggstats::ggcoef_table(glm.fit,
                      significance_labels = c("Significativo", 
                                              "No singificativo"),
                      stripped_rows = TRUE,
                      exponentiate = TRUE) 

ggstats::ggcoef_model(glm.fit,
                      exponentiate = TRUE)
    

result <- parameters::model_parameters(glm.fit, 
                                       exponentiate = TRUE)

plotly::ggplotly(plot(result, size_text = 1))
