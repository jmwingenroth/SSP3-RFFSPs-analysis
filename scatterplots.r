library(tidyverse)

# Load data

scc <- list()
temperature <- list()
temperature_pulse <- list()
frs <- list()
frs_pulse <- list()
damages <- list()
damages_pulse <- list()

scc[[1]] <- read_csv("../paper-biodiversity/output/BN-2020-RFF/scc.csv")
scc[[2]] <- read_csv("../paper-biodiversity/output/BN-2020-SSP3/scc.csv")
scc[[3]] <- read_csv("../paper-biodiversity/output/Nunez-2020-RFF/scc.csv")
scc[[4]] <- read_csv("../paper-biodiversity/output/Nunez-2020-SSP3/scc.csv")
scc[[5]] <- read_csv("./scghg-RFF-sectoral-2020-CO2-n10000/sc-CO2-n10000.csv")
scc[[6]] <- read_csv("./scghg-SSP3-sectoral-2020-CO2-n10000/sc-CO2-n10000.csv")

temperature[[1]] <- read_csv("../paper-biodiversity/output/BN-2020-RFF/results/model_1/species_loss_temperature.csv")
temperature[[2]] <- read_csv("../paper-biodiversity/output/BN-2020-SSP3/results/model_1/species_loss_temperature.csv")
temperature[[3]] <- read_csv("../paper-biodiversity/output/Nunez-2020-RFF/results/model_1/species_loss_temperature.csv")
temperature[[4]] <- read_csv("../paper-biodiversity/output/Nunez-2020-SSP3/results/model_1/species_loss_temperature.csv")
temperature[[5]] <- read_csv("./covariates-RFF-n10000/results/model_1/TempNorm_1850to1900_global_temperature_norm.csv") # different normalization year
temperature[[6]] <- read_csv("./covariates-SSP3-n10000/results/model_1/TempNorm_1850to1900_global_temperature_norm.csv") # different normalization year

temperature_pulse[[1]] <- read_csv("../paper-biodiversity/output/BN-2020-RFF/results/model_2/species_loss_temperature.csv")
temperature_pulse[[2]] <- read_csv("../paper-biodiversity/output/BN-2020-SSP3/results/model_2/species_loss_temperature.csv")
temperature_pulse[[3]] <- read_csv("../paper-biodiversity/output/Nunez-2020-RFF/results/model_2/species_loss_temperature.csv")
temperature_pulse[[4]] <- read_csv("../paper-biodiversity/output/Nunez-2020-SSP3/results/model_2/species_loss_temperature.csv")
temperature_pulse[[5]] <- read_csv("./covariates-RFF-n10000/results/model_2/TempNorm_1850to1900_global_temperature_norm.csv") # different normalization year
temperature_pulse[[6]] <- read_csv("./covariates-SSP3-n10000/results/model_2/TempNorm_1850to1900_global_temperature_norm.csv") # different normalization year

frs[[1]] <- read_csv("../paper-biodiversity/output/BN-2020-RFF/results/model_1/biodiversity_damages_frs.csv")
frs[[2]] <- read_csv("../paper-biodiversity/output/BN-2020-SSP3/results/model_1/biodiversity_damages_frs.csv")
frs[[3]] <- read_csv("../paper-biodiversity/output/Nunez-2020-RFF/results/model_1/biodiversity_damages_frs.csv")
frs[[4]] <- read_csv("../paper-biodiversity/output/Nunez-2020-SSP3/results/model_1/biodiversity_damages_frs.csv")

frs_pulse[[1]] <- read_csv("../paper-biodiversity/output/BN-2020-RFF/results/model_2/biodiversity_damages_frs.csv")
frs_pulse[[2]] <- read_csv("../paper-biodiversity/output/BN-2020-SSP3/results/model_2/biodiversity_damages_frs.csv")
frs_pulse[[3]] <- read_csv("../paper-biodiversity/output/Nunez-2020-RFF/results/model_2/biodiversity_damages_frs.csv")
frs_pulse[[4]] <- read_csv("../paper-biodiversity/output/Nunez-2020-SSP3/results/model_2/biodiversity_damages_frs.csv")

damages[[1]] <- read_csv("../paper-biodiversity/output/BN-2020-RFF/results/model_1/DamageAggregator_biodiversity_damage.csv")
damages[[2]] <- read_csv("../paper-biodiversity/output/BN-2020-SSP3/results/model_1/DamageAggregator_biodiversity_damage.csv")
damages[[3]] <- read_csv("../paper-biodiversity/output/Nunez-2020-RFF/results/model_1/DamageAggregator_biodiversity_damage.csv")
damages[[4]] <- read_csv("../paper-biodiversity/output/Nunez-2020-SSP3/results/model_1/DamageAggregator_biodiversity_damage.csv")

damages_pulse[[1]] <- read_csv("../paper-biodiversity/output/BN-2020-RFF/results/model_2/DamageAggregator_biodiversity_damage.csv")
damages_pulse[[2]] <- read_csv("../paper-biodiversity/output/BN-2020-SSP3/results/model_2/DamageAggregator_biodiversity_damage.csv")
damages_pulse[[3]] <- read_csv("../paper-biodiversity/output/Nunez-2020-RFF/results/model_2/DamageAggregator_biodiversity_damage.csv")
damages_pulse[[4]] <- read_csv("../paper-biodiversity/output/Nunez-2020-SSP3/results/model_2/DamageAggregator_biodiversity_damage.csv")

# Tidy data

models <- rep(c("BN", "Nunez", "Sectoral"), each = 2)
socioeconomics <- rep(c("RFF-SPs", "SSP3"), 3)

for (i in 1:length(scc)) {

    if (models[i] == "Sectoral") {
    
        scc[[i]] <- scc[[i]] %>%
            filter(
                sector == "total",
                discount_rate == "2.0% Ramsey"
            ) %>%
            mutate(trialnum = row_number()) %>%
            select(scc = scghg, trialnum)

        temperature[[i]] <- temperature[[i]] %>%
            rename(temperature = global_temperature_norm)

        temperature_pulse[[i]] <- temperature_pulse[[i]] %>%
            rename(temperature_pulse = global_temperature_norm)
    
    } else {
    
        scc[[i]] <- scc[[i]] %>%
            filter(
                sector == "biodiversity",
                dr == "2.0%"
            ) %>%
            mutate(trialnum = row_number()) %>%
            select(scc, trialnum)

        temperature_pulse[[i]] <- temperature_pulse[[i]] %>%
            rename(temperature_pulse = temperature)

    }
    
    scc[[i]]$model <- models[i]
    scc[[i]]$socio <- socioeconomics[i]
    
    temperature[[i]]$model <- models[i]
    temperature[[i]]$socio <- socioeconomics[i]
    
    temperature_pulse[[i]]$model <- models[i]
    temperature_pulse[[i]]$socio <- socioeconomics[i]
}

for (i in 1:length(frs)) {

    frs[[i]]$socio       <- socioeconomics[i]
    frs_pulse[[i]]$socio <- socioeconomics[i]

    damages[[i]]$socio       <- socioeconomics[i]
    damages_pulse[[i]]$socio <- socioeconomics[i]

    if (i <= 2) {
        
        frs[[i]]$model <- models[1]
        frs_pulse[[i]]$model <- models[1]

        damages[[i]]$model <- models[1]
        damages_pulse[[i]]$model <- models[1]

    }

    if (i > 2)  {

        frs[[i]]$model <- models[3]
        frs_pulse[[i]]$model <- models[3]
        
        damages[[i]]$model <- models[3]
        damages_pulse[[i]]$model <- models[3]
        
    }

    frs_pulse[[i]] <- frs_pulse[[i]] %>%
        rename(frs_pulse = frs)

    damages_pulse[[i]] <- damages_pulse[[i]] %>%
        rename(biodiversity_damage_pulse = biodiversity_damage)

}

scc_tidy <- bind_rows(scc) 

temperature_tidy <- bind_rows(temperature) %>%
    filter(time %in% seq(2050, 2300, 50))

temperature_pulse_tidy <- bind_rows(temperature_pulse) %>%
    filter(time %in% seq(2050, 2300, 50))

frs_tidy <- bind_rows(frs) %>%
    filter(time %in% seq(2050, 2300, 50))

frs_pulse_tidy <- bind_rows(frs_pulse) %>%
    filter(time %in% seq(2050, 2300, 50))

damages_tidy <- bind_rows(damages) %>%
    filter(time %in% seq(2050, 2300, 50))

damages_pulse_tidy <- bind_rows(damages_pulse) %>%
    filter(time %in% seq(2050, 2300, 50))

# Plot data

p1 <- left_join(temperature_tidy, scc_tidy) %>%
    ggplot(aes(x = temperature, y = scc, color = socio)) +
    geom_point(alpha = .3) +
    facet_wrap(~paste(time, model), scales = "free_y", ncol = 3)

ggsave("scc_vs_absolute_temp.png", p1, width = 10, height = 10)

p2 <- left_join(temperature_tidy, temperature_pulse_tidy) %>%
    left_join(scc_tidy) %>%
    mutate(temperature_marginal = temperature_pulse - temperature) %>%
    filter(temperature_marginal > -1e-07) %>%                           # There were some strange negative marginal temperature anomalies 
    ggplot(aes(x = temperature_marginal, y = scc, color = socio)) +
    geom_point(alpha = .3) +
    facet_wrap(~paste(time, model), scales = "free_y", ncol = 3)

ggsave("scc_vs_marginal_temp.png", p2, width = 10, height = 10)

p3 <- left_join(frs_tidy, scc_tidy) %>%
    ggplot(aes(x = frs, y = scc, color = socio)) +
    geom_point(alpha = .3) +
    facet_wrap(~paste(time, model), scales = "free_y", ncol = 2)

ggsave("scc_vs_absolute_frs.png", p3, width = 10, height = 10)

p4 <- left_join(frs_tidy, frs_pulse_tidy) %>%
    left_join(scc_tidy) %>%
    mutate(frs_marginal = frs_pulse - frs) %>%
    filter(frs_marginal > -1e-07, frs_marginal < 5e-8) %>%              # There were some strange FRS anomalies (positive for Nunez versus negative for B&N due to equation form) 
    ggplot(aes(x = frs_marginal, y = scc, color = socio)) +
    geom_point(alpha = .3) +
    facet_grid(time~model, scales = "fixed")

ggsave("scc_vs_marginal_frs.png", p4, width = 10, height = 10)

p5 <- left_join(frs_tidy, frs_pulse_tidy) %>%
    left_join(scc_tidy) %>%
    mutate(frs_marginal = frs_pulse - frs) %>%
    filter(frs_marginal > -1e-07, frs_marginal < 5e-8) %>%              # There were some strange FRS anomalies (positive for Nunez versus negative for B&N due to equation form) 
    ggplot(aes(x = frs_marginal, y = scc, color = socio)) +
    geom_point(alpha = .3) +
    facet_grid(time~model, scales = "fixed") +
    lims(x = c(-2e-8, 0), y = c(NA, 50)) +
    labs(caption = "*focused on Nunez species-loss function range, cropping some Brooks & Newbold data")

ggsave("scc_vs_marginal_frs_zoomed_in.png", p5, width = 10, height = 10)

p6 <- left_join(damages_tidy, damages_pulse_tidy) %>%
    left_join(scc_tidy) %>%
    mutate(damage_marginal = (biodiversity_damage_pulse - biodiversity_damage)/1e5/44*12) %>% # 1e-4 GtC pulse size
    ggplot(aes(x = damage_marginal, y = scc, color = socio)) +
    geom_point(alpha = .3) +
    scale_x_log10(labels = scales::dollar, limits = c(0.01, NA)) +
    facet_wrap(~paste(time, model), scales = "free_y", ncol = 2)

ggsave("scc_vs_marginal_damage.png", p6, width = 10, height = 10)
