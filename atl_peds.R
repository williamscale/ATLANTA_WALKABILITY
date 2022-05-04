# Atlanta Walkability
# Last Updated: 03/22/22

# ADMINISTRATIVE WORK -----------------------------------------------------

# Clear workspace and set seed.
rm(list = ls())
set.seed(55)

# Import libraries.
library(readxl)
library(ggplot2)
library(ggthemes)
library(rgdal)
library(dplyr)

# Set working directory.
setwd('C:/Users/caler/Documents/MyProjects/ATLANTA_WALKABILITY')

# DATA PREP ---------------------------------------------------------------

# Read in citation data.
citations.raw <- read_excel('citations.xlsx')

# Remove rows without latitude or longitude.
citations.loc <- citations.raw[complete.cases(
  citations.raw[, c('LATITUDE', 'LONGITUDE')]), ]

# Inspect.
ggplot(data = citations.loc,
       aes(x = LATITUDE,
           y = LONGITUDE)) +
  geom_point() +
  theme_solarized_2()

# Remove outliers.
citations <- citations.loc[citations.loc$LATITUDE < 33.9 &
                             citations.loc$LONGITUDE < -80, ]

# Inspect.
ggplot(data = citations,
       aes(x = LATITUDE,
           y = LONGITUDE,
           color = arrest_section)) +
  geom_point() +
  theme_solarized_2()

citations$rpt_date <- as.Date(x = citations$rpt_date,
                              format = '%m/%d/%Y')

# CHANGE DETECTION --------------------------------------------------------

start.date <- min(citations$rpt_date)

all.dates <- seq.Date(from = start.date,
                      to = max(citations$rpt_date),
                      by = 'day')



C <- 4
S.t <- c(0)

citations.byDate <- citations %>%
  count(rpt_date) %>%
  arrange(rpt_date)

citations.byDate$Day <- as.numeric(citations.byDate$rpt_date -
                                     start.date + 1)

all.days <- seq(from = min(citations.byDate$Day),
                to = max(citations.byDate$Day))

citations.byDate <- merge(x = data.frame(Day = all.days),
                          y = citations.byDate,
                          all.x = TRUE)

citations.byDate$Date <- all.dates
citations.byDate <- citations.byDate[, -2]
citations.byDate$n[is.na(citations.byDate$n)] <- 0

for (i in 2:nrow(citations.byDate)) {
  
  S.t <- c(S.t, max(0, S.t[i - 1] - (sum(citations.byDate$n[1:(i - 1)]) /
            citations.byDate$Day[i - 1]) + citations.byDate$n[i] - C))
  
}

citations.byDate$S.t <- S.t

ggplot(data = citations.byDate,
       aes(x = Date,
           y = S.t)) +
  geom_line() +
  ylab(bquote(S[t])) +
  theme_solarized_2()

citations.byDate.AA <- citations.byDate[1:1878, ]

ggplot(data = citations.byDate.AA,
       aes(x = Date,
           y = S.t)) +
  geom_line() +
  ylab(bquote(S[t])) +
  theme_solarized_2()

# EXPONENTIAL SMOOTHING ---------------------------------------------------

L <- 365
  
# Create time series data. 
citations.ts <- ts(data = citations.byDate.AA$n,
                   start = 1,
                   frequency = L)

# Run Holt-Winters multiplicative triple exponential smoothing.
S_hw <- HoltWinters(x = citations.ts,
                    alpha = NULL,
                    beta = NULL,
                    gamma = NULL,
                    seasonal = 'additive')

# Plot Holt-Winters and baseline.
plot(S_hw)

# Plot decomposed data.
plot(decompose(citations.ts))

seasonality <- S_hw$fitted[, 4]
baseline <- S_hw$fitted[, 2]
plot(baseline)
plot(seasonality)

# EXPLORATORY PLOTS -------------------------------------------------------

# # Create hexbin plot of attempted field goals.
# # Remove bins with 5 <= count <= 50.
# ggplot(data = citations,
#        aes(x = LATITUDE,
#            y = LONGITUDE),
#        fill = ..count..) +
#   geom_hex(bins = 12) +
#   scale_fill_continuous(lim = c(5, 1500),
#                         na.value = NA,
#                         type = 'viridis') +
#   ggtitle('Bins w/ < 5 Citations Not Shown') +
#   theme_solarized_2()
# 
# ggplot(data = citations,
#                          aes(x = LATITUDE,
#                              y = LONGITUDE)) +
#   stat_density_2d(aes(fill = ..level..),
#                   geom = 'density_2d')
# 
# 
# my_spdf <- readOGR( 
#   dsn= paste0(getwd(),"/ZIP_Codes.shp"))
# 
# plot(my_spdf[which(my_spdf$ZIP == '30303'), ])
