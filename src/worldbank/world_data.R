
# Load --------------------------------------------------------------------

# library(devtools)
# install_github("GIST-ORNL/wbstats")
library(wbstats)
library(ggplot2)
# install_github(repo = "mkao006/FAOSTATpackage", subdir = "FAOSTAT")
library(cowplot)
library(data.table)
library(esmisc)
library(FAOSTAT)
library(tikzDevice)


# Retrieve data -----------------------------------------------------------

## From World Bank
wbsearch(pattern = "Population, total")
wbsearch(pattern = "Agricultural land")
wbsearch(pattern = "land area")
wbsearch(pattern = "food production")
wbsearch(pattern = "mortality rate")
wbsearch(pattern = "population growth")
wbsearch(pattern = "gdp per person")

wbsearch(pattern = "income")

# data we want
inds <- c('SP.POP.TOTL',      # Population, total 
          'AG.LND.AGRI.K2',   # Agricultural land (sq. km)
          'AG.LND.TOTL.K2',   # Land area  (sq. km)
          'AG.PRD.FOOD.XD',   # food production index
          'SP.DYN.IMRT.IN',   # Mortality rate, infant (per 1,000 live births)
          'SP.POP.GROW'       # population growth 
          )

# retrieve data from worldbank
wbdata <- wb(country = 'all', indicator = inds, 
             startdate = 1960, enddate = 2016)
setDT(wbdata)
# retrieve country classification
countries <- wbcountries()
setDT(countries)


## From FAO

# search query
# FAOsearch()
query_pi <- structure(list(elementCode = 5622L, itemCode = "1357", domainCode = "RT", 
                        name = "Pesticides (trade)_Pesticides_Import Value (1000 US$)"), 
                   .Names = c("elementCode", "itemCode", "domainCode", "name"), 
                   row.names = c(NA, -1L), class = "data.frame")
pi_raw <- getFAO(query = query_pi)



# Clean data --------------------------------------------------------------

# Join
jd <- countries[ , list(iso2c, country, region, income)][wbdata, on = .(iso2c)]
# date should be numeric
jd[ , date := as.numeric(date)]


# subset world data
world <- jd[region == 'Aggregates' & country == "World"]
world[ ,c( "i.country", "iso2c", "country", "region", "income") := NULL]



# Remove aggregates (=keep only countries)
df <- jd[!region == 'Aggregates']

# Remove unused cols
df[ ,c("iso2c", "i.country") := NULL]

# indicator
unique(df$indicator)


# keep only countries with data from 1961 - max(date)
df <- df[ , .SD[min(date) <= 1961], by = list(country, indicator)]

# calculate & add % agricultural land per country
# # (cannot use world aggregate as influenced by countries added later?)
j <- df[indicator  == "Agricultural land (sq. km)", list(country, indicator, iso3c, region, income, value, date), 
   keyby = list(country, date)][
  df[indicator  == "Land area (sq. km)", list(country, date, value), 
     keyby = list(country, date)]]
j[ , value := value / i.value]
j[ , c('i.value', 'country.1', 'date.1',  'i.country', 'i.date') := NULL]
j[ , indicator := 'Agricultural land (%)']
j <- j[!is.na(value)]
df <- rbindlist(list(df, j[ , names(df), with = FALSE]))


# calculate % agricultural land of world per country
j <- df[ , list(value = .SD[indicator  == "Agricultural land (sq. km)", sum(value)] /
            .SD[indicator  == "Land area (sq. km)", sum(value)],
           indicator = "Agricultural land (%)"), 
    by = date]
world <- rbindlist(list(world, j), fill = TRUE)



### Clean FAO data
# add country code
pi_raw <- translateCountryCode(data = pi_raw, from = "FAOST_CODE",
                           to = "ISO2_CODE")
# merge with countries
setDT(pi_raw)
pi <- countries[ , list(iso2c, country, region, income)][pi_raw[!is.na(ISO2_CODE), ], on = c(iso2c = "ISO2_CODE"), nomatch = 0]
setnames(pi, 'Pesticides (trade)_Pesticides_Import Value (1000 US$)', 'value')




# Plot data ---------------------------------------------------------------

# 1 plot population over time
# aggregate population by income
pdat <- df[indicator == "Population, total", 
           list(value = sum(value)),  # sum population of all countries
           by = list(income, date)]
# set order
pdat$income <- factor(pdat$income, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))
pop <- ggplot() +
  geom_area(data = pdat, 
            aes(x = date, y = value, fill = income), alpha = 0.8) +
  theme_edi(base_size = 16) +
  labs(x = 'Year', y = 'Population') +
  ggtitle('Population since 1960', subtitle = 'Source: World Bank') +
  scale_fill_brewer('Income', palette = 'Dark2') +
  scale_y_continuous(breaks = c(1e9) * c(0:7)) +
  scale_x_continuous(breaks = seq(1960, 2015, 10)) +
  theme(legend.position = 'none')
pop


# 2 plot food production over time
food <- ggplot() +
  geom_line(data = world[indicator == 'Food production index (2004-2006 = 100)'],
            aes(x = date, y = value), size = 2) +
  theme_edi(base_size = 16) +
  scale_x_continuous(breaks = seq(1960, 2015, 10)) +
  labs(x = 'Year', y = 'Index value') +
  ggtitle('Food production index',
          subtitle = 'Source: World Bank; (2004-2006 = 100)')
food


# 3 agricultural land
pdat <- df[ , list(value = .SD[indicator  == "Agricultural land (sq. km)", sum(value)] /
             .SD[indicator  == "Land area (sq. km)", sum(value)],
           indicator = "Agricultural land (%)"),
    by = list(date, income)]
pdat <- pdat[!value == 0]
agriarea <- ggplot() +
  geom_line(data = pdat, 
            aes(x = date, y = value*100, col = income), size = 2) +
  geom_line(data = world[indicator == 'Agricultural land (%)' & !value == 0],
            aes(x = date, y = value*100), size = 2) +
  theme_edi(base_size = 16) +
  labs(x = 'Year', y = 'Agricultural Area [\\%]') +
  ggtitle('Agricultural Area [\\%]', subtitle = 'Source: World Bank') +
  scale_color_brewer('Income', palette = 'Dark2') +
  scale_x_continuous(breaks = seq(1960, 2015, 10)) +
  theme(legend.position = 'none')
agriarea


# 4 pesticide imports
# tilman like plot
pi$income <- factor(pi$income, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))
mean_pi <- pi[ , list(m = mean(value)) , by = list(Year, income)]

piplot <- ggplot() +
  geom_line(data = pi, aes(x = Year, y = value/1000, col = income, group = country), alpha = 0.3) + 
  geom_line(data = mean_pi, aes(x = Year, y = m/1000, col = income), size = 2) +
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000)) +
  scale_x_continuous(breaks = seq(1960, 2015, 10)) +
  theme_edi(base_size = 16) +
  scale_color_brewer('Income', palette = 'Dark2') +
  labs(y = 'Pesticide Imports (million US \\$)') +
  ggtitle('Pesticide Imports', subtitle = 'Source: Food and Agriculture Organization (FAO)')  +
  theme(legend.position = c(0.8, 0.15))
piplot

ap_persp <- plot_grid(pop, food, agriarea, piplot, ncol = 2)


ggsave('/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/figs/tikz/ap_pers.tikz', 
       ap_persp, 
       device = tikz,
       width = 14, height = 12)


