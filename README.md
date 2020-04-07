
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Estimating COVID-19 underreporting using TB case detection rates

Manuscript at:
<https://docs.google.com/document/d/1yC1M4ifLSp3XoLeWSoyNLABcUnuV9U-77NnFtzumdzQ/edit>

``` r
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(cowplot)
theme_set(theme_cowplot())
options(scipen = '999')
# Get WHO CDR data
who <- read_csv('data/API_SH.TBS.DTEC.ZS_DS2_en_csv_v2_895250.csv', skip = 3) %>%
  dplyr::select(country = `Country Name`, iso = `Country Code`, cdr = `2018`) %>%
  filter(!is.na(cdr))

# Get the pop data
world_pop <- read_csv('data/world_pop.csv') %>% filter(!is.na(pop))

# Get JHU COVID data
jhu <- read_csv('data/jhu.csv')
# Get most recent date
most_recent <- jhu %>% filter(date >= (Sys.Date() - 7)) %>% group_by(country) %>% summarise(d = max(date)) %>%
  ungroup %>% summarise(d = min(d)) %>% .$d
covid <- jhu %>% filter(date == most_recent)

# Join
joined <- 
  covid %>% dplyr::select(iso, deaths, cases) %>%
  left_join(who %>% dplyr::select(iso, cdr)) %>%
  left_join(world_pop %>% dplyr::select(iso, pop, region, sub_region)) %>%
  filter(!is.na(region)) %>%
  filter(region != 'Oceania') %>%
  mutate(y = cases / pop * 100000)

cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, name = 'Set1'))(length(unique(joined$region)))
fit <- lm(log(y) ~ cdr, data = joined)
ggplot(data = joined,
       aes(x = cdr,
           y = y)) +
  # facet_wrap(~region, scales = 'free') +
  scale_y_log10() +
  # scale_x_log10() +
  geom_jitter(aes(#color = region,
                  pch = region),
              size = 3,
              width = 5,
              height = 0.1,
              alpha = 0.8) +
  geom_smooth(#method = 'lm',
              # aes(group = region,
              #     color = region), 
              color = 'black',
              # formula = log(y) ~ x,
              method = 'lm',#  'gam',
              se = FALSE,
              lty = 2) +
  labs(x = 'TB case detection rate',
       y = 'Confirmed COVID-19 cases per 100,000 population',
       title = 'Association between TB Case detection rate and cumulative COVID-19 incidence') +
  scale_color_manual(name = '',
                     values = cols) +
  scale_shape_manual(name = '',
                     values = c(1,2, 5, 8)) +
  theme(legend.position = 'top')# +
```

<img src="figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
  # stat_function(fun = function(x) predict(fit, newdata = data.frame(cdr=x)))
```

``` r
ggplot(data = joined,
       aes(x = cdr,
           y = y,
           group = region)) +
  facet_wrap(~region, scales = 'free') +
  scale_y_log10() +
  # scale_x_log10() +
  geom_jitter(aes(#color = region,
                  pch = region),
              size = 3,
              width = 5,
              height = 0.1,
              alpha = 0.8,
              show.legend = FALSE) +
  geom_smooth(#method = 'lm',
              # aes(group = region,
              #     color = region), 
              color = 'black',
              # formula = log(y) ~ x,
              method = 'lm',#  'gam',
              se = FALSE,
              lty = 2) +
  labs(x = 'TB case detection rate',
       y = 'Confirmed COVID-19 cases per 100,000 population',
       title = 'Association between TB Case detection rate and cumulative COVID-19 incidence') +
  # scale_color_manual(name = '',
  #                    values = cols) +
  scale_shape_manual(name = '',
                     values = c(1,2, 5, 8)) +
  theme(legend.position = 'top')# +
```

<img src="figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
  # stat_function(fun = function(x) predict(fit, newdata = data.frame(cdr=x)))
```

# Make adjustment

``` r
out <- joined %>%
  left_join(world_pop %>% dplyr::select(iso, country)) %>%
  filter(!is.na(country)) %>%
  arrange(country) %>%
  mutate(adj = cases * (100/cdr)) %>%
    dplyr::mutate(ISO = iso, 
                  Region = region,
                  `Confirmed cases` = cases,
                  `Adjusted cases` = round(adj, digits = 1),
                  `TB CDR` = cdr,
                  `Confirmed cumulative incidence per 100,000` = round(cases / pop * 100000, digits = 2),
                  `Adjusted cumulative incidence per 100,000` = round(digits = 2)) %>%
  dplyr::select(ISO, Country = country, Region,`Confirmed cases`, `Adjusted cases`, `TB CDR`, `Confirmed cumulative incidence per 100,000`,`Adjusted cumulative incidence per 100,000`)
write_csv(out, 'adjustments.csv')
knitr::kable(out)
```

| ISO | Country                        | Region   | Confirmed cases | Adjusted cases | TB CDR | Confirmed cumulative incidence per 100,000 | Adjusted cumulative incidence per 100,000 |
| :-- | :----------------------------- | :------- | --------------: | -------------: | -----: | -----------------------------------------: | ----------------------------------------: |
| AFG | Afghanistan                    | Asia     |             367 |          531.9 |     69 |                                       0.99 |                                         2 |
| ALB | Albania                        | Europe   |             377 |          433.3 |     87 |                                      13.15 |                                         2 |
| DZA | Algeria                        | Africa   |            1423 |         1778.8 |     80 |                                       3.37 |                                         2 |
| AND | Andorra                        | Europe   |             525 |          603.4 |     87 |                                     681.77 |                                         2 |
| AGO | Angola                         | Africa   |              16 |           26.2 |     61 |                                       0.05 |                                         2 |
| ATG | Antigua and Barbuda            | Americas |              15 |           17.2 |     87 |                                      15.58 |                                         2 |
| ARG | Argentina                      | Americas |            1554 |         1786.2 |     87 |                                       3.49 |                                         2 |
| ARM | Armenia                        | Asia     |             833 |         1041.2 |     80 |                                      28.22 |                                         2 |
| AUT | Austria                        | Europe   |           12297 |        16617.6 |     74 |                                     139.00 |                                         2 |
| AZE | Azerbaijan                     | Asia     |             641 |          801.2 |     80 |                                       6.45 |                                         2 |
| BHS | Bahamas, The                   | Americas |              29 |           33.3 |     87 |                                       7.52 |                                         2 |
| BHR | Bahrain                        | Asia     |             756 |          869.0 |     87 |                                      48.17 |                                         2 |
| BGD | Bangladesh                     | Asia     |             123 |          164.0 |     75 |                                       0.08 |                                         2 |
| BRB | Barbados                       | Americas |              60 |           69.0 |     87 |                                      20.93 |                                         2 |
| BLR | Belarus                        | Europe   |             700 |          875.0 |     80 |                                       7.38 |                                         2 |
| BEL | Belgium                        | Europe   |           20814 |        23652.3 |     88 |                                     182.23 |                                         2 |
| BLZ | Belize                         | Americas |               7 |            8.0 |     87 |                                       1.83 |                                         2 |
| BEN | Benin                          | Africa   |              26 |           41.9 |     62 |                                       0.23 |                                         2 |
| BTN | Bhutan                         | Asia     |               5 |            6.2 |     80 |                                       0.66 |                                         2 |
| BOL | Bolivia                        | Americas |             183 |          295.2 |     62 |                                       1.61 |                                         2 |
| BIH | Bosnia and Herzegovina         | Europe   |             674 |          842.5 |     80 |                                      20.28 |                                         2 |
| BWA | Botswana                       | Africa   |               6 |           10.2 |     59 |                                       0.27 |                                         2 |
| BRA | Brazil                         | Americas |           12161 |        13978.2 |     87 |                                       5.81 |                                         2 |
| BRN | Brunei Darussalam              | Asia     |             135 |          155.2 |     87 |                                      31.47 |                                         2 |
| BGR | Bulgaria                       | Europe   |             549 |          669.5 |     82 |                                       7.82 |                                         2 |
| BFA | Burkina Faso                   | Africa   |             364 |          577.8 |     63 |                                       1.84 |                                         2 |
| BDI | Burundi                        | Africa   |               3 |            5.2 |     58 |                                       0.03 |                                         2 |
| CPV | Cabo Verde                     | Africa   |               7 |            8.8 |     80 |                                       1.29 |                                         2 |
| KHM | Cambodia                       | Asia     |             114 |          196.6 |     58 |                                       0.70 |                                         2 |
| CMR | Cameroon                       | Africa   |             658 |         1316.0 |     50 |                                       2.61 |                                         2 |
| CAN | Canada                         | Americas |           16563 |        19037.9 |     87 |                                      44.69 |                                         2 |
| CAF | Central African Republic       | Africa   |               8 |           18.6 |     43 |                                       0.17 |                                         2 |
| TCD | Chad                           | Africa   |               9 |           15.3 |     59 |                                       0.06 |                                         2 |
| CHL | Chile                          | Americas |            4815 |         5534.5 |     87 |                                      25.71 |                                         2 |
| CHN | China                          | Asia     |           82665 |        89853.3 |     92 |                                       5.94 |                                         2 |
| COL | Colombia                       | Americas |            1579 |         1973.8 |     80 |                                       3.18 |                                         2 |
| COD | Congo, Dem. Rep.               | Africa   |             161 |          255.6 |     63 |                                       0.19 |                                         2 |
| COG | Congo, Rep.                    | Africa   |              45 |           83.3 |     54 |                                       0.86 |                                         2 |
| CRI | Costa Rica                     | Americas |             467 |          583.8 |     80 |                                       9.34 |                                         2 |
| CIV | Cote d’Ivoire                  | Africa   |             323 |          547.5 |     59 |                                       1.29 |                                         2 |
| HRV | Croatia                        | Europe   |            1222 |         1222.0 |    100 |                                      29.88 |                                         2 |
| CUB | Cuba                           | Americas |             350 |          402.3 |     87 |                                       3.09 |                                         2 |
| CYP | Cyprus                         | Asia     |             465 |          588.6 |     79 |                                      39.10 |                                         2 |
| CZE | Czech Republic                 | Europe   |            4822 |         6429.3 |     75 |                                      45.38 |                                         2 |
| DNK | Denmark                        | Europe   |            4875 |         5603.4 |     87 |                                      84.09 |                                         2 |
| DJI | Djibouti                       | Africa   |              90 |          112.5 |     80 |                                       9.39 |                                         2 |
| DMA | Dominica                       | Americas |              15 |           17.2 |     87 |                                      20.94 |                                         2 |
| DOM | Dominican Republic             | Americas |            1828 |         2285.0 |     80 |                                      17.20 |                                         2 |
| ECU | Ecuador                        | Americas |            3747 |         4683.8 |     80 |                                      21.93 |                                         2 |
| EGY | Egypt, Arab Rep.               | Africa   |            1322 |         1944.1 |     68 |                                       1.34 |                                         2 |
| SLV | El Salvador                    | Americas |              69 |           86.2 |     80 |                                       1.07 |                                         2 |
| GNQ | Equatorial Guinea              | Africa   |              16 |           32.0 |     50 |                                       1.22 |                                         2 |
| EST | Estonia                        | Europe   |            1108 |         1273.6 |     87 |                                      83.88 |                                         2 |
| ETH | Ethiopia                       | Africa   |              44 |           63.8 |     69 |                                       0.04 |                                         2 |
| FIN | Finland                        | Europe   |            2176 |         2472.7 |     88 |                                      39.43 |                                         2 |
| FRA | France                         | Europe   |           98963 |       119232.5 |     83 |                                     147.73 |                                         2 |
| GAB | Gabon                          | Africa   |              24 |           47.1 |     51 |                                       1.13 |                                         2 |
| GMB | Gambia, The                    | Africa   |               4 |            6.7 |     60 |                                       0.18 |                                         2 |
| GEO | Georgia                        | Asia     |             188 |          257.5 |     73 |                                       5.04 |                                         2 |
| DEU | Germany                        | Europe   |          103374 |       118820.7 |     87 |                                     124.66 |                                         2 |
| GHA | Ghana                          | Africa   |             214 |          668.8 |     32 |                                       0.72 |                                         2 |
| GRC | Greece                         | Europe   |            1755 |         2040.7 |     86 |                                      16.36 |                                         2 |
| GRD | Grenada                        | Americas |              12 |           13.8 |     87 |                                      10.77 |                                         2 |
| GTM | Guatemala                      | Americas |              70 |           87.5 |     80 |                                       0.41 |                                         2 |
| GIN | Guinea                         | Africa   |             128 |          196.9 |     65 |                                       1.03 |                                         2 |
| GNB | Guinea-Bissau                  | Africa   |              18 |           60.0 |     30 |                                       0.96 |                                         2 |
| GUY | Guyana                         | Americas |              31 |           38.8 |     80 |                                       3.98 |                                         2 |
| HTI | Haiti                          | Americas |              24 |           34.8 |     69 |                                       0.22 |                                         2 |
| HND | Honduras                       | Americas |             298 |          372.5 |     80 |                                       3.11 |                                         2 |
| HUN | Hungary                        | Europe   |             744 |          767.0 |     97 |                                       7.62 |                                         2 |
| ISL | Iceland                        | Europe   |            1562 |         1795.4 |     87 |                                     441.77 |                                         2 |
| IND | India                          | Asia     |            4778 |         6456.8 |     74 |                                       0.35 |                                         2 |
| IDN | Indonesia                      | Asia     |            2491 |         3717.9 |     67 |                                       0.93 |                                         2 |
| IRN | Iran, Islamic Rep.             | Asia     |           60500 |        75625.0 |     80 |                                      73.96 |                                         2 |
| IRQ | Iraq                           | Asia     |            1031 |         2343.2 |     44 |                                       2.68 |                                         2 |
| IRL | Ireland                        | Europe   |            5364 |         6165.5 |     87 |                                     110.52 |                                         2 |
| ISR | Israel                         | Asia     |            8904 |        10234.5 |     87 |                                     100.23 |                                         2 |
| ITA | Italy                          | Europe   |          132547 |       150621.6 |     88 |                                     219.34 |                                         2 |
| JAM | Jamaica                        | Americas |              58 |           72.5 |     80 |                                       1.98 |                                         2 |
| JPN | Japan                          | Asia     |            3654 |         4200.0 |     87 |                                       2.89 |                                         2 |
| JOR | Jordan                         | Asia     |             349 |          436.2 |     80 |                                       3.51 |                                         2 |
| KAZ | Kazakhstan                     | Asia     |             662 |          662.0 |    100 |                                       3.62 |                                         2 |
| KEN | Kenya                          | Africa   |             158 |          250.8 |     63 |                                       0.31 |                                         2 |
| KOR | Korea, Rep.                    | Asia     |           10284 |        10940.4 |     94 |                                      19.92 |                                         2 |
| KWT | Kuwait                         | Asia     |             665 |          764.4 |     87 |                                      16.07 |                                         2 |
| KGZ | Kyrgyz Republic                | Asia     |             216 |          248.3 |     87 |                                       3.42 |                                         2 |
| LAO | Lao PDR                        | Asia     |              12 |           21.1 |     57 |                                       0.17 |                                         2 |
| LVA | Latvia                         | Europe   |             542 |             NA |     NA |                                      28.13 |                                         2 |
| LBN | Lebanon                        | Asia     |             541 |          621.8 |     87 |                                       7.90 |                                         2 |
| LBR | Liberia                        | Africa   |              14 |           26.4 |     53 |                                       0.29 |                                         2 |
| LBY | Libya                          | Africa   |              19 |           27.9 |     68 |                                       0.28 |                                         2 |
| LIE | Liechtenstein                  | Europe   |              77 |             NA |     NA |                                     203.11 |                                         2 |
| LTU | Lithuania                      | Europe   |             843 |          969.0 |     87 |                                      30.22 |                                         2 |
| LUX | Luxembourg                     | Europe   |            2843 |         3267.8 |     87 |                                     467.81 |                                         2 |
| MDG | Madagascar                     | Africa   |              82 |          149.1 |     55 |                                       0.31 |                                         2 |
| MWI | Malawi                         | Africa   |               5 |           10.4 |     48 |                                       0.03 |                                         2 |
| MYS | Malaysia                       | Asia     |            3793 |         4359.8 |     87 |                                      12.03 |                                         2 |
| MDV | Maldives                       | Asia     |              19 |           23.8 |     80 |                                       3.68 |                                         2 |
| MLI | Mali                           | Africa   |              47 |           69.1 |     68 |                                       0.25 |                                         2 |
| MLT | Malta                          | Europe   |             241 |          262.0 |     92 |                                      49.84 |                                         2 |
| MRT | Mauritania                     | Africa   |               6 |           10.2 |     59 |                                       0.14 |                                         2 |
| MUS | Mauritius                      | Africa   |             244 |          305.0 |     80 |                                      19.28 |                                         2 |
| MEX | Mexico                         | Americas |            2143 |         2678.8 |     80 |                                       1.70 |                                         2 |
| MDA | Moldova                        | Europe   |             965 |         1109.2 |     87 |                                      27.21 |                                         2 |
| MCO | Monaco                         | Europe   |              77 |             NA |     NA |                                     199.06 |                                         2 |
| MNG | Mongolia                       | Asia     |              15 |           51.7 |     29 |                                       0.47 |                                         2 |
| MNE | Montenegro                     | Europe   |             233 |          267.8 |     87 |                                      37.44 |                                         2 |
| MAR | Morocco                        | Africa   |            1120 |         1287.4 |     87 |                                       3.11 |                                         2 |
| MOZ | Mozambique                     | Africa   |              10 |           17.5 |     57 |                                       0.03 |                                         2 |
| MMR | Myanmar                        | Asia     |              22 |           28.9 |     76 |                                       0.04 |                                         2 |
| NAM | Namibia                        | Africa   |              16 |           26.2 |     61 |                                       0.65 |                                         2 |
| NPL | Nepal                          | Asia     |               9 |           12.0 |     75 |                                       0.03 |                                         2 |
| NLD | Netherlands                    | Europe   |           18926 |        21754.0 |     87 |                                     109.84 |                                         2 |
| NIC | Nicaragua                      | Americas |               6 |            7.5 |     80 |                                       0.09 |                                         2 |
| NER | Niger                          | Africa   |             253 |          460.0 |     55 |                                       1.13 |                                         2 |
| NGA | Nigeria                        | Africa   |             238 |          991.7 |     24 |                                       0.12 |                                         2 |
| MKD | North Macedonia                | Europe   |             570 |          712.5 |     80 |                                      27.36 |                                         2 |
| NOR | Norway                         | Europe   |            5865 |         6741.4 |     87 |                                     110.36 |                                         2 |
| OMN | Oman                           | Asia     |             331 |          380.5 |     87 |                                       6.85 |                                         2 |
| PAK | Pakistan                       | Asia     |            3766 |         5884.4 |     64 |                                       1.77 |                                         2 |
| PAN | Panama                         | Americas |            1988 |         2485.0 |     80 |                                      47.60 |                                         2 |
| PRY | Paraguay                       | Americas |             113 |          129.9 |     87 |                                       1.62 |                                         2 |
| PER | Peru                           | Americas |            2561 |         3201.2 |     80 |                                       8.01 |                                         2 |
| PHL | Philippines                    | Asia     |            3660 |         5809.5 |     63 |                                       3.43 |                                         2 |
| POL | Poland                         | Europe   |            4413 |         5072.4 |     87 |                                      11.62 |                                         2 |
| PRT | Portugal                       | Europe   |           11730 |        13482.8 |     87 |                                     114.09 |                                         2 |
| QAT | Qatar                          | Asia     |            1832 |         2105.7 |     87 |                                      65.86 |                                         2 |
| ROU | Romania                        | Europe   |            4057 |         4663.2 |     87 |                                      20.83 |                                         2 |
| RUS | Russian Federation             | Europe   |            6343 |         6407.1 |     99 |                                       4.39 |                                         2 |
| RWA | Rwanda                         | Africa   |             105 |          131.2 |     80 |                                       0.85 |                                         2 |
| SMR | San Marino                     | Europe   |             266 |             NA |     NA |                                     787.33 |                                         2 |
| STP | Sao Tome and Principe          | Africa   |               4 |            7.0 |     57 |                                       1.90 |                                         2 |
| SAU | Saudi Arabia                   | Asia     |            2605 |         2994.3 |     87 |                                       7.73 |                                         2 |
| SEN | Senegal                        | Africa   |             226 |          318.3 |     71 |                                       1.43 |                                         2 |
| SRB | Serbia                         | Europe   |            2200 |         2528.7 |     87 |                                      31.51 |                                         2 |
| SYC | Seychelles                     | Africa   |              11 |           12.6 |     87 |                                      11.37 |                                         2 |
| SLE | Sierra Leone                   | Africa   |               6 |            8.0 |     75 |                                       0.08 |                                         2 |
| SGP | Singapore                      | Asia     |            1375 |         1580.5 |     87 |                                      24.39 |                                         2 |
| SVK | Slovak Republic                | Europe   |             534 |          613.8 |     87 |                                       9.80 |                                         2 |
| SVN | Slovenia                       | Europe   |            1021 |         1147.2 |     89 |                                      49.39 |                                         2 |
| SOM | Somalia                        | Africa   |               7 |           16.7 |     42 |                                       0.05 |                                         2 |
| ZAF | South Africa                   | Africa   |            1686 |         2218.4 |     76 |                                       2.92 |                                         2 |
| SSD | South Sudan                    | Africa   |               1 |            1.1 |     91 |                                       0.01 |                                         2 |
| ESP | Spain                          | Europe   |          140511 |       140511.0 |    100 |                                     300.73 |                                         2 |
| LKA | Sri Lanka                      | Asia     |             178 |          278.1 |     64 |                                       0.82 |                                         2 |
| KNA | St. Kitts and Nevis            | Americas |              10 |             NA |     NA |                                      19.07 |                                         2 |
| LCA | St. Lucia                      | Americas |              14 |           16.1 |     87 |                                       7.70 |                                         2 |
| VCT | St. Vincent and the Grenadines | Americas |               7 |            8.0 |     87 |                                       6.35 |                                         2 |
| SDN | Sudan                          | Africa   |              12 |           17.9 |     67 |                                       0.03 |                                         2 |
| SUR | Suriname                       | Americas |              10 |           12.5 |     80 |                                       1.74 |                                         2 |
| SWE | Sweden                         | Europe   |            7206 |         8282.8 |     87 |                                      70.76 |                                         2 |
| CHE | Switzerland                    | Europe   |           21657 |        24893.1 |     87 |                                     254.29 |                                         2 |
| SYR | Syrian Arab Republic           | Asia     |              19 |           23.8 |     80 |                                       0.11 |                                         2 |
| TZA | Tanzania                       | Africa   |              24 |           45.3 |     53 |                                       0.04 |                                         2 |
| THA | Thailand                       | Asia     |            2220 |         2775.0 |     80 |                                       3.20 |                                         2 |
| TLS | Timor-Leste                    | Asia     |               1 |            1.7 |     60 |                                       0.08 |                                         2 |
| TGO | Togo                           | Africa   |              58 |           68.2 |     85 |                                       0.74 |                                         2 |
| TTO | Trinidad and Tobago            | Americas |             105 |          120.7 |     87 |                                       7.55 |                                         2 |
| TUN | Tunisia                        | Africa   |             596 |          745.0 |     80 |                                       5.15 |                                         2 |
| TUR | Turkey                         | Asia     |           30217 |        34732.2 |     87 |                                      36.71 |                                         2 |
| UGA | Uganda                         | Africa   |              52 |           80.0 |     65 |                                       0.12 |                                         2 |
| UKR | Ukraine                        | Europe   |            1319 |         1758.7 |     75 |                                       2.96 |                                         2 |
| ARE | United Arab Emirates           | Asia     |            2076 |         2386.2 |     87 |                                      21.56 |                                         2 |
| GBR | United Kingdom                 | Europe   |           52279 |        58740.4 |     89 |                                      78.63 |                                         2 |
| USA | United States                  | Americas |          366614 |       421395.4 |     87 |                                     112.06 |                                         2 |
| URY | Uruguay                        | Americas |             406 |          466.7 |     87 |                                      11.77 |                                         2 |
| UZB | Uzbekistan                     | Asia     |             457 |          634.7 |     72 |                                       1.39 |                                         2 |
| VEN | Venezuela, RB                  | Americas |             165 |          206.2 |     80 |                                       0.57 |                                         2 |
| VNM | Vietnam                        | Asia     |             245 |          429.8 |     57 |                                       0.26 |                                         2 |
| PSE | West Bank and Gaza             | Asia     |             254 |          317.5 |     80 |                                       5.56 |                                         2 |
| ZMB | Zambia                         | Africa   |              39 |           67.2 |     58 |                                       0.22 |                                         2 |
| ZWE | Zimbabwe                       | Africa   |              10 |           12.0 |     83 |                                       0.07 |                                         2 |
