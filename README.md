
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
                  `Adjusted cumulative incidence per 100,000` = round(adj / pop * 100000,digits = 2)) %>%
  dplyr::select(ISO, Country = country, Region,`Confirmed cases`, `Adjusted cases`, `TB CDR`, `Confirmed cumulative incidence per 100,000`,`Adjusted cumulative incidence per 100,000`)
write_csv(out, 'adjustments.csv')
knitr::kable(out)
```

| ISO | Country                        | Region   | Confirmed cases | Adjusted cases | TB CDR | Confirmed cumulative incidence per 100,000 | Adjusted cumulative incidence per 100,000 |
| :-- | :----------------------------- | :------- | --------------: | -------------: | -----: | -----------------------------------------: | ----------------------------------------: |
| AFG | Afghanistan                    | Asia     |             367 |          531.9 |     69 |                                       0.99 |                                      1.43 |
| ALB | Albania                        | Europe   |             377 |          433.3 |     87 |                                      13.15 |                                     15.12 |
| DZA | Algeria                        | Africa   |            1423 |         1778.8 |     80 |                                       3.37 |                                      4.21 |
| AND | Andorra                        | Europe   |             525 |          603.4 |     87 |                                     681.77 |                                    783.64 |
| AGO | Angola                         | Africa   |              16 |           26.2 |     61 |                                       0.05 |                                      0.09 |
| ATG | Antigua and Barbuda            | Americas |              15 |           17.2 |     87 |                                      15.58 |                                     17.91 |
| ARG | Argentina                      | Americas |            1554 |         1786.2 |     87 |                                       3.49 |                                      4.01 |
| ARM | Armenia                        | Asia     |             833 |         1041.2 |     80 |                                      28.22 |                                     35.28 |
| AUT | Austria                        | Europe   |           12297 |        16617.6 |     74 |                                     139.00 |                                    187.83 |
| AZE | Azerbaijan                     | Asia     |             641 |          801.2 |     80 |                                       6.45 |                                      8.06 |
| BHS | Bahamas, The                   | Americas |              29 |           33.3 |     87 |                                       7.52 |                                      8.64 |
| BHR | Bahrain                        | Asia     |             756 |          869.0 |     87 |                                      48.17 |                                     55.37 |
| BGD | Bangladesh                     | Asia     |             123 |          164.0 |     75 |                                       0.08 |                                      0.10 |
| BRB | Barbados                       | Americas |              60 |           69.0 |     87 |                                      20.93 |                                     24.06 |
| BLR | Belarus                        | Europe   |             700 |          875.0 |     80 |                                       7.38 |                                      9.22 |
| BEL | Belgium                        | Europe   |           20814 |        23652.3 |     88 |                                     182.23 |                                    207.08 |
| BLZ | Belize                         | Americas |               7 |            8.0 |     87 |                                       1.83 |                                      2.10 |
| BEN | Benin                          | Africa   |              26 |           41.9 |     62 |                                       0.23 |                                      0.37 |
| BTN | Bhutan                         | Asia     |               5 |            6.2 |     80 |                                       0.66 |                                      0.83 |
| BOL | Bolivia                        | Americas |             183 |          295.2 |     62 |                                       1.61 |                                      2.60 |
| BIH | Bosnia and Herzegovina         | Europe   |             674 |          842.5 |     80 |                                      20.28 |                                     25.35 |
| BWA | Botswana                       | Africa   |               6 |           10.2 |     59 |                                       0.27 |                                      0.45 |
| BRA | Brazil                         | Americas |           12161 |        13978.2 |     87 |                                       5.81 |                                      6.67 |
| BRN | Brunei Darussalam              | Asia     |             135 |          155.2 |     87 |                                      31.47 |                                     36.17 |
| BGR | Bulgaria                       | Europe   |             549 |          669.5 |     82 |                                       7.82 |                                      9.53 |
| BFA | Burkina Faso                   | Africa   |             364 |          577.8 |     63 |                                       1.84 |                                      2.93 |
| BDI | Burundi                        | Africa   |               3 |            5.2 |     58 |                                       0.03 |                                      0.05 |
| CPV | Cabo Verde                     | Africa   |               7 |            8.8 |     80 |                                       1.29 |                                      1.61 |
| KHM | Cambodia                       | Asia     |             114 |          196.6 |     58 |                                       0.70 |                                      1.21 |
| CMR | Cameroon                       | Africa   |             658 |         1316.0 |     50 |                                       2.61 |                                      5.22 |
| CAN | Canada                         | Americas |           16563 |        19037.9 |     87 |                                      44.69 |                                     51.37 |
| CAF | Central African Republic       | Africa   |               8 |           18.6 |     43 |                                       0.17 |                                      0.40 |
| TCD | Chad                           | Africa   |               9 |           15.3 |     59 |                                       0.06 |                                      0.10 |
| CHL | Chile                          | Americas |            4815 |         5534.5 |     87 |                                      25.71 |                                     29.55 |
| CHN | China                          | Asia     |           82665 |        89853.3 |     92 |                                       5.94 |                                      6.45 |
| COL | Colombia                       | Americas |            1579 |         1973.8 |     80 |                                       3.18 |                                      3.98 |
| COD | Congo, Dem. Rep.               | Africa   |             161 |          255.6 |     63 |                                       0.19 |                                      0.30 |
| COG | Congo, Rep.                    | Africa   |              45 |           83.3 |     54 |                                       0.86 |                                      1.59 |
| CRI | Costa Rica                     | Americas |             467 |          583.8 |     80 |                                       9.34 |                                     11.68 |
| CIV | Cote d’Ivoire                  | Africa   |             323 |          547.5 |     59 |                                       1.29 |                                      2.18 |
| HRV | Croatia                        | Europe   |            1222 |         1222.0 |    100 |                                      29.88 |                                     29.88 |
| CUB | Cuba                           | Americas |             350 |          402.3 |     87 |                                       3.09 |                                      3.55 |
| CYP | Cyprus                         | Asia     |             465 |          588.6 |     79 |                                      39.10 |                                     49.49 |
| CZE | Czech Republic                 | Europe   |            4822 |         6429.3 |     75 |                                      45.38 |                                     60.51 |
| DNK | Denmark                        | Europe   |            4875 |         5603.4 |     87 |                                      84.09 |                                     96.65 |
| DJI | Djibouti                       | Africa   |              90 |          112.5 |     80 |                                       9.39 |                                     11.73 |
| DMA | Dominica                       | Americas |              15 |           17.2 |     87 |                                      20.94 |                                     24.07 |
| DOM | Dominican Republic             | Americas |            1828 |         2285.0 |     80 |                                      17.20 |                                     21.50 |
| ECU | Ecuador                        | Americas |            3747 |         4683.8 |     80 |                                      21.93 |                                     27.42 |
| EGY | Egypt, Arab Rep.               | Africa   |            1322 |         1944.1 |     68 |                                       1.34 |                                      1.98 |
| SLV | El Salvador                    | Americas |              69 |           86.2 |     80 |                                       1.07 |                                      1.34 |
| GNQ | Equatorial Guinea              | Africa   |              16 |           32.0 |     50 |                                       1.22 |                                      2.44 |
| EST | Estonia                        | Europe   |            1108 |         1273.6 |     87 |                                      83.88 |                                     96.42 |
| ETH | Ethiopia                       | Africa   |              44 |           63.8 |     69 |                                       0.04 |                                      0.06 |
| FIN | Finland                        | Europe   |            2176 |         2472.7 |     88 |                                      39.43 |                                     44.81 |
| FRA | France                         | Europe   |           98963 |       119232.5 |     83 |                                     147.73 |                                    177.99 |
| GAB | Gabon                          | Africa   |              24 |           47.1 |     51 |                                       1.13 |                                      2.22 |
| GMB | Gambia, The                    | Africa   |               4 |            6.7 |     60 |                                       0.18 |                                      0.29 |
| GEO | Georgia                        | Asia     |             188 |          257.5 |     73 |                                       5.04 |                                      6.90 |
| DEU | Germany                        | Europe   |          103374 |       118820.7 |     87 |                                     124.66 |                                    143.28 |
| GHA | Ghana                          | Africa   |             214 |          668.8 |     32 |                                       0.72 |                                      2.25 |
| GRC | Greece                         | Europe   |            1755 |         2040.7 |     86 |                                      16.36 |                                     19.02 |
| GRD | Grenada                        | Americas |              12 |           13.8 |     87 |                                      10.77 |                                     12.38 |
| GTM | Guatemala                      | Americas |              70 |           87.5 |     80 |                                       0.41 |                                      0.51 |
| GIN | Guinea                         | Africa   |             128 |          196.9 |     65 |                                       1.03 |                                      1.59 |
| GNB | Guinea-Bissau                  | Africa   |              18 |           60.0 |     30 |                                       0.96 |                                      3.20 |
| GUY | Guyana                         | Americas |              31 |           38.8 |     80 |                                       3.98 |                                      4.97 |
| HTI | Haiti                          | Americas |              24 |           34.8 |     69 |                                       0.22 |                                      0.31 |
| HND | Honduras                       | Americas |             298 |          372.5 |     80 |                                       3.11 |                                      3.89 |
| HUN | Hungary                        | Europe   |             744 |          767.0 |     97 |                                       7.62 |                                      7.85 |
| ISL | Iceland                        | Europe   |            1562 |         1795.4 |     87 |                                     441.77 |                                    507.79 |
| IND | India                          | Asia     |            4778 |         6456.8 |     74 |                                       0.35 |                                      0.48 |
| IDN | Indonesia                      | Asia     |            2491 |         3717.9 |     67 |                                       0.93 |                                      1.39 |
| IRN | Iran, Islamic Rep.             | Asia     |           60500 |        75625.0 |     80 |                                      73.96 |                                     92.45 |
| IRQ | Iraq                           | Asia     |            1031 |         2343.2 |     44 |                                       2.68 |                                      6.10 |
| IRL | Ireland                        | Europe   |            5364 |         6165.5 |     87 |                                     110.52 |                                    127.03 |
| ISR | Israel                         | Asia     |            8904 |        10234.5 |     87 |                                     100.23 |                                    115.20 |
| ITA | Italy                          | Europe   |          132547 |       150621.6 |     88 |                                     219.34 |                                    249.24 |
| JAM | Jamaica                        | Americas |              58 |           72.5 |     80 |                                       1.98 |                                      2.47 |
| JPN | Japan                          | Asia     |            3654 |         4200.0 |     87 |                                       2.89 |                                      3.32 |
| JOR | Jordan                         | Asia     |             349 |          436.2 |     80 |                                       3.51 |                                      4.38 |
| KAZ | Kazakhstan                     | Asia     |             662 |          662.0 |    100 |                                       3.62 |                                      3.62 |
| KEN | Kenya                          | Africa   |             158 |          250.8 |     63 |                                       0.31 |                                      0.49 |
| KOR | Korea, Rep.                    | Asia     |           10284 |        10940.4 |     94 |                                      19.92 |                                     21.19 |
| KWT | Kuwait                         | Asia     |             665 |          764.4 |     87 |                                      16.07 |                                     18.47 |
| KGZ | Kyrgyz Republic                | Asia     |             216 |          248.3 |     87 |                                       3.42 |                                      3.93 |
| LAO | Lao PDR                        | Asia     |              12 |           21.1 |     57 |                                       0.17 |                                      0.30 |
| LVA | Latvia                         | Europe   |             542 |             NA |     NA |                                      28.13 |                                        NA |
| LBN | Lebanon                        | Asia     |             541 |          621.8 |     87 |                                       7.90 |                                      9.08 |
| LBR | Liberia                        | Africa   |              14 |           26.4 |     53 |                                       0.29 |                                      0.55 |
| LBY | Libya                          | Africa   |              19 |           27.9 |     68 |                                       0.28 |                                      0.42 |
| LIE | Liechtenstein                  | Europe   |              77 |             NA |     NA |                                     203.11 |                                        NA |
| LTU | Lithuania                      | Europe   |             843 |          969.0 |     87 |                                      30.22 |                                     34.74 |
| LUX | Luxembourg                     | Europe   |            2843 |         3267.8 |     87 |                                     467.81 |                                    537.71 |
| MDG | Madagascar                     | Africa   |              82 |          149.1 |     55 |                                       0.31 |                                      0.57 |
| MWI | Malawi                         | Africa   |               5 |           10.4 |     48 |                                       0.03 |                                      0.06 |
| MYS | Malaysia                       | Asia     |            3793 |         4359.8 |     87 |                                      12.03 |                                     13.83 |
| MDV | Maldives                       | Asia     |              19 |           23.8 |     80 |                                       3.68 |                                      4.61 |
| MLI | Mali                           | Africa   |              47 |           69.1 |     68 |                                       0.25 |                                      0.36 |
| MLT | Malta                          | Europe   |             241 |          262.0 |     92 |                                      49.84 |                                     54.18 |
| MRT | Mauritania                     | Africa   |               6 |           10.2 |     59 |                                       0.14 |                                      0.23 |
| MUS | Mauritius                      | Africa   |             244 |          305.0 |     80 |                                      19.28 |                                     24.10 |
| MEX | Mexico                         | Americas |            2143 |         2678.8 |     80 |                                       1.70 |                                      2.12 |
| MDA | Moldova                        | Europe   |             965 |         1109.2 |     87 |                                      27.21 |                                     31.28 |
| MCO | Monaco                         | Europe   |              77 |             NA |     NA |                                     199.06 |                                        NA |
| MNG | Mongolia                       | Asia     |              15 |           51.7 |     29 |                                       0.47 |                                      1.63 |
| MNE | Montenegro                     | Europe   |             233 |          267.8 |     87 |                                      37.44 |                                     43.03 |
| MAR | Morocco                        | Africa   |            1120 |         1287.4 |     87 |                                       3.11 |                                      3.57 |
| MOZ | Mozambique                     | Africa   |              10 |           17.5 |     57 |                                       0.03 |                                      0.06 |
| MMR | Myanmar                        | Asia     |              22 |           28.9 |     76 |                                       0.04 |                                      0.05 |
| NAM | Namibia                        | Africa   |              16 |           26.2 |     61 |                                       0.65 |                                      1.07 |
| NPL | Nepal                          | Asia     |               9 |           12.0 |     75 |                                       0.03 |                                      0.04 |
| NLD | Netherlands                    | Europe   |           18926 |        21754.0 |     87 |                                     109.84 |                                    126.25 |
| NIC | Nicaragua                      | Americas |               6 |            7.5 |     80 |                                       0.09 |                                      0.12 |
| NER | Niger                          | Africa   |             253 |          460.0 |     55 |                                       1.13 |                                      2.05 |
| NGA | Nigeria                        | Africa   |             238 |          991.7 |     24 |                                       0.12 |                                      0.51 |
| MKD | North Macedonia                | Europe   |             570 |          712.5 |     80 |                                      27.36 |                                     34.21 |
| NOR | Norway                         | Europe   |            5865 |         6741.4 |     87 |                                     110.36 |                                    126.85 |
| OMN | Oman                           | Asia     |             331 |          380.5 |     87 |                                       6.85 |                                      7.88 |
| PAK | Pakistan                       | Asia     |            3766 |         5884.4 |     64 |                                       1.77 |                                      2.77 |
| PAN | Panama                         | Americas |            1988 |         2485.0 |     80 |                                      47.60 |                                     59.49 |
| PRY | Paraguay                       | Americas |             113 |          129.9 |     87 |                                       1.62 |                                      1.87 |
| PER | Peru                           | Americas |            2561 |         3201.2 |     80 |                                       8.01 |                                     10.01 |
| PHL | Philippines                    | Asia     |            3660 |         5809.5 |     63 |                                       3.43 |                                      5.45 |
| POL | Poland                         | Europe   |            4413 |         5072.4 |     87 |                                      11.62 |                                     13.36 |
| PRT | Portugal                       | Europe   |           11730 |        13482.8 |     87 |                                     114.09 |                                    131.13 |
| QAT | Qatar                          | Asia     |            1832 |         2105.7 |     87 |                                      65.86 |                                     75.70 |
| ROU | Romania                        | Europe   |            4057 |         4663.2 |     87 |                                      20.83 |                                     23.95 |
| RUS | Russian Federation             | Europe   |            6343 |         6407.1 |     99 |                                       4.39 |                                      4.43 |
| RWA | Rwanda                         | Africa   |             105 |          131.2 |     80 |                                       0.85 |                                      1.07 |
| SMR | San Marino                     | Europe   |             266 |             NA |     NA |                                     787.33 |                                        NA |
| STP | Sao Tome and Principe          | Africa   |               4 |            7.0 |     57 |                                       1.90 |                                      3.33 |
| SAU | Saudi Arabia                   | Asia     |            2605 |         2994.3 |     87 |                                       7.73 |                                      8.89 |
| SEN | Senegal                        | Africa   |             226 |          318.3 |     71 |                                       1.43 |                                      2.01 |
| SRB | Serbia                         | Europe   |            2200 |         2528.7 |     87 |                                      31.51 |                                     36.22 |
| SYC | Seychelles                     | Africa   |              11 |           12.6 |     87 |                                      11.37 |                                     13.07 |
| SLE | Sierra Leone                   | Africa   |               6 |            8.0 |     75 |                                       0.08 |                                      0.10 |
| SGP | Singapore                      | Asia     |            1375 |         1580.5 |     87 |                                      24.39 |                                     28.03 |
| SVK | Slovak Republic                | Europe   |             534 |          613.8 |     87 |                                       9.80 |                                     11.27 |
| SVN | Slovenia                       | Europe   |            1021 |         1147.2 |     89 |                                      49.39 |                                     55.49 |
| SOM | Somalia                        | Africa   |               7 |           16.7 |     42 |                                       0.05 |                                      0.11 |
| ZAF | South Africa                   | Africa   |            1686 |         2218.4 |     76 |                                       2.92 |                                      3.84 |
| SSD | South Sudan                    | Africa   |               1 |            1.1 |     91 |                                       0.01 |                                      0.01 |
| ESP | Spain                          | Europe   |          140511 |       140511.0 |    100 |                                     300.73 |                                    300.73 |
| LKA | Sri Lanka                      | Asia     |             178 |          278.1 |     64 |                                       0.82 |                                      1.28 |
| KNA | St. Kitts and Nevis            | Americas |              10 |             NA |     NA |                                      19.07 |                                        NA |
| LCA | St. Lucia                      | Americas |              14 |           16.1 |     87 |                                       7.70 |                                      8.85 |
| VCT | St. Vincent and the Grenadines | Americas |               7 |            8.0 |     87 |                                       6.35 |                                      7.30 |
| SDN | Sudan                          | Africa   |              12 |           17.9 |     67 |                                       0.03 |                                      0.04 |
| SUR | Suriname                       | Americas |              10 |           12.5 |     80 |                                       1.74 |                                      2.17 |
| SWE | Sweden                         | Europe   |            7206 |         8282.8 |     87 |                                      70.76 |                                     81.34 |
| CHE | Switzerland                    | Europe   |           21657 |        24893.1 |     87 |                                     254.29 |                                    292.29 |
| SYR | Syrian Arab Republic           | Asia     |              19 |           23.8 |     80 |                                       0.11 |                                      0.14 |
| TZA | Tanzania                       | Africa   |              24 |           45.3 |     53 |                                       0.04 |                                      0.08 |
| THA | Thailand                       | Asia     |            2220 |         2775.0 |     80 |                                       3.20 |                                      4.00 |
| TLS | Timor-Leste                    | Asia     |               1 |            1.7 |     60 |                                       0.08 |                                      0.13 |
| TGO | Togo                           | Africa   |              58 |           68.2 |     85 |                                       0.74 |                                      0.86 |
| TTO | Trinidad and Tobago            | Americas |             105 |          120.7 |     87 |                                       7.55 |                                      8.68 |
| TUN | Tunisia                        | Africa   |             596 |          745.0 |     80 |                                       5.15 |                                      6.44 |
| TUR | Turkey                         | Asia     |           30217 |        34732.2 |     87 |                                      36.71 |                                     42.19 |
| UGA | Uganda                         | Africa   |              52 |           80.0 |     65 |                                       0.12 |                                      0.19 |
| UKR | Ukraine                        | Europe   |            1319 |         1758.7 |     75 |                                       2.96 |                                      3.94 |
| ARE | United Arab Emirates           | Asia     |            2076 |         2386.2 |     87 |                                      21.56 |                                     24.78 |
| GBR | United Kingdom                 | Europe   |           52279 |        58740.4 |     89 |                                      78.63 |                                     88.35 |
| USA | United States                  | Americas |          366614 |       421395.4 |     87 |                                     112.06 |                                    128.80 |
| URY | Uruguay                        | Americas |             406 |          466.7 |     87 |                                      11.77 |                                     13.53 |
| UZB | Uzbekistan                     | Asia     |             457 |          634.7 |     72 |                                       1.39 |                                      1.93 |
| VEN | Venezuela, RB                  | Americas |             165 |          206.2 |     80 |                                       0.57 |                                      0.71 |
| VNM | Vietnam                        | Asia     |             245 |          429.8 |     57 |                                       0.26 |                                      0.45 |
| PSE | West Bank and Gaza             | Asia     |             254 |          317.5 |     80 |                                       5.56 |                                      6.95 |
| ZMB | Zambia                         | Africa   |              39 |           67.2 |     58 |                                       0.22 |                                      0.39 |
| ZWE | Zimbabwe                       | Africa   |              10 |           12.0 |     83 |                                       0.07 |                                      0.08 |
