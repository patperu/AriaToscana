# Aria Toscana
Patrick Hausmann  
`r Sys.Date()`  

<!-- README.md is generated from README.Rmd. Please edit that file -->




```r

library('ggplot2')
library('tidyr')
library('dplyr')
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library('AriaToscana')

options(stringsAsFactors = FALSE,
        knitr.table.format = 'markdown')
```


```r

addmargins(table(at_inq$parameter, at_inq$year)) %>% 
  knitr::kable(format.args = list(big.mark = '.', decimal.mark = ','))
```



|          |      2008|      2009|      2010|      2011|      2012|      2013|      2014|      2015|        Sum|
|:---------|---------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|----------:|
|BENZENE   |    61.488|    43.800|    43.800|    35.040|    26.352|         0|    52.560|    70.080|    333.120|
|CH4       |    43.920|    43.800|    52.560|    26.280|    17.568|         0|         0|         0|    184.128|
|CO        |   342.576|   341.640|   341.542|   245.280|   105.408|    61.320|    70.080|    78.840|  1.586.686|
|EBENZENE  |    52.704|    43.800|    43.800|    35.040|    26.352|         0|    52.560|    70.080|    324.336|
|H2S       |    43.920|    43.800|    43.800|    26.280|    17.568|    17.520|    17.520|    17.520|    227.928|
|HCL       |     8.784|     8.760|     8.760|         0|         0|         0|         0|         0|     26.304|
|M-XYLENE  |    43.920|    43.800|    43.800|    35.040|    26.352|         0|         0|         0|    192.912|
|MP-XILENE |     8.784|         0|         0|         0|         0|         0|         0|         0|      8.784|
|MP-XYLENE |         0|         0|         0|         0|         0|         0|    26.280|    70.080|     96.360|
|N-EPTANO  |    17.568|    17.520|    17.520|     8.760|     8.784|         0|         0|         0|     70.152|
|N-ESANO   |    17.568|    17.520|    17.520|     8.760|     8.784|         0|         0|         0|     70.152|
|N-OTTANO  |    17.568|    17.520|    17.520|     8.760|     8.784|         0|         0|         0|     70.152|
|NH3       |    17.568|    17.520|    17.520|     8.760|         0|         0|         0|         0|     61.368|
|NMHC      |    43.920|    43.800|    52.560|    26.280|    17.568|         0|         0|         0|    184.128|
|NO        |   518.256|   534.360|   534.360|   402.960|   377.712|   315.360|   332.880|   332.880|  3.348.768|
|NO2       |   518.256|   534.360|   534.360|   402.960|   377.712|   315.360|   332.880|   332.880|  3.348.768|
|NOX       |   518.256|   534.360|   534.360|   402.960|   377.712|   315.360|   332.880|   332.880|  3.348.768|
|O-XYLENE  |    43.920|    43.800|    43.800|    35.040|    26.352|         0|    52.560|    70.080|    315.552|
|O3        |   281.088|   289.080|   262.796|   227.760|   166.896|   113.880|   105.120|    96.360|  1.542.980|
|P-XYLENE  |    52.704|    43.800|    43.800|    35.040|    26.352|         0|    26.280|         0|    227.976|
|SO2       |   158.112|   166.440|   148.920|   105.120|    52.704|    26.280|    35.040|    26.280|    718.896|
|TN        |    17.568|    17.520|    17.520|     8.760|         0|         0|         0|         0|     61.368|
|TNX       |    17.568|    17.520|    17.520|     8.760|         0|         0|         0|         0|     61.368|
|TOLUENE   |    61.488|    43.800|    43.800|    35.040|    26.352|         0|    52.560|    70.080|    333.120|
|Sum       | 2.907.504| 2.908.320| 2.881.938| 2.128.680| 1.695.312| 1.165.080| 1.489.200| 1.568.040| 16.744.074|


```r

addmargins(table(at_pm$parameter, at_pm$year)) %>% 
  knitr::kable(format.args = list(big.mark = '.', decimal.mark = ','))
```



|      |   2008|   2009|   2010|   2011|   2012|   2013|   2014|   2015|     Sum|
|:-----|------:|------:|------:|------:|------:|------:|------:|------:|-------:|
|PM10  | 18.300| 20.805| 20.440| 15.695| 13.176| 12.410| 13.140| 12.775| 126.741|
|PM2.5 |  1.464|  2.555|  4.380|  4.380|  4.392|  5.110|  5.475|  6.205|  33.961|
|Sum   | 19.764| 23.360| 24.820| 20.075| 17.568| 17.520| 18.615| 18.980| 160.702|



```r

# If 4 or more measures are 'NA', then 'NA' for the whole time frame (br)
ff <- function(L) ifelse(sum(is.na(L)) >= 4, NA, mean(L, na.rm = TRUE))

x1 <- tbl_df(at_inq) %>%
       filter(year %in% c(2008, 2012, 2014, 2015) & parameter %in% c("O3", "NO2", "CO")) %>%
       mutate(br = factor(cut(hour, breaks = 4), 
                          labels = c("00-06h", "06-12h", 
                                     "12-18h", "18-24h")),
              provincia = substr(stazione, 1, 2)) %>%
       group_by(provincia, stazione, year, month, day, parameter, br) %>%
       summarise(val_na   = sum(is.na(value)),
                 val_mean = ff(value))
```


```r

p1 <- ggplot(subset(x1, subset = year == 2015 & parameter == "NO2" & provincia == "FI"), 
             aes(as.factor(month), val_mean, group = year, color = year))
p1 <- p1 + geom_point(shape = 1, col ="blue") 
p1 <- p1 + facet_grid(br ~ stazione) 
p1 <- p1 + stat_smooth(aes(group = 1), col = "red")
p1 <- p1 + labs(title = "Biossido di azoto NO2, 2015, Firenze (FI)")
p1 <- p1 + labs(x = "Mese", y = expression(paste(Biossido~di~azoto~(NO2)~µg/m^3)))
p1
#> Warning: Removed 192 rows containing non-finite values (stat_smooth).
#> Warning: Removed 192 rows containing missing values (geom_point).
```

<img src="Report_files/figure-html/plot_NO2_FI-1.png" width="1122.24" />


```r

p2 <- ggplot(subset(x1, subset = 
                         stazione %in% c("LU-CAPANNORI", "FI-MOSSE", 
                                         "FI-GRAMSCI", "PO-ROMA") & 
                         parameter == "NO2"), aes(as.factor(month), val_mean))
p2 <- p2 + facet_grid(br ~ stazione)
p2 <- p2 + stat_smooth(aes(group = as.factor(year), color = as.factor(year)), se = FALSE)
p2 <- p2 + labs(title = "Biossido di azoto NO2, 2008/2012/2014/2015")
p2 <- p2 + labs(x = "Mese", 
                y = expression(paste(Biossido~di~azoto~(NO2)~µg/m^3)))
p2
#> Warning: Removed 737 rows containing non-finite values (stat_smooth).
```

<img src="Report_files/figure-html/plot_2008_2015-1.png" width="1122.24" />

## PM10

This is table 4.1.2. on page 18 from the [last report 2015](http://www.arpat.toscana.it/documentazione/catalogo-pubblicazioni-arpat/relazione-annuale-sullo-stato-della-qualita-dellaria-nella-regione-toscana-anno-2014/attachment_download/pubblicazione)


```r

m <- tbl_df(at_pm) %>%
       filter(parameter == "PM10" & valid == 1) %>%
       group_by(stazione, year) %>%
       mutate(days_gt50 = cumsum(value > 50)) %>%
       select(stazione, year, days_gt50) %>%
       top_n(1, days_gt50) # %>%
       #distinct(stazione, year) %>%
       #ungroup() %>%
       #arrange(stazione, year)

spread(unique(m), year, days_gt50, fill = ".") %>% knitr::kable()
```



|stazione                      |2008 |2009 |2010 |2011 |2012 |2013 |2014 |2015 |
|:-----------------------------|:----|:----|:----|:----|:----|:----|:----|:----|
|AR-ACROPOLI                   |.    |.    |.    |.    |.    |12   |9    |18   |
|AR-CASA-STABBI                |1    |0    |0    |0    |1    |0    |4    |0    |
|AR-PIAZZA-REPUBBLICA          |17   |15   |20   |.    |.    |.    |.    |.    |
|AR-REPUBBLICA                 |.    |.    |.    |34   |29   |26   |31   |34   |
|AR-VIA-FIORENTINA             |44   |36   |26   |.    |.    |.    |.    |.    |
|FI-BASSI                      |33   |23   |13   |19   |11   |17   |4    |9    |
|FI-BOBOLI                     |19   |13   |10   |17   |7    |18   |3    |5    |
|FI-CALENZANO-BOCCACCIO        |56   |20   |.    |.    |.    |.    |.    |.    |
|FI-CALENZANO-GIOVANNI         |.    |64   |52   |.    |.    |.    |.    |.    |
|FI-CAMPI-BISENZIO             |42   |49   |45   |.    |.    |.    |.    |.    |
|FI-EMPOLI-RIDOLFI             |16   |23   |48   |.    |.    |.    |.    |.    |
|FI-GRAMSCI                    |98   |88   |65   |55   |46   |38   |19   |26   |
|FI-GREVE                      |.    |.    |.    |17   |3    |0    |.    |.    |
|FI-GREVE-PASSO-PECORAI        |12   |1    |10   |.    |.    |.    |.    |.    |
|FI-INCISA                     |.    |5    |18   |48   |45   |.    |.    |.    |
|FI-MONTELUPO-VIA-ASIA         |32   |4    |5    |0    |.    |.    |.    |.    |
|FI-MOSSE                      |88   |82   |66   |59   |69   |46   |11   |14   |
|FI-PONTASSIEVE                |.    |.    |.    |1    |0    |2    |1    |0    |
|FI-PONTASSIEVE-CURIEL         |.    |5    |2    |.    |.    |.    |.    |.    |
|FI-SCANDICCI                  |.    |.    |.    |37   |23   |22   |5    |10   |
|FI-SCANDICCI-BUOZZI           |49   |48   |38   |.    |.    |.    |.    |.    |
|FI-SESTO                      |.    |39   |26   |.    |.    |.    |.    |.    |
|FI-SIGNA                      |120  |85   |64   |.    |.    |.    |26   |33   |
|FI-SIGNA-ROMA                 |.    |.    |.    |36   |46   |30   |.    |.    |
|GR-SONNINO                    |.    |.    |.    |.    |5    |.    |.    |.    |
|GR-URSS                       |.    |.    |.    |.    |0    |0    |3    |0    |
|GR-VIA-URSS                   |3    |4    |0    |.    |.    |.    |.    |.    |
|GR-VIALE-SONNINO              |29   |17   |29   |.    |.    |.    |.    |.    |
|LI-CAPPIELLO                  |.    |.    |.    |.    |.    |.    |0    |0    |
|LI-CARDUCCI                   |.    |.    |.    |7    |4    |1    |0    |2    |
|LI-COTONE                     |.    |.    |.    |14   |6    |8    |8    |0    |
|LI-COTONE-RETE-LI             |29   |21   |27   |.    |.    |.    |.    |.    |
|LI-GIARDINI-PUBBLICI          |.    |.    |.    |.    |.    |44   |3    |.    |
|LI-GOBETTI                    |.    |.    |.    |2    |0    |3    |0    |.    |
|LI-LAPIRA                     |.    |.    |.    |.    |.    |.    |0    |0    |
|LI-MAUROGORDATO               |.    |.    |.    |0    |0    |.    |.    |.    |
|LI-PIOMBINO-PARCO-VIII-MARZO  |.    |.    |.    |.    |.    |.    |2    |0    |
|LI-POGGIO-SAN-ROCCO           |.    |.    |.    |0    |0    |0    |.    |.    |
|LI-POGGIO-SAN-ROCCO-ROSIGNANO |.    |0    |0    |.    |.    |.    |.    |.    |
|LI-VENETO                     |.    |.    |.    |10   |6    |.    |.    |.    |
|LI-VIA-GOBETTI                |10   |1    |0    |.    |.    |.    |.    |.    |
|LI-VIA-VENETO                 |13   |10   |3    |.    |.    |.    |.    |.    |
|LI-VIALE-CARDUCCI             |40   |20   |11   |.    |.    |.    |.    |.    |
|LI-VILLA-MAUROGORDATO         |0    |0    |0    |.    |.    |.    |.    |.    |
|LU-2VIAREGGIO                 |59   |27   |9    |.    |.    |.    |.    |.    |
|LU-CAPANNORI                  |40   |35   |38   |57   |36   |30   |59   |68   |
|LU-FORNOLI                    |.    |.    |.    |.    |.    |.    |.    |30   |
|LU-MICHELETTO                 |41   |50   |48   |65   |54   |41   |34   |52   |
|LU-PORCARI                    |42   |41   |41   |54   |37   |27   |23   |.    |
|LU-SAN-CONCORDIO              |.    |.    |.    |.    |.    |.    |.    |27   |
|LU-TANGENZIALE                |121  |92   |98   |54   |.    |.    |.    |.    |
|LU-VIAREGGIO                  |.    |.    |.    |37   |15   |21   |11   |26   |
|MS-CAPUCCINI                  |.    |0    |4    |1    |.    |.    |.    |.    |
|MS-CARRIONA                   |.    |.    |.    |30   |.    |.    |.    |.    |
|MS-COLOMBAROTTO               |.    |.    |.    |2    |3    |9    |2    |1    |
|MS-GALVANI                    |.    |.    |.    |9    |8    |7    |9    |2    |
|MS-MARINA-VECCHIA             |.    |.    |.    |.    |.    |.    |.    |11   |
|MS-PARCHEGGIO-COLOMBAROTTO    |18   |5    |2    |.    |.    |.    |.    |.    |
|MS-VIA-CARRIONA               |73   |62   |57   |.    |.    |.    |.    |.    |
|MS-VIA-FRASSINA               |211  |166  |161  |51   |.    |.    |.    |.    |
|MS-VIA-GALVANI                |36   |25   |20   |.    |.    |.    |.    |.    |
|PI-BORGHETTO                  |36   |31   |31   |44   |35   |31   |18   |34   |
|PI-CASCINA                    |93   |82   |50   |.    |.    |.    |.    |.    |
|PI-LARI                       |21   |14   |43   |.    |.    |.    |.    |.    |
|PI-MONTECERBOLI               |.    |.    |.    |0    |1    |0    |0    |0    |
|PI-MONTECERBOLI-BIS           |1    |0    |0    |.    |.    |.    |.    |.    |
|PI-NAVACCHIO                  |72   |49   |31   |.    |.    |.    |.    |.    |
|PI-ORATOIO                    |40   |45   |29   |.    |.    |.    |.    |.    |
|PI-PASSI                      |.    |.    |13   |28   |17   |22   |10   |14   |
|PI-PONTEDERA                  |34   |16   |25   |27   |9    |19   |4    |12   |
|PI-SAN-ROMANO                 |60   |38   |28   |18   |.    |.    |.    |.    |
|PI-SANTA-CROCE-COOP           |35   |32   |33   |47   |33   |27   |22   |40   |
|PI-SANTA-CROCE-SERAO          |20   |6    |10   |.    |.    |.    |.    |.    |
|PO-FERRUCCI                   |41   |51   |45   |50   |44   |37   |28   |34   |
|PO-FONTANELLE                 |39   |53   |.    |.    |.    |.    |.    |.    |
|PO-POGGIO-A-CAIANO            |.    |.    |.    |18   |.    |.    |.    |.    |
|PO-ROMA                       |29   |27   |30   |43   |43   |35   |30   |40   |
|PO-STROZZI                    |28   |28   |36   |.    |.    |.    |.    |.    |
|PO-XX-SETTEMBRE               |35   |31   |34   |.    |.    |.    |.    |.    |
|PT-MONT-VIA-MERLINI           |59   |46   |30   |22   |.    |.    |.    |.    |
|PT-MONTALE                    |70   |35   |28   |65   |63   |45   |32   |57   |
|PT-SIGNORELLI                 |.    |12   |19   |25   |22   |28   |12   |15   |
|PT-ZAMENHOF                   |51   |57   |41   |18   |.    |.    |.    |.    |
|SI-BRACCI                     |.    |.    |.    |.    |.    |.    |7    |2    |
|SI-DE-AMICIS                  |.    |.    |12   |.    |.    |.    |.    |.    |
|SI-DUE-PONTI                  |.    |.    |.    |35   |34   |25   |15   |.    |
|SI-LARGO-CAMPIDOGLIO          |103  |89   |.    |.    |.    |.    |.    |.    |
|SI-LOC-DUE-PONTI              |5    |0    |4    |.    |.    |.    |.    |.    |
|SI-POGGIBONSI                 |.    |.    |.    |20   |0    |1    |1    |0    |

This is table 4.1.5. on page 24 from the [last report 2015](http://www.arpat.toscana.it/documentazione/catalogo-pubblicazioni-arpat/relazione-annuale-sullo-stato-della-qualita-dellaria-nella-regione-toscana-anno-2014/attachment_download/pubblicazione)


```r

m <- tbl_df(at_pm) %>%
       filter(parameter == "PM10" & valid == 1) %>%
       group_by(stazione, year) %>%
       summarise(val_mean = round(mean(value), 0)) %>%
       select(stazione, year, val_mean) %>%
       ungroup() %>%
       arrange(year, stazione)

spread(m, year, val_mean, fill = ".") %>% knitr::kable()
```



|stazione                      |2008 |2009 |2010 |2011 |2012 |2013 |2014 |2015 |
|:-----------------------------|:----|:----|:----|:----|:----|:----|:----|:----|
|AR-ACROPOLI                   |.    |.    |.    |.    |.    |23   |21   |23   |
|AR-CASA-STABBI                |12   |11   |10   |13   |13   |9    |11   |11   |
|AR-PIAZZA-REPUBBLICA          |32   |30   |27   |.    |.    |.    |.    |.    |
|AR-REPUBBLICA                 |.    |.    |.    |28   |28   |27   |27   |30   |
|AR-VIA-FIORENTINA             |33   |30   |27   |.    |.    |.    |.    |.    |
|FI-BASSI                      |29   |27   |22   |24   |23   |20   |18   |22   |
|FI-BOBOLI                     |25   |25   |23   |26   |23   |20   |19   |22   |
|FI-CALENZANO-BOCCACCIO        |33   |24   |.    |.    |.    |.    |.    |.    |
|FI-CALENZANO-GIOVANNI         |.    |37   |35   |.    |.    |.    |.    |.    |
|FI-CAMPI-BISENZIO             |31   |34   |32   |.    |.    |.    |.    |.    |
|FI-EMPOLI-RIDOLFI             |28   |32   |34   |.    |.    |.    |.    |.    |
|FI-GRAMSCI                    |44   |43   |38   |38   |36   |34   |29   |31   |
|FI-GREVE                      |.    |.    |.    |31   |20   |20   |.    |.    |
|FI-GREVE-PASSO-PECORAI        |24   |23   |27   |.    |.    |.    |.    |.    |
|FI-INCISA                     |.    |21   |23   |32   |33   |.    |.    |.    |
|FI-MONTELUPO-VIA-ASIA         |32   |26   |22   |35   |.    |.    |.    |.    |
|FI-MOSSE                      |42   |43   |39   |38   |39   |30   |23   |24   |
|FI-PONTASSIEVE                |.    |.    |.    |20   |20   |19   |17   |20   |
|FI-PONTASSIEVE-CURIEL         |.    |23   |19   |.    |.    |.    |.    |.    |
|FI-SCANDICCI                  |.    |.    |.    |29   |27   |24   |20   |23   |
|FI-SCANDICCI-BUOZZI           |35   |35   |33   |.    |.    |.    |.    |.    |
|FI-SESTO                      |.    |35   |34   |.    |.    |.    |.    |.    |
|FI-SIGNA                      |48   |41   |36   |.    |.    |.    |25   |26   |
|FI-SIGNA-ROMA                 |.    |.    |.    |48   |30   |30   |.    |.    |
|GR-SONNINO                    |.    |.    |.    |.    |30   |.    |.    |.    |
|GR-URSS                       |.    |.    |.    |.    |19   |17   |17   |17   |
|GR-VIA-URSS                   |22   |23   |18   |.    |.    |.    |.    |.    |
|GR-VIALE-SONNINO              |35   |35   |37   |.    |.    |.    |.    |.    |
|LI-CAPPIELLO                  |.    |.    |.    |.    |.    |.    |17   |18   |
|LI-CARDUCCI                   |.    |.    |.    |28   |27   |23   |23   |25   |
|LI-COTONE                     |.    |.    |.    |27   |25   |23   |21   |18   |
|LI-COTONE-RETE-LI             |31   |29   |27   |.    |.    |.    |.    |.    |
|LI-GIARDINI-PUBBLICI          |.    |.    |.    |.    |.    |36   |30   |.    |
|LI-GOBETTI                    |.    |.    |.    |21   |19   |19   |17   |.    |
|LI-LAPIRA                     |.    |.    |.    |.    |.    |.    |20   |21   |
|LI-MAUROGORDATO               |.    |.    |.    |14   |13   |.    |.    |.    |
|LI-PIOMBINO-PARCO-VIII-MARZO  |.    |.    |.    |.    |.    |.    |18   |19   |
|LI-POGGIO-SAN-ROCCO           |.    |.    |.    |19   |16   |18   |.    |.    |
|LI-POGGIO-SAN-ROCCO-ROSIGNANO |.    |17   |17   |.    |.    |.    |.    |.    |
|LI-VENETO                     |.    |.    |.    |29   |26   |.    |.    |.    |
|LI-VIA-GOBETTI                |26   |21   |19   |.    |.    |.    |.    |.    |
|LI-VIA-VENETO                 |29   |28   |27   |.    |.    |.    |.    |.    |
|LI-VIALE-CARDUCCI             |35   |32   |27   |.    |.    |.    |.    |.    |
|LI-VILLA-MAUROGORDATO         |17   |17   |14   |.    |.    |.    |.    |.    |
|LU-2VIAREGGIO                 |35   |31   |26   |.    |.    |.    |.    |.    |
|LU-CAPANNORI                  |29   |27   |27   |31   |26   |24   |29   |33   |
|LU-FORNOLI                    |.    |.    |.    |.    |.    |.    |.    |25   |
|LU-MICHELETTO                 |31   |33   |31   |33   |32   |29   |28   |32   |
|LU-PORCARI                    |30   |29   |28   |31   |26   |24   |38   |.    |
|LU-SAN-CONCORDIO              |.    |.    |.    |.    |.    |.    |.    |27   |
|LU-TANGENZIALE                |48   |44   |44   |57   |.    |.    |.    |.    |
|LU-VIAREGGIO                  |.    |.    |.    |30   |27   |27   |24   |27   |
|MS-CAPUCCINI                  |.    |19   |18   |18   |.    |.    |.    |.    |
|MS-CARRIONA                   |.    |.    |.    |32   |.    |.    |.    |.    |
|MS-COLOMBAROTTO               |.    |.    |.    |24   |24   |24   |22   |23   |
|MS-GALVANI                    |.    |.    |.    |25   |25   |24   |25   |27   |
|MS-MARINA-VECCHIA             |.    |.    |.    |.    |.    |.    |.    |25   |
|MS-PARCHEGGIO-COLOMBAROTTO    |26   |24   |22   |.    |.    |.    |.    |.    |
|MS-VIA-CARRIONA               |36   |35   |36   |.    |.    |.    |.    |.    |
|MS-VIA-FRASSINA               |66   |55   |51   |45   |.    |.    |.    |.    |
|MS-VIA-GALVANI                |32   |29   |27   |.    |.    |.    |.    |.    |
|PI-BORGHETTO                  |29   |32   |29   |30   |28   |26   |25   |29   |
|PI-CASCINA                    |43   |40   |35   |.    |.    |.    |.    |.    |
|PI-LARI                       |26   |28   |31   |.    |.    |.    |.    |.    |
|PI-MONTECERBOLI               |.    |.    |.    |15   |14   |10   |8    |11   |
|PI-MONTECERBOLI-BIS           |15   |15   |13   |.    |.    |.    |.    |.    |
|PI-NAVACCHIO                  |39   |36   |30   |.    |.    |.    |.    |.    |
|PI-ORATOIO                    |34   |36   |32   |.    |.    |.    |.    |.    |
|PI-PASSI                      |.    |.    |25   |26   |25   |23   |21   |25   |
|PI-PONTEDERA                  |31   |30   |30   |28   |25   |23   |21   |24   |
|PI-SAN-ROMANO                 |36   |33   |29   |30   |.    |.    |.    |.    |
|PI-SANTA-CROCE-COOP           |29   |29   |30   |31   |28   |27   |27   |29   |
|PI-SANTA-CROCE-SERAO          |25   |26   |24   |.    |.    |.    |.    |.    |
|PO-FERRUCCI                   |32   |34   |33   |35   |31   |30   |25   |27   |
|PO-FONTANELLE                 |32   |28   |.    |.    |.    |.    |.    |.    |
|PO-POGGIO-A-CAIANO            |.    |.    |.    |29   |.    |.    |.    |.    |
|PO-ROMA                       |26   |25   |31   |30   |30   |27   |25   |28   |
|PO-STROZZI                    |27   |28   |32   |.    |.    |.    |.    |.    |
|PO-XX-SETTEMBRE               |34   |31   |31   |.    |.    |.    |.    |.    |
|PT-MONT-VIA-MERLINI           |35   |33   |28   |50   |.    |.    |.    |.    |
|PT-MONTALE                    |39   |33   |29   |34   |34   |29   |26   |31   |
|PT-SIGNORELLI                 |.    |37   |26   |25   |24   |23   |21   |23   |
|PT-ZAMENHOF                   |34   |35   |31   |45   |.    |.    |.    |.    |
|SI-BRACCI                     |.    |.    |.    |.    |.    |.    |31   |21   |
|SI-DE-AMICIS                  |.    |.    |29   |.    |.    |.    |.    |.    |
|SI-DUE-PONTI                  |.    |.    |.    |33   |34   |30   |32   |.    |
|SI-LARGO-CAMPIDOGLIO          |45   |44   |.    |.    |.    |.    |.    |.    |
|SI-LOC-DUE-PONTI              |20   |19   |28   |.    |.    |.    |.    |.    |
|SI-POGGIBONSI                 |.    |.    |.    |29   |22   |18   |18   |20   |

## Percent of missing observations per month


```r

x3 <- tbl_df(x1) %>%
       group_by(provincia, stazione, year, month, parameter) %>%
        summarise(pct_na = round((sum(val_na) / (max(day) * 24)) * 100, 1) )

filter(x3, year == 2015 & 
           stazione == "FI-GRAMSCI" & 
           parameter == "NO2") %>% knitr::kable()
```



|provincia |stazione   | year| month|parameter | pct_na|
|:---------|:----------|----:|-----:|:---------|------:|
|FI        |FI-GRAMSCI | 2015|     1|NO2       |   10.3|
|FI        |FI-GRAMSCI | 2015|     2|NO2       |    4.6|
|FI        |FI-GRAMSCI | 2015|     3|NO2       |   11.7|
|FI        |FI-GRAMSCI | 2015|     4|NO2       |    4.2|
|FI        |FI-GRAMSCI | 2015|     5|NO2       |    4.4|
|FI        |FI-GRAMSCI | 2015|     6|NO2       |    4.4|
|FI        |FI-GRAMSCI | 2015|     7|NO2       |    6.0|
|FI        |FI-GRAMSCI | 2015|     8|NO2       |    4.7|
|FI        |FI-GRAMSCI | 2015|     9|NO2       |    4.3|
|FI        |FI-GRAMSCI | 2015|    10|NO2       |    6.9|
|FI        |FI-GRAMSCI | 2015|    11|NO2       |    5.6|
|FI        |FI-GRAMSCI | 2015|    12|NO2       |    9.3|

## Missing PM10

Percent of missing values of PM10 by year and month


```r

x4 <- tbl_df(at_pm) %>%
       group_by(stazione, year, month, parameter) %>%
       summarise(pct_na = round((sum(is.na(value)) /  (max(day))) * 100, 1) ) %>%
       filter(stazione == "FI-GRAMSCI" & parameter == "PM10") %>%
       ungroup() %>%
       select(year, month, pct_na)

spread(x4, year, pct_na) %>% knitr::kable()
```



| month| 2008| 2009| 2010| 2011| 2012| 2013| 2014| 2015|
|-----:|----:|----:|----:|----:|----:|----:|----:|----:|
|     1|  0.0|  0.0|  0.0|  9.7|  0.0|  0.0|  6.5|  6.5|
|     2|  0.0|  7.1|  0.0|  0.0|  0.0|  0.0|  7.1| 14.3|
|     3| 32.3|  9.7|  9.7|  6.5|  0.0|  3.2|  0.0| 25.8|
|     4| 13.3|  0.0|  6.7|  3.3|  3.3| 36.7| 10.0|  0.0|
|     5|  6.5| 19.4|  0.0|  0.0|  0.0|  3.2|  0.0|  9.7|
|     6|  0.0|  3.3|  0.0| 30.0|  0.0|  6.7|  3.3|  0.0|
|     7|  9.7| 16.1| 41.9| 19.4|  0.0| 19.4| 12.9|  0.0|
|     8|  0.0|  0.0|  3.2|  9.7|  3.2|  0.0| 16.1|  3.2|
|     9|  0.0|  3.3|  0.0|  0.0|  0.0|  3.3|  3.3|  6.7|
|    10|  3.2|  0.0|  0.0|  0.0|  0.0|  3.2| 12.9|  6.5|
|    11|  0.0|  6.7|  0.0|  0.0|  0.0|  6.7| 20.0| 30.0|
|    12| 19.4|  0.0| 22.6|  3.2|  3.2| 16.1| 19.4| 12.9|


```r
devtools::session_info()
#> Session info --------------------------------------------------------------
#>  setting  value                                      
#>  version  R version 3.3.0 Patched (2016-05-03 r70575)
#>  system   x86_64, mingw32                            
#>  ui       RTerm                                      
#>  language (EN)                                       
#>  collate  German_Germany.1252                        
#>  tz       Europe/Berlin                              
#>  date     2016-05-28
#> Packages ------------------------------------------------------------------
#>  package     * version     date       source                            
#>  AriaToscana * 0.2.0.9000  2016-05-28 local                             
#>  assertthat    0.1         2013-12-06 CRAN (R 3.3.0)                    
#>  colorspace    1.2-6       2015-03-11 CRAN (R 3.3.0)                    
#>  DBI           0.4-1       2016-05-08 CRAN (R 3.3.0)                    
#>  devtools      1.11.1      2016-04-21 CRAN (R 3.3.0)                    
#>  digest        0.6.9       2016-01-08 CRAN (R 3.3.0)                    
#>  dplyr       * 0.4.3.9001  2016-05-05 Github (hadley/dplyr@3074cf7)     
#>  evaluate      0.9         2016-04-29 CRAN (R 3.3.0)                    
#>  formatR       1.4         2016-05-09 CRAN (R 3.3.0)                    
#>  ggplot2     * 2.1.0       2016-03-01 CRAN (R 3.3.0)                    
#>  gtable        0.2.0       2016-02-26 CRAN (R 3.3.0)                    
#>  highr         0.6         2016-05-09 CRAN (R 3.3.0)                    
#>  htmltools     0.3.5       2016-03-21 CRAN (R 3.3.0)                    
#>  knitr         1.13        2016-05-09 CRAN (R 3.3.0)                    
#>  labeling      0.3         2014-08-23 CRAN (R 3.3.0)                    
#>  lattice       0.20-33     2015-07-14 CRAN (R 3.3.0)                    
#>  lazyeval      0.1.10.9000 2016-05-05 Github (hadley/lazyeval@bce211b)  
#>  magrittr      1.5         2014-11-22 CRAN (R 3.3.0)                    
#>  Matrix        1.2-6       2016-05-02 CRAN (R 3.3.0)                    
#>  memoise       1.0.0       2016-01-29 CRAN (R 3.3.0)                    
#>  mgcv          1.8-12      2016-03-03 CRAN (R 3.3.0)                    
#>  munsell       0.4.3       2016-02-13 CRAN (R 3.3.0)                    
#>  nlme          3.1-128     2016-05-10 CRAN (R 3.3.0)                    
#>  plyr          1.8.3       2015-06-12 CRAN (R 3.3.0)                    
#>  R6            2.1.2       2016-01-26 CRAN (R 3.3.0)                    
#>  Rcpp          0.12.5      2016-05-14 CRAN (R 3.3.0)                    
#>  reshape2      1.4.1       2014-12-06 CRAN (R 3.3.0)                    
#>  rmarkdown     0.9.6.10    2016-05-25 Github (rstudio/rmarkdown@3cf8685)
#>  scales        0.4.0       2016-02-26 CRAN (R 3.3.0)                    
#>  stringi       1.0-1       2015-10-22 CRAN (R 3.3.0)                    
#>  stringr       1.0.0       2015-04-30 CRAN (R 3.3.0)                    
#>  tibble        1.0-1       2016-05-05 Github (hadley/tibble@cb38672)    
#>  tidyr       * 0.4.1       2016-02-05 CRAN (R 3.3.0)                    
#>  withr         1.0.1       2016-02-04 CRAN (R 3.3.0)                    
#>  yaml          2.1.13      2014-06-12 CRAN (R 3.3.0)
```

