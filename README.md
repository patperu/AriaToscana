# AriaToscana

[![Build Status](https://travis-ci.org/patperu/AriaToscana.png?branch=master)](https://travis-ci.org/patperu/AriaToscana)

This package contains data of various pollutants from Tuscany over the period 2008-2014. 

This is not an offical dataset. 

The source are the CSV-Files from [ARPAT Toscana](http://www.arpat.toscana.it/datiemappe/dati/qualita-dellaria-dati-orari).

The dataset is split in two variables:

* `at_inq`: Hourly values of various pollutants, 2008-2014
* `at_pm`:  Daily values of Particulate Matter (PM10 and PM2.5), 2008-2014

# Issues:

Some data was excluded: 

* `GR_DATI_QA_2011_CSV.zip` - file `GR-SONNINO_1_2011.csv` has zero values
* No PTS-data for `LI-GIARDINI-PUBBLICI`
* I guess that the spelling for some pollutants names (`parameter`) are not always the same over the years.

# Analysis

The directory [report](https://github.com/patperu/AriaToscana/tree/master/report) contains a first analysis.

