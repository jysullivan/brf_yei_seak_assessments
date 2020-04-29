
# YEI assessment input 

## Length compositions

Decision 2020-04-29 to combine Sport and Commercial (only project codes 2=directed DSR, 21=bycatch in halibut longline, and 8=special project in 1984/5 in SSEI https://www.adfg.alaska.gov/FedAidpdfs/afrbIL.258.pdf) YE based on major overlap in length distributions (the two fleets
select for similar lengths in the population). The commercial project codes were combined because of overlap in length distributions and sample size limitations.

Length data is combined for all sexes (even unknown) because growth is similar between male and female YE.

Lengths.csv = Sport and Commercial compositions for years with n >= 50s

## Catches

Catch is in metric tons.

Sport catch and releases (with release mortalty) combined. Lots of documentation in sport_rf_seak/r/catch_conversion.R and in Howard et al. 2020 (RIR for sportfish reconstruction project)

###Commercial catch reconstruction:

**1888-1984:**
This analysis assumes that pre-1985, the bycatch of YEI in the halibut fishery was 5% of the IPHC 2C area catch. 5% is based on expert opinion and K Wood's GIS work looking at bycatch rates over space and time and habitat suitability for YEI. It assumes the proportion of halibut catch in inside and outside waters in constant over time, and that bycatch rates have been constant in the halibut fishery (they may have been much higher in early years if YE were highly abundant).

**1985-2018:**
Primarily fish ticket data, however at-sea discards have been accounted for in the following ways: 
1.  Mandatory retention for YEI went into regulation in 2001. For YEO even though there is mandatory retention, the byacth is inflated by 10-15% to account for illegal catch or inaccurate reporting. We have never done this for YEI. In order to account for this in inside waters, inflate bycatch by *15% in 2001-present*. 
2.  For *1985-2000 inflate by 25%* based on expert opinion (both from biometrics and management biologists).