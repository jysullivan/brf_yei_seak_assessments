

# BRF assessment input 

## Length comp data

Two fleets here! Decision 2020-04-29 to combine Sport and Commercial jig (project 4) based on similarities in length distributions. Jig and sport select for smaller fish compared to commercial longline. This may be due to fishing in different habitat/depths. Sport and jig nearshore, shallower (and directed); Longline deeper and bycatch when targeting DSR. 

Length data is only for females because BRF have sexually dimorphic growth.

Lengths.csv = Sport and Commercial jig compositions for years with n >= 50s
Lengths2.csv = Commercial longline compositions for years with n >= 50s

## Catches

Catch is in metric tons. 

Sport catch and releases (with release mortalty) combined. Lots of documentation in sport_rf_seak/r/catch_conversion.R and in Howard et al. 2020 (RIR for sportfish reconstruction project)

Commercial: straight from fish ticket data. Work needs to be done to improve catch reconstruction and accounting for at-sea discards.

Three files:

Catches_total.csv - combined sport (directed), comm jig (directed), and comm longline (bycatch)
Catches.csv - directed catch, combined sport and comm jig (based on length comp definitions)
Catches2.csv - bycatch, longline DSR (based on length comp definitions)