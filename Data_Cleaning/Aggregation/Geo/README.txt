./Uber_Chicago/Data_Cleaning/Aggregation/Geo
This folder contains codes to aggregate trips level data.
Aggregation is two steps; (1) by area (Census tract to Community area) and (2) by week.

./Slots
Contains functions that creates slots (base unit observation). Flexible to either census tract level or community area level.

./Aggregate 
Contains functions that aggregates trip level data and connects it to the slots.

./Community_Areas
Contains functions that extracts the community zones that contains census tracts far away from the congestion zone.