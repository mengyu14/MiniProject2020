#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This script is used for matching latitude and longitude of the powerplants to 
their respective county and state.

Mary Yu

"""
import pandas as pd
import reverse_geocoder as rg


powerplant = pd.read_csv("~/Desktop/Second Year Scholarship/Data/US_Dataset_PowerPlants_Locations_Nature_County.csv")

powerplant.head()

County = []
State = []

for i in range(len(powerplant)):
    coor = (powerplant['latitude'][i], powerplant['longitude'][i])
    result = rg.search(coor)  
    State.append(result[0]['admin1'])
    County.append(result[0]['admin2'])

powerplant['County'] = County
powerplant['State'] = State

powerplant.to_csv('~/Desktop/Second Year Scholarship/Data/Powerplant.csv')
