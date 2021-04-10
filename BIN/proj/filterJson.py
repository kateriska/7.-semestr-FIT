from random import seed
from random import random
import json

seed(1)
with open('cgp-approx14ep.json', "r") as json_file:
    json_data = json.load(json_file)

    data = {}
    i = 0
    for item in json_data:
        print(item)
        if ("_rcam" not in item or random() < 0.5):
            continue
        data[item] = []
        data[item].append({
            "cells": json_data[item]["cells"],
            "mae": json_data[item]["mae"],
            "wce": json_data[item]["wce"],
            "wce%": json_data[item]["wce%"],
            "levels": json_data[item]["evo"]["Levels"],
            "pdk45_area": json_data[item]["pdk45_area"],
            "pdk45_delay": json_data[item]["pdk45_delay"],
            "pdk45_pwr": json_data[item]["pdk45_pwr"]
})
        #print(data)
        i = i + 1

    with open('filtered_data.json', 'w+') as outfile:
        json.dump(data, outfile)
        #print(json_data['mult8_cgp14ep_ep65536_wc9_wtmrca']["mae"])
print("Count of objects in filtered json file: " + str(i))
