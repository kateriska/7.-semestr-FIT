from random import seed
from random import random
import json
import sys

seed(1)
# rcam, wtmcla, wtmcsa
arguments_count = len(sys.argv) - 1
used_filter = ""
if (arguments_count == 1 and (sys.argv[1] == "rcam" or sys.argv[1] == "wtmcla" or sys.argv[1] == "wtmcsa")):
    used_filter = sys.argv[1]

with open('cgp-approx14ep.json', "r") as json_file:
    json_data = json.load(json_file)

    data = {}
    origin = {} # origin parents of multipliers evolved by CGP
    i = 0
    for item in json_data:
        print(item)
        if (i < 3): # first three multipliers are origin multipliers from which cgp evolved aproximated multipliers
            origin[item] = []
            origin[item].append({
                "cells": json_data[item]["cells"],
                "mae": json_data[item]["mae"],
                "wce": json_data[item]["wce"],
                "wce%": json_data[item]["wce%"],
                "levels": json_data[item]["evo"]["Levels"],
                "pdk45_area": json_data[item]["pdk45_area"],
                "pdk45_delay": json_data[item]["pdk45_delay"],
                "pdk45_pwr": json_data[item]["pdk45_pwr"],
                "seed": json_data[item]["seed"]
                })

        else:
            if (used_filter != ""):
                if (used_filter not in item or random() < 0.5):
                    continue
            #else:
                #if (random() < 0.5):
                #    continue
            data[item] = []
            data[item].append({
            "cells": json_data[item]["cells"],
            "mae": json_data[item]["mae"],
            "wce": json_data[item]["wce"],
            "wce%": json_data[item]["wce%"],
            "levels": json_data[item]["evo"]["Levels"],
            "pdk45_area": json_data[item]["pdk45_area"],
            "pdk45_delay": json_data[item]["pdk45_delay"],
            "pdk45_pwr": json_data[item]["pdk45_pwr"],
            "seed": json_data[item]["seed"]
            })



            #print(data)
        i = i + 1

    with open('filtered_data.json', 'w+') as outfile:
        json.dump(data, outfile)
        #print(json_data['mult8_cgp14ep_ep65536_wc9_wtmrca']["mae"])
    with open('origin_data.json', 'w+') as origin_outfile:
        json.dump(origin, origin_outfile)
            #print(json_data['mult8_cgp14ep_ep65536_wc9_wtmrca']["mae"])
print("Count of objects in filtered json file: " + str(i - 3))
