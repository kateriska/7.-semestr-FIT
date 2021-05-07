# Project name: Relationship Analysis of Approximated Circuits
# Author: Katerina Fortova
# Login: xforto00
# Year: 2020 / 2021

# Description:
# This program has to be run before countFeatures.py
# Program filteres JSON file for only interesting metrics which could be used in next analysis
# User can filter only objects of one type (rcam|wtmcla|wtmcsa|csamcsa|csamrca|wtmrca) or all of them and set a number of filtered objects or ratio (e.g. 0.5 - 50 % of objects)
# It is recommended to analyse only some smaller subset of objects to get results in more reasonable time for countFeatures.py script, because processing all almost 25 000 multipliers take some longer time.

import random
import json
import sys

def filterFile(used_filter, used_count, selected_json_items_name, value):
    with open('cgp-approx14ep.json', "r") as json_file:
        json_data = json.load(json_file)

        data = {}
        origin = {} # origin parents of multipliers evolved by CGP
        i = 0
        filtered_items_count = 0
        for item in json_data:
            #print(item)
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
                # if user selected ratio of filtered items (e.g. -p 0.5 - filter half of objects)
                if (used_count == False):
                    generated_random_number = random.random()
                    if (used_filter != "all"):
                        if (used_filter not in item or generated_random_number >= value):
                            continue
                        else:
                            filtered_items_count += 1
                    else:
                        # filter only count of previously randomly selected objects
                        if (generated_random_number >= value):
                            continue
                        else:
                            filtered_items_count += 1
                else:
                    if (item not in selected_json_items_name):
                        continue
                    else:
                        filtered_items_count += 1

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

        # create new json file with filtered objects
        with open('filtered_data.json', 'w+') as outfile:
            json.dump(data, outfile)
            #print(json_data['mult8_cgp14ep_ep65536_wc9_wtmrca']["mae"])
        with open('origin_data.json', 'w+') as origin_outfile:
            json.dump(origin, origin_outfile)
                #print(json_data['mult8_cgp14ep_ep65536_wc9_wtmrca']["mae"])
        print("Count of objects in filtered json file: " + str(filtered_items_count))

# rcam, wtmcla, wtmcsa
arguments_count = len(sys.argv) - 1
used_filter = "all"
used_count = False

if (arguments_count == 3 and (sys.argv[1] == "rcam" or sys.argv[1] == "wtmcla" or sys.argv[1] == "wtmcsa" or sys.argv[1] == "csamrca" or sys.argv[1] == "csamcsa" or sys.argv[1] == "wtmrca" or sys.argv[1] == "all")):
    used_filter = sys.argv[1]

    if (sys.argv[2] == "-n"): # count of objects e.g. -n 20
        used_count = True
        try:
            value = int (sys.argv[3])
        except:
            sys.stderr.write("ERROR - Bad input value for -n, use integer \n")
            exit(1)
    elif (sys.argv[2] == "-p"): # ratio of filtered objects e.g. -p 0.8 for filtering 80 % of objects
        used_count = False
        try:
            value = float (sys.argv[3])
        except:
            sys.stderr.write("ERROR - Bad input value for -p, use float \n")
            exit(1)

        if (value < 0 or value > 1):
            sys.stderr.write("ERROR - Bad input value for -p, use float in interval 0 - 1 \n")
            exit(1)

else:
    sys.stderr.write("ERROR - Bad input arguments\n")
    sys.stderr.write("Run with: filterJson.py rcam|wtmcla|wtmcsa|all -n|-p [number]\n")
    exit(1)


json_items_name = []
selected_json_items_name = []

# if -n is inserted, select all multipliers by used filter of type of all of them and randomly select n of them
if (used_count == True):
    with open('cgp-approx14ep.json', "r") as json_file:
        json_data = json.load(json_file)
        for item in json_data:
            if (used_filter != "all"):
                if (used_filter in item):
                    json_items_name.append(item)
            else:
                json_items_name.append(item)

    selected_json_items_name = random.sample(json_items_name, value)

filterFile(used_filter, used_count, selected_json_items_name, value)
