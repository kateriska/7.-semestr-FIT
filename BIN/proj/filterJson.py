import json

with open('cgp-approx14ep.json', "r") as json_file:
    json_data = json.load(json_file)

    data = {}
    for item in json_data:
        print(item)

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

    with open('filtered_data.json', 'w+') as outfile:
        json.dump(data, outfile)
        #print(json_data['mult8_cgp14ep_ep65536_wc9_wtmrca']["mae"])



with open('filtered_data.json', "r") as json_file:
    json_data = json.load(json_file)

    #json_name_id = json_data.keys()
    #print (json_name_id)


    print(json_data['mult8_cgp14ep_ep65536_wc9_wtmrca'][0]["mae"])
