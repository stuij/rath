#!/usr/bin/env python3
# convert tiled json data to binary map

# currently this just writes the collision data tile array to binary

import argparse
import json
import os
import struct

apartment = "/home/zeno/code/rath/assets/apartment-map.json"

toi_map = {
    "collisions": 0,
    "kitchen": 1,
    "sink": 2,
    "toilet": 3,
    "bath": 4,
    "couch": 5,
    "tv": 6,
    "front-door": 7,
    "desk": 8,
    "bed": 9,
    "closet": 10,
    "clothes": 11,
    "gamecube": 12,
    "fridge": 13,
    "poster": 14,
}

def to_json(file):
    with open(file) as f:
        return json.load(f)

def get_toi_data(data):
    global toi_map
    value_dict = {}
    for layer in data["layers"]:
        if layer["name"] in toi_map:
            value_dict[layer["name"]] = [1 if x != 0 else x for x in layer["data"]]
    return value_dict

def zip_toi_data(toi_dict):
    collisions = toi_dict["collisions"]
    out_arr = [0x1 if x != 0 else x for x in collisions]
    for name, toi_list in toi_dict.items():
        if name != "collisions":
            toi_val = toi_map[name]
            for i, val in enumerate(toi_list):
                if val != 0:
                    out_arr[i] = 1 << toi_val
    return out_arr

def to_file(array, out):
    bytes = struct.pack("{}H".format(len(array)), *array)
    with open(out, 'wb') as f:
        f.write(bytes)

# start
def main():
    parser = argparse.ArgumentParser(description='convert tiled json data to binary map')
    parser.add_argument('file', help='a Tiled JSON file')
    parser.add_argument('-o', default="",
                        help='the output file')

    args = parser.parse_args()
    out = os.path.splitext(os.path.basename(args.file))[0] + ".bin" if args.o == "" else args.o
    data = to_json(args.file)

    toi_dict = get_toi_data(data)
    output_array = zip_toi_data(toi_dict)
    to_file(output_array, out)

if __name__ == "__main__":
    main()
