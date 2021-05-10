#!/usr/bin/env python
# convert tiled json data to binary map

# currently this just writes the collision data tile array to binary

import argparse
import json
import os

apartment = "/home/zeno/code/rath/assets/apartment-map.json"

def to_json(file):
    with open(file) as f:
        return json.load(f)

def get_collision_data(data):
    for layer in data["layers"]:
        if layer["name"] == "collisions":
            return [1 if x != 0 else x for x in layer["data"]]

def to_file(array, out):
    with open(out, 'wb') as f:
        f.write(bytearray(array))

# start
def main():
    parser = argparse.ArgumentParser(description='convert tiled json data to binary map')
    parser.add_argument('file', help='a Tiled JSON file')
    parser.add_argument('-o', default="",
                        help='the output file')

    args = parser.parse_args()
    out = os.path.splitext(os.path.basename(args.file))[0] + ".bin" if args.o == "" else args.o
    data = to_json(args.file)

    output_array = get_collision_data(data)
    to_file(output_array, out)

if __name__ == "__main__":
    main()
