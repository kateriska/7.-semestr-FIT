#!/usr/bin/bash

# Create and start virtual environment
python3 -m venv BINenv
source BINenv/bin/activate
export PYTHONPATH=$PWD:$PYTHONPATH

# Download and install required packages
pip install -r requirements.txt
