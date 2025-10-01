#!/bin/bash

# If no arguments provided, start interactive bash
if [ $# -eq 0 ]; then
    /bin/bash
else
    # Execute the provided command
    exec "$@"
fi
