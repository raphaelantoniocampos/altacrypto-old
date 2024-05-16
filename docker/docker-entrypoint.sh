#!/bin/sh

set -e

# activate our virtual environment here
. .venv/bin/activate


# Evaluating passed command:
exec "$@"
