#!/bin/bash

echo "1:$1"
echo "2:$2"
echo "3:$3"
echo "4:$4"
echo "5:$5"

# Este script usa ssh con dockeruser.
# ATENCIÓN, este script asume que ssh usa .huslogin para que el Message of the Day MoD no interfiera en la captura de los output!
# Fuente: https://askubuntu.com/questions/676374/how-to-disable-welcome-message-after-ssh-login/1175798
# Function to print usage
usage() {
    echo "Usage: $0 --remote_script <remote_script> --dir_in <dir_in> [--dir_out <dir_out>] [--help]"
    echo "Example: $0 --remote_script Modelar.sh --dir_in /path/to/input --dir_out /path/to/output"
    exit 1
}

# Parse named parameters
while [[ "$#" -gt 0 ]]; do
    case "$1" in
        --remote_script) REMOTE_SCRIPT="$2"; echo "$2"; shift ;;
        --dir_in) DIR_IN="$2"; echo "$2"; shift ;;
        --dir_out) DIR_OUT="$2"; echo "$2"; shift ;;
        --help) usage ;;
        *) echo "Unknown parameter passed: $1"; usage ;;
    esac
    shift
done

# Check if required parameters are provided
if [ -z "$REMOTE_SCRIPT" ] || [ -z "$DIR_IN" ]; then
    usage
fi

# Set DIR_OUT to DIR_IN if not provided
DIR_OUT=${DIR_OUT:-$DIR_IN}

echo "Parámetros recibidos"
echo "$REMOTE_SCRIPT"
echo "$DIR_IN"
echo "$DIR_OUT"
