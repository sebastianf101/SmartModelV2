#!/bin/bash

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
        --remote_script) REMOTE_SCRIPT="$2"; shift ;;
        --dir_in) DIR_IN="$2"; shift ;;
        --dir_out) DIR_OUT="$2"; shift ;;
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
DIR_OUT=${DIR_OUT:-"$DIR_IN"}

# SSH credentials and remote server details
REMOTE_USER="dockeruser"
REMOTE_HOST="besmart.ar"
REMOTE_PORT="2022" # Change if using a different port

# Use SSH key for authentication
SSH_KEY="~/.ssh/id_consultor"

# Create temporary directories on the remote server
# uso una locación variable de env_vars.sh para evitar colisiones. 
TEMP_DIR_OUT=$(ssh -T -i $SSH_KEY -p $REMOTE_PORT $REMOTE_USER@$REMOTE_HOST << 'EOF'
    TEMP_DIR_IN=$(mktemp -d)
    TEMP_DIR_OUT=$(mktemp -d)
    echo "TEMP_DIR_IN=$TEMP_DIR_IN" > $TEMP_DIR_OUT/env_vars.sh
    echo "TEMP_DIR_OUT=$TEMP_DIR_OUT" >> $TEMP_DIR_OUT/env_vars.sh
    echo $TEMP_DIR_OUT
EOF
)

# Copy back the env vars file and source it
scp -i $SSH_KEY -P $REMOTE_PORT $REMOTE_USER@$REMOTE_HOST:$TEMP_DIR_OUT/env_vars.sh .
source ./env_vars.sh

# Verify the local environment variables
echo "Directorio de Entrada en dockersrv: $TEMP_DIR_IN"
echo "Directorio de Salida en dockersrv: $TEMP_DIR_OUT"

# Copy contents of dir_in to TEMP_DIR_IN on the remote server
scp -i $SSH_KEY -P $REMOTE_PORT -r "$DIR_IN"/* $REMOTE_USER@$REMOTE_HOST:$TEMP_DIR_IN

#ssh -T -i $SSH_KEY -p $REMOTE_PORT $REMOTE_USER@$REMOTE_HOST "ls $TEMP_DIR_IN"
# Path donde están en dockersrv (host) los scripts Modelar.sh, Validar_Desa.sh, etc
SCRIPT_PATH="/home/$REMOTE_USER/BSM/Scripts/10.3/$REMOTE_SCRIPT"

# Run the script on the remote server
ssh -T -i $SSH_KEY -p $REMOTE_PORT $REMOTE_USER@$REMOTE_HOST << EOF
    export TEMP_DIR_IN=$TEMP_DIR_IN
    export TEMP_DIR_OUT=$TEMP_DIR_OUT
    export SCRIPT_PATH=$SCRIPT_PATH
    bash \$SCRIPT_PATH --input-dir \$TEMP_DIR_IN --output-dir \$TEMP_DIR_OUT
EOF

# Copy contents of TEMP_DIR_OUT back to dir_out on the local machine
scp -i $SSH_KEY -P $REMOTE_PORT -r $REMOTE_USER@$REMOTE_HOST:$TEMP_DIR_OUT/* "$DIR_OUT"

echo "Archivos copiados a directorio de salida local $DIR_OUT"
ls "$DIR_OUT"

# Clean up temporary directories on the remote server
ssh -T -i $SSH_KEY -p $REMOTE_PORT $REMOTE_USER@$REMOTE_HOST << 'EOF'
    export TEMP_DIR_IN=$TEMP_DIR_IN
    export TEMP_DIR_OUT=$TEMP_DIR_OUT
    rm -rf $TEMP_DIR_IN $TEMP_DIR_OUT
EOF

echo "Process completed successfully."

