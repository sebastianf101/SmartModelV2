#!/bin/bash

url="www.lanacion.com"

# Function to check if running in WSL
is_wsl() {
    grep -qEi "(Microsoft|WSL)" /proc/version &> /dev/null
    return $?
}

if is_wsl; then
    # Running in WSL
    powershell.exe /c start "$url"
else
    # Running in native Linux
    xdg-open "$url"
fi
