#!/bin/bash

# Function to display help message
show_help() {
  echo "Usage: $0 [OPTIONS] [PATTERN]"
  echo
  echo "Convert Windows-style line endings to Unix-style in files matching the given PATTERN."
  echo
  echo "Options:"
  echo "  --help    Show this help message and exit"
  echo
  echo "Example:"
  echo "  $0 '*.sh'    Process all .sh files in the current directory"
}

# Check if the --help option is provided
if [[ "$1" == "--help" ]]; then
  show_help
  exit 0
fi

# Use the provided pattern or default to '*.*'
pattern="${1:-*.*}"

# Warn if the default pattern is used
if [[ -z "$1" ]]; then
  echo "Warning: No pattern provided. Using default pattern '*.*'."
fi

# Loop through all files matching the given pattern
for file in $pattern; do
  # Check if the file exists and is a regular file
  if [ -f "$file" ]; then
    # Convert Windows-style line endings to Unix-style
    sed -i 's/\r$//' "$file"
    echo "Processed $file"
  fi
done

echo "All files matching the pattern '$pattern' have been processed."

