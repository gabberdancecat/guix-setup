#!/bin/sh

# Check if both zip file and destination directory are provided
if [ $# -lt 1 ]; then
    echo "Usage: $0 <path_to_zipfile> [destination_directory]"
    exit 1
fi

# Get the full path to the zip file
ZIPFILE=$(readlink -f "$1")

# Determine destination directory
if [ $# -eq 2 ]; then
    # Use provided destination directory, create if it doesn't exist
    DEST_DIR=$(readlink -f "$2")
    mkdir -p "$DEST_DIR"
else
    # If no destination specified, use current working directory
    DEST_DIR=$(pwd)
fi

# Extract the filename without the path and extension
DIRNAME=$(basename "$ZIPFILE" .zip)

# Full path for the new directory
FULL_DEST_PATH="$DEST_DIR/$DIRNAME"

## debug

echo "zip: $ZIPFILE"
echo "dest: $DEST_DIR"
echo "dir: $DIRNAME"
echo "full dest: $FULL_DEST_PATH"
echo "dest"

echo "Proceed? (Enter / C-c)"

read tmp

# Create the directory in the destination
mkdir -p "$FULL_DEST_PATH"

# Move the zip file into the new directory
cp "$ZIPFILE" "$FULL_DEST_PATH/"

# Change to the new directory
cd "$FULL_DEST_PATH"

# Unzip the file
unzip "$(basename "$ZIPFILE")"

# Optional: Remove the zip file after unzipping (comment out if you want to keep it)
rm "$(basename "$ZIPFILE")"

echo "Unzipped $ZIPFILE to $FULL_DEST_PATH"
