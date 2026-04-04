#!/bin/bash

# Script to compare OUTP and OUTP_REF directories
# Usage: ./compare_outputs.sh

OUTP_DIR="./OUTP"
OUTP_REF_DIR="./OUTP_REF"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "Comparing OUTP and OUTP_REF directories..."
echo "============================================"

# Check if directories exist
if [ ! -d "$OUTP_DIR" ]; then
    echo -e "${RED}Error: $OUTP_DIR does not exist${NC}"
    exit 1
fi

if [ ! -d "$OUTP_REF_DIR" ]; then
    echo -e "${RED}Error: $OUTP_REF_DIR does not exist${NC}"
    exit 1
fi

# Track differences
differences_found=0
diff_files=()

# Compare each file in OUTP_REF with OUTP
for ref_file in "$OUTP_REF_DIR"/*; do
    ref_filename=$(basename "$ref_file")
    outp_file="$OUTP_DIR/$ref_filename"
    
    if [ ! -f "$outp_file" ]; then
        echo -e "${YELLOW}Missing in OUTP: $ref_filename${NC}"
        differences_found=1
    else
        # Compare files (skip first line)
        if diff -q <(tail -n +2 "$ref_file") <(tail -n +2 "$outp_file") > /dev/null; then
            echo -e "${GREEN}✓ $ref_filename${NC}"
        else
            echo -e "${RED}✗ $ref_filename (DIFFERENCES FOUND)${NC}"
            differences_found=1
            diff_files+=("$ref_filename")
            
            # Show first 10 lines of diff (skipping first line of both files)
            diff <(tail -n +2 "$ref_file") <(tail -n +2 "$outp_file") | head -10 | sed 's/^/    /'
            
            if [ $(diff <(tail -n +2 "$ref_file") <(tail -n +2 "$outp_file") | wc -l) -gt 10 ]; then
                echo "    ... (more differences)"
            fi
        fi
    fi
done

# Check for extra files in OUTP
echo ""
echo "Checking for extra files in OUTP..."
for outp_file in "$OUTP_DIR"/*; do
    outp_filename=$(basename "$outp_file")
    if [ ! -f "$OUTP_REF_DIR/$outp_filename" ]; then
        echo -e "${YELLOW}Extra in OUTP: $outp_filename${NC}"
        differences_found=1
    fi
done

echo ""
echo "============================================"
if [ $differences_found -eq 0 ]; then
    echo -e "${GREEN}All files match!${NC}"
    exit 0
else
    echo -e "${RED}Differences found.${NC}"
    echo ""
    
    # Ask if user wants to open vimdiff for differing files
    if [ ${#diff_files[@]} -gt 0 ]; then
        echo "Files with differences:"
        for i in "${!diff_files[@]}"; do
            echo "  $((i+1)). ${diff_files[$i]}"
        done
        
        echo ""
        read -p "Open vimdiff for any of these files? (y/n): " -n 1 -r
        echo
        
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            for filename in "${diff_files[@]}"; do
                read -p "Open vimdiff for $filename? (y/n): " -n 1 -r
                echo
                if [[ $REPLY =~ ^[Yy]$ ]]; then
                    vimdiff "$OUTP_REF_DIR/$filename" "$OUTP_DIR/$filename"
                fi
            done
        fi
    fi
    
    exit 1
fi
