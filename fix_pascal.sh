#!/bin/bash
# Fix the structure of wekan.pas by moving implementations after end. to before the main begin

# Create backup
cp wekan.pas wekan.pas.bak

# Extract parts
head -n 255 wekan.pas > part1.tmp
sed -n '256,367p' wekan.pas > part2.tmp
tail -n +368 wekan.pas > part3.tmp

# Combine parts in correct order
cat part1.tmp > wekan.pas.new
cat part3.tmp >> wekan.pas.new
cat part2.tmp >> wekan.pas.new

# Clean up
rm part1.tmp part2.tmp part3.tmp

# Replace the original file
mv wekan.pas.new wekan.pas

echo "Fixed wekan.pas by moving functions/procedures from after end. to before the main begin statement"
