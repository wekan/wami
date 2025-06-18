#!/bin/bash

# Create backup
cp wekan.pas wekan.pas.bak3

# Find the begin statement of the main program
grep -n "^begin" wekan.pas | tail -1 > begin_line.txt
BEGIN_LINE=$(cat begin_line.txt | cut -d':' -f1)

# Extract main program code after the first end.
sed -n '/^end\./,$ p' wekan.pas | tail -n +2 > main_program_code.txt

# Remove this code from the file
sed -n '1,/^end\./ p' wekan.pas > wekan.pas.tmp

# Now find a proper begin block in the main program and append the code
# Insert before the end. statement
sed -i "${BEGIN_LINE}r main_program_code.txt" wekan.pas.tmp

# Replace the original file
mv wekan.pas.tmp wekan.pas

# Clean up
rm begin_line.txt main_program_code.txt

echo "Fixed program structure by moving code after end."
