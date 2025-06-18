#!/bin/bash
# Fix all RouteParams.IndexOfName and .Values occurrences

# Create backup
cp wekan.pas wekan.pas.bak2

# Find all parameter names being accessed
params=$(grep -Po "aRequest\.RouteParams\.Values\[\'\K[^\']*" wekan.pas | sort | uniq)

# Fix each parameter one by one
for param in $params; do
  sed -i "s/if aRequest\.RouteParams\.IndexOfName('$param') >= 0 then//" wekan.pas
  sed -i "s/  $param := aRequest\.RouteParams\.Values\['$param'\];/$param := aRequest.RouteParams['$param'];/" wekan.pas
done

echo "Fixed RouteParams usage in wekan.pas"
