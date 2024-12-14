#!/bin/sh

# Todo: support case "Not Charging"

acpi | head -n 1 | awk -F'[:,] *' '/Battery/ {
    state = $2 ~ /Discharging/ ? "-" : ($2 ~ /Charging/ ? "+" : "?")
    hours = $4 ~ /^0/ ? $4 + 0 : $4
    minutes = $5 ~ /^0/ ? $5 + 0 : $5
    if (hours == 0)
        printf "%s %s %sm", state, $3, minutes
    else
	printf "%s %s %sh %sm", state, $3, hours, minutes
}'
