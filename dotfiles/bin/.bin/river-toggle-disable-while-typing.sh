#!/usr/bin/env sh

die() { echo "Error: $@. Exiting..."; exit 1; }

setRiverInput() {
    riverctl 
}


input_all_names="$(riverctl list-inputs | tr -s '\n')"
input_all_names_NR="$(echo $input_all_names | wc -l)"

echo "$input_all_names"
echo "$input_all_names_NR"

for i in $(seq 1 $input_all_names_NR); do
    if grep -q 

input_names="$(echo "$input_all_names" | \
awk '{ \
    for (i=1; i<=NR; i++) {
    	tmp=match($0, /*configured:.true.*/) ; \
	print tmp ; \
	if(tmp) { print $i } ; \
    } \
}' $1 \
)"

echo "$input_names"

exit









input_configs_raw="$(riverctl list-input-configs)"

# get input name

input_configs_name="$(echo "$input_configs_raw" | \
head -n 1)"

# get current status


input_configs_search_num="$(echo "$input_configs_raw" | \
grep -c 'disable-while-typing')"

input_configs_status="$(echo "$input_configs_raw" | \
perl -n -e '/^.*disable-while-typing:[[:space:]]*(.*)$/ && print $1')"

echo $input_configs_status

# set 

# toggle if no args

if [ $# = 0 ]; then
    if [ "$input_configs_status" = "enabled" ]; then
	
