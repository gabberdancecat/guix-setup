#!/bin/sh

function die { echo "Error: $@, exiting..." ; exit 1 ; }


function all_mullvad_wg_names {
    # alternative to key-pairs
    local con_names="$(nmcli -f NAME con show | sed 1d)"
    local con_types="$(nmcli -f TYPE con show | sed 1d)"
    local IFS=$'\n'
    IFS=$'\n' local con_names_arr=($nm_lists)
    IFS=$'\n' local con_types_arr=($nm_types)
    echo "$con_names $con_types $con_names_arr $con_types_arr"
    local IFS=$' '
    [ ${con_names_arr[@]} = ${con_types_arr[@]} ] || die "fuck you"
    echo "TEST: ${#con_names_arr[*]} ${#con_names_arr[@]}"
    for index in ${!con_names_arr[@]}; do
	echo "${con_names_arr[$index]}"
    done
}

all_mullvad_wg_names


# nm_types="$(nmcli -f TYPE con show | sed 1d)"
# nm_lists="$(nmcli -f NAME con show | sed 1d)"

# echo
# echo "$nm_types"
# echo 
# echo "$nm_lists"
# echo

# IFS_save_to_arr() {
#     local IFS=$'\n'
#     local lines=($1)
#     local i
#     for (( i=0; i<${#lines[@]}; i++ )) ; do
# 	echo "$i: ${lines[$i]}"
#     done
# }

# IFS_save_to_arr "$nm_types"
# IFS_save_to_arr "$nm_lists"

# # IFS=$'\n' read -r -d '' -a array <<< $nm_lists

# IFS=$'\n' array1=($nm_lists)
# IFS=$'\n' array2=($nm_types)
# for i in ${array1[@]}; do echo $i; done
# for i in ${array2[@]}; do echo $i; done

# # readarray -t arr1 <<< "$nm_types"
# # readarray -t arr2 <<< "$nm_lists"

# # echo "$arr1"
# # echo "$arr2"
