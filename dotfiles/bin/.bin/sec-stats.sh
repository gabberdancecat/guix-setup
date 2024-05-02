#!/bin/sh

# Note: make sure to quote when echo vars, to print newlines.
# https://stackoverflow.com/questions/22101778/how-to-preserve-line-breaks-when-storing-command-output-to-a-variable


ConTypes=$(nmcli -f TYPE con show --active | sed 1d) # rem first word
ConNames=$(nmcli -f NAME con show --active | sed 1d)
ConTypeName=$(nmcli -f TYPE,NAME con show --active | sed 1d)

IsWGActive=$(echo "$ConTypeName" | grep -zqv "^wireguard" ; echo $?)
ConWGs=$(echo "$ConTypeName" | grep "^wireguard" | awk '{print $2}')
ConVPNs=$(echo "$ConTypeName" | grep "^vpn" | awk '{print $2}')
NumWGPersActive=$(echo "$ConWGs" | grep -Ec "us-mia-wg-.*")
NumWGNotPersActive=$(echo "$ConWGs" | grep -Evc "us-mia-wg-.*")
NumVPNPersActive=$(echo "$ConWGs" | grep -Evc "us-mia-wg-.*")
NumVPNNotPersActive=$(echo "$ConWGs" | grep -Ec "us-mia-wg-.*")


echo
echo "$ConTypes"
echo
echo "$ConNames"
echo
echo "$ConTypeName"
echo
echo "$IsWGActive"
echo
echo "$ConWGs"
echo
echo "$NumWGMullvadActive"
echo


# 
vpn_stat=(0)


VPN_STATUS() {
    local stat=0

    ## All good; connected to 1 Mullvad connection
    if (( $NumPersActive == 1 )); then
	return 0
    fi

    ## Warning; connected to another WG or VPN
    if (( $NumWGNotPersActive > 0 || $NumVPNNotPersActive > 0 )); then
	return 1
    fi
    
    
    
}

VPN_STATUS
echo $?
