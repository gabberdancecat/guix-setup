#!/bin/bash

# customize these
WGET=""
ICS2ORG=<path to ical2org>
ICSFILE=$(mktemp)
ORGFILE=<path to orgfile>
URL=<url to your private Google calendar>

# no customization needed below

$WGET -O $ICSFILE $URL
$ICS2ORG $ICSFILE $ORGFILE
rm -f $ICSFILE
