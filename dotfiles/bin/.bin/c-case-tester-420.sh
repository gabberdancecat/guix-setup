#!/bin/sh

### Functions ###

print_usage() {
    cat <<EOF
Usage: $(basename $0) [ARGUMENTS]

 Single test case:
   $(basename $0) [C_FILE] [INPUT] [OUTPUT] ...
 Multiple test cases:
   $(basename $0) [C_FILE] [INPUT(@)] [OUTPUT(@)] [RANGE(-)] ...

DESCRIPTION:
 This script will show diff(s) of a C program's output and specified test case(s).
 Depending on the arguments provided, it will either do just one test case
 or a set of test cases.
 - If a RANGE is specified (e.g. 1-10), it will assume we are testing multiple
   test cases. In this case, the INPUT AND OUTPUT args must both have a "@" in
   their filenames. This char will iter through the range of numbers, evaluating
   test cases for all matching filenames. (Note: INPUT is the input for the C binary
   and OUTPUT is the expected output or test case).
 - If a RANGE is not specified, it will assume we are only testing a single test
   case. In this case, the corresponding INPUT and OUTPUT files will be used
   to eval the test case.

ARGUMENTS:
 -c : C file
 -i : file with inputs for C binary (if Multiple mode, must have "@" char)
 -o : file with expected output (if Multiple mode, must have "@" char)
 -r : range for "@" char (for Multiple mode)
 -d : (optional) arguments for diff command (default: "-u")
 -s : (optional) force-use Single mode (usually will be assumed correctly)
 -m : (optional) force-use Multiple mode (usually will be assumed correctly)
 
EXAMPLES:
 $ $(basename $0) -c main.c -i inputs -o testcase
 $ $(basename $0) -c main.c -i @.in.txt -o @.out.txt -r 1-10
   (Note: @.in.txt will be substituted to 1.in.txt, 2.in.txt, etc...)

EOF
    exit 0
}

die() { echo "> Error: $1."; exit 1; }

### Main ###

## Process Args ##

[ "$#" -eq 0 ] && print_usage

while getopts ":c:i:o:r:d:smh" opt; do
    case $opt in
	# No argument required:
	s)
	    if [ "$mode" = "multiple" ]; then
		die "Conflicting options -m and -s"
	    fi
	    mode=single
	    ;;
	m)
	    if [ "$mode" = "single" ]; then
		die "Conflicting options -s and -m"
	    fi
	    mode=multiple
	    ;;
	h)
	    print_usage
	    ;;
	# Argument required:
	c)
	    c_file=$OPTARG ;;
	i)
	    in=$OPTARG ;;
	o)
	    out=$OPTARG ;;
	r)
	    range=$OPTARG ;;
	d)
	    diff_opts=$OPTARG ;;
	*)
	    die "Invalid argument option, run with -h to view available" ;;
    esac
done

# assume default mode if unspecified
if [ -z $mode ]; then
    if [ -z $range ]; then
	mode=single
	echo "> Notice: using mode Single (-s) by default."
    else
	mode=multiple
	echo "> Notice: using mode Multiple (-m) because range specified."
    fi
fi

# set default args
[ -z $diff_opts ] && diff_opts="-u"

## Args Error Checking ##

[ -z $c_file ] && die "Missing -c (C_FILE) argument"
[ -z $in     ] && die "Missing -i (INPUT) argument"
[ -z $out    ] && die "Missing -o (OUTPUT) argument"
if [ "$mode" = "single" ]; then
    # Note: -n requires "quoted string"
    [ -n "$range" ] && die "Arg -r (RANGE) not applicable in Single mode"
fi
if [ "$mode" = "multiple" ]; then
    if [ -z $range ]; then
	die "Range not specified, use -r (RANGE)"
    fi
    basename $in |grep -q "@" || die "cannot find '@' in filename"
    basename $out |grep -q "@" || die "cannot find '@' in filename"
fi

## Compile ##

exec=${c_file%.*}
gcc_command="gcc -std=gnu11 -lm -g -o $exec $c_file"
echo "> GCC command: $gcc_command"
$gcc_command || die "compilation failed"

## Single Mode ##

if [ "$mode" = "single" ]; then
    echo "> Comparing $in with $out:"
    exec_output=`$exec < $in`
    case_output=`cat $out`
    
    echo "# Diff"
    diff <(echo "$exec_output") <(echo "$case_output") --color
    echo "# Diff - end"
fi

## Multiple Mode ##

if [ "$mode" = "multiple" ]; then
    beg=${range%-*} && end=${range#*-}
    # test cases
    for i in $(seq $beg $end); do
	echo "# Case #$i"
	
	in_cur=$(dirname $in)/$(basename $in |tr '@' $i)
	out_cur=$(dirname $in)/$(basename $out |tr '@' $i)
	
	exec_output=`./$exec < $in_cur`
	case_output=`cat $out_cur`

	diff <(echo "$exec_output") <(echo "$case_output") --color
	
	echo "# Case #$i - end"
    done
fi
