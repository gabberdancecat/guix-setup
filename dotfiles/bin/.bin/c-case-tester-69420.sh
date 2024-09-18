#!/bin/sh

### User variables ###

compiler="gcc"
my_default_args="-std=gnu11 -lm -g"
my_valgrind_default_args="--leak-check=full \
--show-leak-kinds=all \
--track-origins=yes \
--log-file=valgrind-out.txt \
--verbose"

### Functions ###

die() { echo "> Error: $1." ; exit 1 ; }

print_usage() {
    cat <<EOF
Usage: $(basename $0) [ARGUMENTS]

 Single test case:
   $(basename $0) [C_FILE] [INPUT] [OUTPUT] ...
 Multiple test cases:
   $(basename $0) [C_FILE] [INPUT(@)] [OUTPUT(@)] [RANGE(-)] ...

DESCRIPTION:
 This script will show diff(s) of a C program's output and specified test case(s).
 - For a single test case, specify the C file (-c), the input test case path (-i),
   and the output test case path (-o). All other arguments are optional.
 - For multiple test cases (e.g. out1.txt, out2.txt), have a "@" in both input and
   output file paths to iterate through a number (e.g. -i ./cases/out@.txt).
   The range of numbers to iterate through must also be specified (-r)
   (e.g. -r 1-10) (both inclusive).
 - There is an optional argument to run the program with Valgrind (-V) and output
   the results in a file valgrind-out.txt.
 - The default compile command args and run/valgrind command args can be
   overwritten with (-d, -D). It can instead be appended with (-a, -A).

VALGRIND OPTION:
 - If -V arg is passed, the binary will be executed using Valgrind.
   The -D arg can be used to completely overwrite and replace the default
   Valgrind args.
   The -A arg can be used to append new args to the default Valgrind args.

ARGUMENTS:
 -c : C file
 -i : input test case (requires "@" if Multiple mode)
 -o : output test case (requires "@" if Multiple mode)
 -r : iter range for "@" char (inclusive, only for Multiple mode)
 -d : (optional) overwrite default compile args
 -a : (optional) append to default compile args
 -D : (optional) overwrite default Valgrind run args
 -A : (optional) append to default Valgrind run args
 -s : (optional) force-use Single mode
 -m : (optional) force-use Multiple mode
 
EXAMPLES:
 $ $(basename $0) -c main.c -i in.txt -o out.txt
 $ $(basename $0) -c main.c -i @.in.txt -o @.out.txt -r 1-10
   (Note: @.in.txt will be substituted to 1.in.txt, 2.in.txt, etc...)
 $ $(basename $0) -c main.c -i in.txt -V

EOF
}

process_args() {
    [ "$#" -eq 0 ] && { print_usage ; exit 0 ; }
    mode=nil
    valgrind=0
    while getopts ":c:i:o:r:d:a:D:A:smvh" opt; do
	case $opt in
            # No additional argument required:
            s)  [ $mode != nil ] && die "Conflicting options -s and -${mode}" ||
			mode='s'  ;;
	    m)  [ $mode != nil ] && die "Conflicting options -m and -${mode}" ||
			mode='m'  ;;
	    v)  valgrind=1  ;;
            h)  { print_usage ; exit 0 ; }  ;;
            # Additional argument required:
            c)  c_file=$OPTARG  ;;
            i)  case_in=$OPTARG  ;;
            o)  case_out=$OPTARG  ;;
            r)  range=$OPTARG  ;;
            d)  default_args=$OPTARG  ;;
	    a)  additional_args=$OPTARG  ;;
	    D)  valgrind_default_args=$OPTARG  ;;
	    A)  valgrind_additional_args=$OPTARG  ;;
            *)  die "Invalid argument option, run with -h to view available"  ;;
	esac
    done
}

assume_default_mode() {
    if [ $mode = nil ]; then
	[ -z "$range" ] && {
	    mode=s  # range not set, assume single
	    echo "> Notice: using mode Single (-s) by default."
	} || {
	    mode=m  # range set, assume multiple
	    echo "> Notice: using mode Multiple (-m) by default."
	}
    fi
}

args_error_checking() {
    [ -z "$c_file" ] && die "Missing -c (C_FILE) argument"
    [ -z "$case_in" ] && die "Missing -i (CASE_IN) argument"
    [ $valgrind = 0 ] &&
        [ -z "$case_out" ] && die "Missing -o (CASE_OUT) argument"
    case $mode in
        s)  [ -n "$range" ] && die "Arg -r (RANGE) not applicable in single mode"
            ;;
        m)  [ -z "$range" ] && die "Range not specified, use -r (RANGE)"
            basename "$case_in" | grep -q "@" || die "cannot find '@' in -i filename"
            basename "$case_out" | grep -q "@" || die "cannot find '@' in -o filename"
            ;;
    esac
    if [ $valgrind = 0 ]; then
        [ -n "$valgrind_default_args" ] && die "Arg -D not applicable without -V"
        [ -n "$valgrind_additional_args" ] && die "Arg -A not applicable without -V"
    fi
}

set_my_default_vars() {
    [ -z "$default_args" ] && default_args="$my_default_args"
    [ $valgrind = 1 ] &&
	[ -z "$valgrind_default_args" ] && valgrind_default_args="$my_valgrind_default_args"
}

prepare_exec() {
    bin="${c_file%.*}"
    compile_command="$compiler $default_args $additional_args -o $bin $c_file"
    case $valgrind in
	0)  run_command="./$bin" ;;
	1)  run_command="valgrind $valgrind_default_args $valgrind_additional_args ./$bin" ;;
    esac
}

prompt_exec() {
    echo "> compile command: $compile_command"
    echo "> run command: $run_command"
    read -n1 -p "Proceed (Y/n): " ans
    case "$ans" in
        [Yy]*|'' )  return 0 ;;   # Yes
        [Nn]* )  echo "Exiting..." ; exit 0 ;;
        *     )  echo "Invalid input, exiting..." ; exit 1 ;;
    esac
}

run_commands() {
    echo "Compiling..."
    $gcc_command || die "compilation failed"
    echo "Executing..."
    case $mode in
	s)  # single - exec and diff
	    run_output=`$run_command < case_in`
	    case_output=`cat $case_out`
	    echo "# Diff"
	    diff --color <(echo "$run_output") <(echo "$case_output")
	    echo "# Diff - end"
	    ;;
	m)  # multiple - exec and diff
	    beg=${range%-*} && end=${range#*-}
	    for i in $(seq $beg $end); do
		echo "# Case #$i"
		case_in_cur="$(dirname $case_in)/$(basename $case_in |tr '@' $i)"
		case_out_cur="$(dirname $case_in)/$(basename $case_out |tr '@' $i)"
		echo "# case_in: $case_in_cur"
		echo "# case_out: $case_out_cur"
		
		run_output=`./$exec < $case_in_cur`
		case_output=`cat $case_out_cur`
		diff --color <(echo "$run_output") <(echo "$case_output")
		echo "# Case #$i - end"
	    done
	    ;;
    esac
}

main() {
    # defines variable $mode and maybe $c_file, $infile, $outfile, $range, and $command_args
    process_args "$@"

    # if $mode is unset, set to most appropriate based on other args
    assume_default_mode "$@"

    # error checks args and resolves unspecified args
    args_error_checking "$@"

    # set default variable values if not specified ($my_default_args, $my_valgrind_default_args)
    set_my_default_vars "$@"

    # set exec commands ($compile_command, $run_command)
    prepare_exec "$@"

    # print the commands to run and prompt to proceed
    prompt_exec "$@"

    # run commands and show diff if appropriate
    run_and_diff "$@"

    echo "Done"
}

main "$@"
