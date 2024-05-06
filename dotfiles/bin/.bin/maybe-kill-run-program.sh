#!/bin/sh

# (1,launch-cmd) (2,target-pid-count[opt]) (3,pgrep-name[opt])

die() {
    echo "$@."
    echo "Args:"
    echo "1.launch-cmd"
    echo "2.target-pid-count[opt]"
    echo "3.pgrep-name[opt]"
    echo
    echo "exiting..."
    exit 1
}

[ $# -lt 1 ] && die "need at least 1 arg"

# eg: "/bin/sh -c $HOME/.config/river/status"
launch_cmd="$1"

# eg: 1
target_count="${2:-1}"

# eg: "sh.*\.config/river/status"
search="${3:-^sh $1}"

echo "DEBUG: ($launch_cmd) ($target_count) ($search)"

# kill newest pids until reach target_count - 1

p_count="$(pgrep -fc "$search")"
while [ $p_count -ge $target_count ]; do
    newest_pid="$(pgrep -fn "$search")"
    echo "DEBUG: $(pgrep -fa "$search")"
    echo "$p_count. $newest_pid"
    kill $newest_pid
    # increment
    p_count="$(pgrep -fc "$search")"
done

# 
