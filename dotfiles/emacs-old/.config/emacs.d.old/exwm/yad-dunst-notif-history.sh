#!/bin/sh

# yad --notification --no-middle --text="text tooltip" --listen \
#     --command='
if (($(dunstctl count displayed) == 0));
then
    for i in {1..5};
    do
	$(dunstctl history-pop);
    done;
else
    $(dunstctl close-all);
fi

