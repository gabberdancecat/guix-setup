(define-module (src home misc aliases)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  
  #:export (my-misc-aliases))

(define my-misc-aliases
  '(("em" . "emacsclient")
    ;; general
    ("l" . "ls")
    ("ls" . "ls -p --color=auto")
    ("la" . "ls -a")
    ("ll" . "ls -lah --color=auto")
    ("rm" . "rm -i")
    ("ts" . "trash")
    ("b" . "cd ..")
    ("grep" . "grep --color=auto")
    ;; complex
    ("iping" . "ping gnu.org")
    ("recursive-find" . "grep -rnw . -e")
    ("clipboard" . "xclip -sel clip")
    
    ;; misc
    ("recompileurxvt" . "xrdb ~/.Xresources")
    ("ireconnect" . "ireconnect.sh")
    ("istatus" . "iwctl station wlan0 show | bat")
    ("iscan" . "iwctl station wlan0 scan && iwctl station wlan0 get-networks")
    ;; special
    ("Xephyr-start-server" . "Xephyr -br -ac -noreset -parent -screen -resizeable -verbosity 10 :1 &")
    ("Xephyr-start-wm" . "DISPLAY=:1")
    
    ;; uwuified
    ("pwease" . "sudo")
    
    ("meow" . "echo")
    ("mew" . "echo")
    ("pwint" . "printf")
    
    ("nya" . "ls")
    ("nyan" . "ll")
    ("nyaa" . "la")
    
    ("maw" . "mkdir")
    ("vore" . "rm")
    ("chomp" . "rmdir")
    
    ("gwep" . "grep")
    ("lowocate" . "find")
    
    ("pets" . "head")
    
    ("halp" . "man")
    ("weh" . "awk")
    ("snek" . "python")
    
    ("owo" . "if")
    ("ewe" . "else")
    ("uwu" . "done")
    
    ("pounces" . "for")
    ("on" . "in")
    
    ("nuzzles" . "while")
    
    ("duwu" . "do")
    
    ("glomps" . "exit")))
