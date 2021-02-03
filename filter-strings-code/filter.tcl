
set filename [lindex $argv 0]
set filters  [split [lindex $argv 1] ";"]

set lines [split [read [open $filename "r"]] "\n"]

foreach line $lines {
        set newline $line
        foreach filter $filters {
                set newline [string map [list $filter ""] $newline]
        }
        
        if { [string compare $line $newline] == 0 } {
                puts $line
        }       
}


