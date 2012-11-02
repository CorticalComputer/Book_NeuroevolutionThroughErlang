save "$0.gp" #Save commands to file
set t push #Store current terminal settings
set t png $1 #Set terminal type to PNG, taking additional options from second argument
set o "$0.png" #Set output file name
replot #Generate plot
set o #Restore output to interactive terminal
set t pop #Restore interactive terminal settings

