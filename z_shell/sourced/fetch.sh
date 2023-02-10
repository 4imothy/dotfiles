#!/bin/sh
  
fetch(){
    local black="\033[30m█\033[0m"
    local red="\033[31m█\033[0m"
    local green="\033[32m█\033[0m"
    local yellow="\033[33m█\033[0m"
    local blue="\033[34m█\033[0m"
    local magenta="\033[35m█\033[0m"
    local cyan="\033[36m█\033[0m"
    local white="\033[37m█\033[0m"

  print_all(){ 
    echo -e "        __________________            User: $(whoami)"
    echo -e "      .'------------------'.          CPU Architecture: $(arch)"

    local chip=$(echo $system_hardware | grep "Chip:" | awk '{print($2$3$4)}')
    echo -e "      | __________________ |          Chip: $chip"

    local cores=$(sysctl -n hw.ncpu)
    echo -e "      | |$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white| |          # Cores:$cores"

    local kern=$(sysctl kern.version | awk '{print $2,$3,$4,$5}')
    echo -e "      | |$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan| |          Kernel: $kern"

    local shell=$($SHELL --version | awk 'NR==1 {print($1,$2,$3)}')
    echo -e "      | |$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta| |          Shell: $shell" 

    echo -e "      | |$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue| |          Os Type:$OSTYPE"

    local resolution=$(system_profiler SPDisplaysDataType |grep Resolution | awk '{print($2 $3 $4)}')
    echo -e "      | |$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow| |          Resolution: $resolution"

    local batt_percent=$(pmset -g batt | grep -E "([0-9]+\%).*" -o | cut -f1 -d';')
    echo -e "+-----| ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾ |-----+    Battery: $batt_percent"

    local system_hardware=$(system_profiler SPHardwareDataType)
    local total_mem=$(echo $system_hardware | grep "Memory:" | awk '{print($2$3)}') 
    local cpu_mem=$(top -l 1 | grep -E "^CPU|^Phys")
    local used=$(echo $cpu_mem | awk 'NR == 2 {print $2}')
    echo -e "|     '--------.-.---------'     |    Memory: $used / $total_mem"

    local user=$(echo $cpu_mem | awk 'NR == 1 {print $3}')
    local system=$(echo $cpu_mem | awk 'NR == 1{print $5}')
    local idle=$(echo $cpu_mem | awk 'NR == 1 {print $7}')
    echo -e "|        ______|_|_______        |    CPU Usage: User: $user, System: $system, idle: $idle"
    echo -e "|       /  pyfgcrlaoeui  \\       |"
    echo -e "|      /  dhtnsqjkxbmwvz  \\      |"
    echo -e "|      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾      |"
    echo -e "+--------------------------------+"
    unset -f print_all
  } 
  print_none(){
    echo -e "        __________________"   
    echo -e "      .'------------------'."
    echo -e "      | __________________ |"
    echo -e "      | |$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white| |"
    echo -e "      | |$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan| |"
    echo -e "      | |$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta| |"
    echo -e "      | |$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue| |"
    echo -e "      | |$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow| |"
    echo -e "+-----| ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾ |-----+"
    echo -e "|     '--------.-.---------'     |"
    echo -e "|        ______|_|_______        |"
    echo -e "|       /  pyfgcrlaoeui  \\       |"
    echo -e "|      /  dhtnsqjkxbmwvz  \\      |"
    echo -e "|      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾      |"
    echo -e "+--------------------------------+"
    unset -f print_none
  }

  print_some(){
    echo -e "        __________________            User: $(whoami)"
    echo -e "      .'------------------'.          CPU Architecture: $(arch)"

    local shell=$($SHELL --version | awk 'NR==1 {print($1,$2,$3)}')
    echo -e "      | __________________ |          Shell: $shell"

    echo -e "      | |$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white| |          Os Type:$OSTYPE"

    local batt_percent=$(pmset -g batt | grep -E "([0-9]+\%).*" -o | cut -f1 -d';')
    echo -e "      | |$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan| |          Battery: $batt_percent"

    local cpu_mem=$(top -l 1 | grep -E "^CPU|^Phys")
    local user=$(echo $cpu_mem | awk 'NR == 1 {print $3}')
    local system=$(echo $cpu_mem | awk 'NR == 1{print $5}')
    local idle=$(echo $cpu_mem | awk 'NR == 1 {print $7}')
    local used=$(echo $cpu_mem | awk 'NR == 2 {print $2}')
    echo -e "      | |$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta| |          Memory Used: $used"
    echo -e "      | |$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue| |          CPU Usage: User: $user, System: $system, idle: $idle"
    echo -e "      | |$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow| |"
    echo -e "+-----| ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾ |-----+"
    echo -e "|     '--------.-.---------'     |"
    echo -e "|        ______|_|_______        |"
    echo -e "|       /  pyfgcrlaoeui  \\       |"
    echo -e "|      /  dhtnsqjkxbmwvz  \\      |"
    echo -e "|      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾      |"
    echo -e "+--------------------------------+"
    unset -f print_some
  }

  if [ $# -ne 1 ]; then # if there are no flags, default case
    print_all
  fi
  while getopts ":s :n" opt; do
    case $opt in
      s)
        print_some
        ;;
      n)
        print_none
        ;;
      \?)
        echo "Invalid option: -$OPTARG" >&2
        ;;
      :)
        print_all
        ;;
    esac
  done
}
# other art
# echo -e "           ______________ "
# echo -e "          /             /|User: $(whoami)"
# echo -e "         /             / |CPU Architecture: $(arch)"
# echo -e "        /____________ /  |Battery: $(pmset -g batt | grep -E "([0-9]+\%).*" -o | cut -f1 -d';')"
# echo -e "       | ___________ |   |OS Type: $OSTYPE"
# echo -e "       ||$black$red$green$yellow$blue$magenta$cyan$white$black$red$green||   |CPU Usage:"
# echo -e "       ||$green$black$red$green$yellow$blue$magenta$cyan$white$black$red||   |         User: $user"
# echo -e "       ||$red$green$black$red$green$yellow$blue$magenta$cyan$white$black||   |         System: $system"
# echo -e "       | ‾‾‾‾‾‾‾‾‾‾‾ |   |         Idle: $idle"
# echo -e "       |   _______   |  / Memory:"
# echo -e "      /|  (_______)  | /        Used: $used"
# echo -e "     ( |_____________|/         Unused: $unused"
# echo -e "      \\"
# echo -e " .=======================."
# echo -e " | ::::::::::::::::  ::: |"
# echo -e " | ::::::::::::::[]  ::: |"
# echo -e " |   -----------     ::: |"
# echo -e " '-----------------------'"
