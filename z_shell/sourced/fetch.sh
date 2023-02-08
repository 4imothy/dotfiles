#!/bin/bash

# local black="\033[30m█\033[0m"
# local red="\033[31m█\033[0m"
# local green="\033[32m█\033[0m"
# local yellow="\033[33m█\033[0m"
# local blue="\033[34m█\033[0m"
# local magenta="\033[35m█\033[0m"
# local cyan="\033[36m█\033[0m"
# local white="\033[37m█\033[0m"
# local cpu=$(top -l 1 | grep "^CPU")
# local user=$(echo $cpu | awk '{print $3}')
# local system=$(echo $cpu | awk '{print $5}')
# local idle=$(echo $cpu | awk '{print $7}')
# local mem=$(top -l 1 | grep "^Phys")
# local used=$(echo $mem | awk '{print $2}')
# local unused=$(echo $mem | awk '{print $8}') 

# # echo -e $black$red$green$yellow$blue$magenta$cyan$white
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


fetch(){
  local black="\033[30m█\033[0m"
  local red="\033[31m█\033[0m"
  local green="\033[32m█\033[0m"
  local yellow="\033[33m█\033[0m"
  local blue="\033[34m█\033[0m"
  local magenta="\033[35m█\033[0m"
  local cyan="\033[36m█\033[0m"
  local white="\033[37m█\033[0m"
  local cpu=$(top -l 1 | grep "^CPU")
  local user=$(echo $cpu | awk '{print $3}')
  local system=$(echo $cpu | awk '{print $5}')
  local idle=$(echo $cpu | awk '{print $7}')
  local mem=$(top -l 1 | grep "^Phys")
  local used=$(echo $mem | awk '{print $2}')
  local batt_percent=$(pmset -g batt | grep -E "([0-9]+\%).*" -o | cut -f1 -d';')
  local total_mem=$(system_profiler SPHardwareDataType | grep "Memory:" | awk '{print($2$3)}')
  local chip=$(system_profiler SPHardwareDataType | grep "Chip:" | awk '{print($2$3$4)}')
  local resolution=$(system_profiler SPDisplaysDataType |grep Resolution | awk '{print($2 $3 $4)}')
  local shell=$($SHELL --version)
  local cores=$(sysctl -n hw.ncpu)
  local kern=$(sysctl kern.version | awk '{print $2,$3,$4,$5}')
  echo -e "        __________________            User: $(whoami)"
  echo -e "      .'------------------'.          CPU Architecture: $(arch)"
  echo -e "      | __________________ |          Chip: $chip"
  echo -e "      | |$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white| |          # Cores:$cores"
  echo -e "      | |$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan| |         Shell: $shell"
  echo -e "      | |$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta| |          Kernel: $kern"
  echo -e "      | |$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow$blue| |          Os Type:$OSTYPE"
  echo -e "      | |$blue$magenta$cyan$white$black$red$green$yellow$blue$magenta$cyan$white$black$red$green$yellow| |          Resolution: $resolution"
  echo -e "+-----| ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾ |-----+    Battery: $batt_percent"
  echo -e "|     '--------.-.---------'     |    Memory: $used / $total_mem"
  echo -e "|        ______|_|_______        |    CPU Usage: User: $user, System: $system, idle: $idle"
  echo -e "|       /  pyfgcrlaoeui  \\       |"
  echo -e "|      /  dhtnsqjkxbmwvz  \\      |"
  echo -e "|      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾      |"
  echo -e "+--------------------------------+"
}