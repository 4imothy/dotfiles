export COLOR_RED="$(tput setaf 1)"
export COLOR_RESET="$(tput sgr0)"

exec 9>&2
exec 8> >(
    perl -e '$|=1; while(sysread STDIN,$a,9999) {print "$ENV{COLOR_RED}$a$ENV{COLOR_RESET}"}'
)
function undirect(){ exec 2>&9; } # reset to original 9 (==2)
function redirect(){ exec 2>&8; } # set to custom 8
trap "redirect;" DEBUG
PROMPT_COMMAND='undirect;'
