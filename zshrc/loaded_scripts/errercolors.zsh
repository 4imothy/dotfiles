exec 9>&2
exec 8> >(
    while IFS='' read -r line || [ -n "$line" ]; do
       echo -e "\033[31m${line}\033[0m"
    done
)
function undirect(){ exec 2>&9; }
function redirect(){ exec 2>&8; }
trap "redirect;" DEBUG
PROMPT_COMMAND='undirect;'
