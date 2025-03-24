$env.TODO_FILE = "$HOME/Documents/org/tasks.org"
$env.TREEGREP_DEFAULT_OPTS = "--glob=!.git --hidden --char-style=rounded"
$env.EDITOR = "nvim"
$env.NOTES_EDITOR = "nvim"
let path_dirs = [
    "/opt/homebrew/bin"
    $"($env.HOME)/bin"
    $"($env.HOME)/.cargo/bin"
    $"($env.HOME)/.go/bin"
    "/opt/homebrew/opt/qt5/bin"
]

$env.PATH = ($env.PATH | split row (char esep) | prepend $path_dirs | uniq)
$env.GOPATH = $"($env.HOME)/.go"
$env.JAVA_HOME = ("/usr/libexec/java_home" | str trim)
$env.CPPFLAGS = "-I/opt/homebrew/opt/openjdk/include"

$env.PROMPT_INDICATOR = {|| "  " }
