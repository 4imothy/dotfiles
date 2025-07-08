module completions {

  def "nu-complete tgrep searcher" [] {
    [ "rg" "tgrep" "ripgrep" "treegrep" ]
  }

  def "nu-complete tgrep char-style" [] {
    [ "ascii" "single" "double" "heavy" "rounded" "none" ]
  }

  def "nu-complete tgrep open-like" [] {
    [ "vi" "hx" "code" "jed" "default" ]
  }

  def "nu-complete tgrep completions" [] {
    [ "bash" "elvish" "fish" "powershell" "zsh" ]
  }

  # pattern matcher that displays results in a tree structure with an interface to jump to matched text
  export extern tgrep [
    positional regexp?: string # the regex expression
    --regexp(-e): string      # the regex expression
    --path(-p): path          # the path to search, if not provided, search the current directory
    --glob: string            # rules match .gitignore globs, but ! has inverted meaning, overrides other ignore logic
    --searcher: string@"nu-complete tgrep searcher" # executable to do the searching
    --char-style: string@"nu-complete tgrep char-style" # style of characters to use
    --editor: string          # command used to open selections
    --open-like: string@"nu-complete tgrep open-like" # command line syntax for opening a file at a line
    --long-branch             # multiple files from the same directory are shown on the same branch
    --completions: string@"nu-complete tgrep completions" # generate completions for given shell
    --plugin-support
    --selection-file: path    # file to write selection to (first line: file path, second line: line number if applicable)
    --repeat-file: path       # file where arguments are saved
    --hidden(-.)              # search hidden files
    --repeat                  # repeats the last saved search
    --line-number(-n)         # show line number of match
    --files(-f)               # if a pattern is given hide matched content, otherwise show the files that would be searched
    --links                   # search linked paths
    --no-ignore               # don't use ignore files
    --count(-c)               # display number of files matched in directory and number of lines matched in a file
    --no-color                # don't use colors
    --no-bold                 # don't bold anything
    --overview                # conclude results with an overview
    --select(-s)              # results are shown in a selection interface for opening
    --menu                    # provide arguments and select result through an interface
    --trim                    # trim whitespace at the beginning of lines
    --pcre2                   # enable PCRE2
    --threads: string         # set the appropriate number of threads to use
    --max-depth: string       # the max depth to search
    --prefix-len: string      # number of characters to show before a match
    --max-length: string      # set the max length for a matched line
    --long-branch-each: string # number of files to print on each branch
    --help(-h)                # Print help
    --version(-V)             # Print version
  ]

}

export use completions *
