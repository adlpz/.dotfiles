# oh-my-zsh
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"
plugins=(git)
source $ZSH/oh-my-zsh.sh

# Custom prompt
PROMPT=$'%{$fg_bold[blue]%}$(parse_git_dirty)$(git_prompt_info)%{$fg_bold[blue]%}%{$fg[white]%}%c%{$fg_bold[cyan]%} %#%{$reset_color%} '
RPROMPT=$'[%{\e[0;31m%}%n%{\e[0;37m%}@%{\e[0;32m%}%m%{\e[0m%}]'

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%}"

# Path
export PATH="/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/Users/adria/tools/bin:/usr/local/mysql-5.5.28-osx10.6-x86_64/bin/:/usr/local/Cellar/php54/5.4.9/bin/"

# Homebrew
export HOMEBREW_GITHUB_API_TOKEN="6d172fc6b90506f5e375654e21aa7482e685f46b"

# Aliases
alias ls='ls -G'
alias l='ls -l'
alias v="vagrant"

# History
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob

# keys
bindkey "\e[3~" delete-char

# No duplicates when searching history
setopt HIST_FIND_NO_DUPS

# completion.zsh: Directives for the Z-Shell completion system.
# P.C. Shyamshankar <sykora@lucentbeing.com>

autoload -Uz compinit && compinit

zstyle ':completion:*' list-colors "${LS_COLORS}" # Complete with same colors as ls.

# Fuzzy matching of completions for when you mistype them:
zstyle ':completion:*' completer _expand _complete _correct _approximate # Completion modifiers.
zstyle ':completion:*:match:*' original only
zstyle ':completion:*' max-errors 1 # Be lenient to 1 errors.

# And if you want the number of errors allowed by _approximate to increase with the length of what you have typed so far:
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# Ignore completion functions for commands you don?~@~Yt have:
zstyle ':completion:*:functions' ignored-patterns '_*'

# Ignore the current directory in completions
zstyle ':completion:*' ignore-parents pwd

# Use a completion cache
zstyle ':completion:*' use-cache true
zstyle ':completion:*' cache-path /.zsh/cache

# Completing process IDs with menu selection:
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

# If you end up using a directory as argument, this will remove the trailing slash (usefull in ln)
zstyle ':completion:*' squeeze-slashes true

# Sudo completion
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
