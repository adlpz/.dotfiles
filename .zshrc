# Tmux ZSH plugin environment variables
#ZSH_TMUX_AUTOSTART=true

# oh-my-zsh
ZSH=$HOME/.oh-my-zsh
ZSH_CUSTOM=$HOME/.zsh
ZSH_THEME="adlpz"
plugins=(ssh-agent zsh-autosuggestions last-working-dir wd history-substring-search z fzf-z)
source $ZSH/oh-my-zsh.sh

# BROWSER
export BROWSER='/usr/bin/firefox-developer-edition'

# TERM
export TERM='xterm-256color'

# Banner
_print_zsh_banner

# Load ZSH colors
autoload -U colors && colors


# Custom prompt
#PROMPT=$'%{$fg_bold[blue]%}$(parse_git_dirty)$(git_prompt_info)%{$fg_bold[blue]%}%{$fg[white]%}%c%{$fg_bold[cyan]%} %#%{$reset_color%} '
#INITIAL_RPROMPT=$'[%{\e[0;31m%}%n%{\e[0;37m%}@%{\e[0;32m%}%m%{\e[0m%}]'
#RPROMPT=$INITIAL_RPROMPT

#ZSH_THEME_GIT_PROMPT_PREFIX=""
#ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
#ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}"
#ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%}"

# Path and other general variables are sourced from .profile
source $HOME/.profile

# $EDITOR
export EDITOR=$(which vim)

# vim mode
bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
bindkey '^[^[[D' backward-word
bindkey '^[^[[C' forward-word
export KEYTIMEOUT=1
function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[yellow]%} [% N]% %{$reset_color%}"
    RPROMPT="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $INITIAL_RPROMPT"
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# Extra keys
bindkey '^H' backward-kill-word # CTRL+Backspace delete word

# History substring search
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Homebrew
export HOMEBREW_GITHUB_API_TOKEN="6d172fc6b90506f5e375654e21aa7482e685f46b"

# Vagrant
vagrant_up_and_ssh() {
    vagrant status | sed -n 3p | grep -q running > /dev/null
    if [ $? -eq 1 ]; then
        vagrant up
    fi
    vagrant ssh
}

# Aliases
alias ls='ls -G --color=auto'
alias l='ls -lah'
alias did="vim +'normal Go' +'r!date' ~/did.txt"
alias xclip="xclip -selection c"
alias gs='git status'
alias gc='git commit'
alias gl='git log'
alias gd='git diff'
alias gds='git diff --staged'
alias gls="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias gll="gls -p"
alias nv="vim +NV"

# Project-specific aliases
alias crmassets='pushd ~/work/ydevs/ganaenergia/crm/design/web/js/pages/gamma_facturacion && yarn build --prod && popd'
alias crmsync='git checkout nueva-facturacion && git pull origin nueva-facturacion --ff-only && git pull && git push'

# History
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
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

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Helper functions
c () {
    echo "$@" | bc -l
}

# NPM
# Note: set global path with:
# npm config set prefix '~/.npm-global'
PATH=$PATH:~/.npm-global/bin

# Go
export GOROOT=/usr/lib/go
export GOBIN=$GOROOT/bin
export GOPATH=$HOME/golang
export PATH=$PATH:$GOBIN

# Ruby
RUBYPATH=$HOME/.gem/ruby/2.4.0/bin:$HOME/.gem/ruby/2.5.0/bin
export PATH=$PATH:$RUBYPATH

# NPM and others
export PATH=$PATH:$HOME/.local/bin

