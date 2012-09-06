

# Custom prompt
PROMPT=$'%{\e[1;31m%}%~ %{\e[0;36m%}%(!.#.$)%{\e\[0m%} '
RPROMPT=$'[%{\e[0;31m%}%n%{\e[0;37m%}@%{\e[0;32m%}%m%{\e[0m%}]'

# Aliases
alias ls='ls --color'
alias l='ls -l'

export PATH=$PATH:/home/adria/toolchain/i386-elf/bin/:/home/adria/.gem/ruby/1.9.1/bin

# The following lines were added by compinstall

zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' original true
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle :compinstall filename '/home/adria/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob
bindkey -v
# End of lines configured by zsh-newuser-install


# keys
bindkey "\e[3~" delete-char

# dircolors
eval "$(dircolors ~/.config/dircolors)"
