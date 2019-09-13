# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Theme
ZSH_THEME="ys"

# Aliases
alias cpwd="pwd | tr -d '\n' | pbcopy"
alias s="subl $@"
alias zc="subl ~/.zshrc"
alias ..="cd .."
alias p="python $@"
alias rs="rsync -az --progress $@"

# zsh functions
fpath=( ~/.zfunc "${fpath[@]}" )
autoload -Uz stanmake
# define history search fn
hgrep () {
    history | egrep --color=auto --recursive "$@" | egrep --color=auto --recursive -v "hgrep $@"
}

# red dots to displayed while waiting for completion
COMPLETION_WAITING_DOTS="false"

# Plugins
plugins=(git autojump ruby tmux docker)

source $ZSH/oh-my-zsh.sh

# for dotfiles config
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# for getting full paths
alias rf="readlink -f $@"

# for powerline theme
export TERM="xterm-256color"

# for vim everywhere
export EDITOR="vim"

# add key to keychain on login
#eval $(keychain --eval --agents ssh id_rsa)

if [[ "$SHORT_HOST" == "siilipoiss" ]];
then
    source ~/.secrets
    [[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh
    export PATH=/usr/local/bin:/usr/local/sbin:/bin:/sbin:/usr/bin:$PATH
    export PATH="/usr/local/anaconda3/bin:$PATH"
    # alias for readlink (brew install coreutils)
    alias readlink=greadlink
elif [[ "$HOST" == "beaker" ]];
then
    . /usr/share/autojump/autojump.sh
    export PATH=/home/jaan/miniconda3/bin:$PATH
elif [[ "$HOST" == "tiger1" ]];
then
    export PATH=/home/altosaar/bin:$PATH
elif [[ "$HOST" == "adroit4"]];
then
    [[ -s /home/altosaar/.autojump/etc/profile.d/autojump.sh ]] && source /home/altosaar/.autojump/etc/profile.d/autojump.sh
    autoload -U compinit && compinit -u
fi

# keybindings for alt arrow keys to work for navigating shell lines
bindkey -e
bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line
bindkey '^[[1;9C' forward-word
bindkey '^[[1;9D' backward-word

# locale options
export LC_ALL=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US:en

export POWERLINE_CONFIG_COMMAND=powerline-config
export POWERLINE_COMMAND=powerline

# environment variables LOG, DAT for data and log files - unique to a server
source ~/.experimentrc
