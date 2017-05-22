# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"
# ZSH_THEME="agnoster" # (this is one of the fancy ones)

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias cpwd="pwd | tr -d '\n' | pbcopy"
alias s="subl $@"
alias zc="subl ~/.zshrc"
alias ..="cd .."
alias p="python $@"
alias rs="rsync -az --progress $@"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="false"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git autojump ruby tmux)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

#define history search fn
hgrep () {
	history | egrep --color=auto --recursive "$@" | egrep --color=auto --recursive -v "hgrep $@"
}


# This way the completion script does not have to parse Bazel's options
# repeatedly.  The directory in cache-path must be created manually.
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# for dotfiles config
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# for getting full paths
alias rf="readlink -f $@"

# for powerline theme
export TERM="xterm-256color"

# for vim everywhere
export EDITOR="vim"



if [[ "$USER" == "jaanaltosaar" ]];
then
	export PATH="/usr/local/anaconda3/bin:$PATH"
	alias rf="realpath $@"
	[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh
	# export PATH=/usr/local/bin:/usr/local/sbin:/bin:/sbin:/usr/bin:$PATH
	source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
	# for rbenv, ruby, jekyll
	if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
elif [[ "$USER" == "jaan" ]];
then
	. /usr/share/autojump/autojump.sh
	# Setup CUDA
	export DYLD_LIBRARY_PATH=/usr/local/cuda-7.5/lib/:/usr/local/cuda-7.5/lib64/
	export LD_LIBRARY_PATH=/usr/local/cuda-7.5/lib:/usr/local/cuda-7.5/lib64/
	export CUDA_HOME=/usr/local/cuda-7.5
	export PATH=$PATH:/usr/local/cuda-7.5/bin/
	# Add my stuff
	export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/usr/local/lib/
	export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib/
	# only let tensorflow see gpu:1
	export CUDA_VISIBLE_DEVICES=1
	# Configuration for ~/.bash_profile, ~/.zshrc etc:
	# # Pipe anything into `clip` to forward it to Clipper
	alias clip="nc localhost 8377"
	# Or, if you are running Clipper on a UNIX domain socket:
	#alias clip="nc -U ~/.clipper.sock"
elif [[ "$UID" == "0" ]];
then
	source /usr/share/autojump/autojump.zsh
fi

# for managing ssh keys and not having to type in key pass all the time
keychain id_rsa
. ~/.keychain/`uname -n`-sh
#eval $(keychain --eval --agents ssh id_rsa)

bindkey -e
bindkey '^[[1;9C' forward-word
bindkey '^[[1;9D' backward-word

# export LC_ALL=en_US.UTF-8
# export LC_CTYPE=en_US.UTF-8
# export LANG=en_US.UTF-8

export POWERLINE_CONFIG_COMMAND=powerline-config
export POWERLINE_COMMAND=powerline

source ~/.experimentrc
