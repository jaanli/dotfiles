## Dotfiles management

Modified from [Atlassian and Hacker News](https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/) to work with submodules in github.

Initial setup:
```
git init --bare $HOME/.cfg
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
config config --local status.showUntrackedFiles no
# or add this to .zshrc
echo "alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'" >> $HOME/.bashrc
```

Then to add stuff, we can do:
```
config add .zshrc
config commit -m 'added zshrc'
config remote set-url origin git@github.com:altosaar/dotfiles.git
config push
```

On a new computer:
```
# locally
ssh-copy-id user@host
# install zsh and other things
sudo apt-get install zsh autojump keychain
# make zsh default, install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
# may need to logout
exit
# locally
ssh -O exit user@host
# login again; shell should change
# clone the repo
git clone --bare git@github.com:altosaar/dotfiles.git $HOME/.cfg
# add to zshrc or bashrc
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
# add to gitignore
echo ".cfg" >> .gitignore
config checkout
config config --local status.showUntrackedFiles no
# IMPORTANT: this pulls the latest plugins we are using, see below
config submodule update --init
# set git variables
git config --global user.email blah
git config --global user.name "Jaan Altosaar"
# generate a new key
ssh-keygen -t rsa -b 4096 -C "your_email@example"
source ~/.zshrc
# update & upgrade
sudo apt-get update
sudo apt-get upgrade
# install latest tmux
lsb_release -a  # then follow: https://gist.github.com/P7h/91e14096374075f5316e
sudo apt-get install tmux-next=2.3~20161117~bzr3621+20-1ubuntu1~ppa0~ubuntu14.04.1	# get this from https://launchpad.net/~pi-rho/+archive/ubuntu/dev
```

Random commands that may be necessary on a new machine:
```
# generate a new ssh key for adding to github ssh keys
ssh-keygen -t rsa -b 4096 -C "jaan.altosaar@gmail.com"
cat ~/.ssh/id_rsa.pub
# set default git config
git config user.name "Jaan Altosaar"
git config --global user.email "jaan.altosaar@gmail.com"
# set a remote
config remote set-url origin git@github.com:altosaar/dotfiles.git
# install oh-my-zsh
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
# set correct locale
sudo locale-gen "en_US.UTF-8"
# install latest tmux (required for scrolling and proper copying in panes): https://gist.github.com/P7h/91e14096374075f5316e/ffef0c44907f3f247aab4d0888d116e85eb5c072 with correct URL from https://launchpad.net/~pi-rho/+archive/ubuntu/dev
```

Hiccups:
* if the latest tmux/vim can't be installed (e.g. on AWS), oh-my-zsh autocompletion in tmux may fail for some themes/trailing dots

### Managing plugins with pathogen and tmux plugin manager
To install a plugin, we need to tell git that it is a submodule. Adding a folder recursively that contains git repositories (submodules) will not work. After adding all submodules/plugins we need, we can pull them all at once. The dotfiles repo then needs to be cloned with the `--recursive` option, or we can run `git submodule update --init` as above.
```
cd ~/.vim/bundle
config submodule add https://github.com/tpope/vim-repeat
config submodule foreach git pull
```
