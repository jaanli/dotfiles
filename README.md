## Dotfiles management

Modified from [Atlassian and Hacker News](https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/) to work with submodules in github.

```
git init --bare $HOME/.cfg
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
config config --local status.showUntrackedFiles no
# or zshrc
echo "alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'" >> $HOME/.bashrc
```

On a new computer:
```
# add to zhrc or bashrc
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
echo ".cfg" >> .gitignore
git clone --bare <git-repo-url> $HOME/.cfg
config checkout
config config --local status.showUntrackedFiles no
# IMPORTANT: this pulls the latest plugins we are using
config submodule update --init
```

May need:
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
```

### Managing plugins with pathogen and tmux plugin manager
To install a plugin:
```
cd ~/.vim/bundle
config submodule add https://github.com/tpope/vim-repeat
```
