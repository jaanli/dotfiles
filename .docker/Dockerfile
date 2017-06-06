# Fixed ubuntu version
FROM ubuntu:16.04

# Install Ruby and Rails dependencies
RUN apt-get update && apt-get install -y \
  tmux \
  gdb \
  gcc \
  g++ \
  python \
  pkg-config \
  clang-format \
  libgsl2 \
  libgsl-dev \
  libarmadillo6 \
  libarmadillo-dev \
  libboost-all-dev \
  vim-nox \
  tmux \
  zsh \
  autojump \
  keychain \
  sudo \
  git \
  build-essential \
  cmake \
  python-dev \
  python3-dev



# Set neovim as default editor
# RUN update-alternatives --install /usr/bin/editor editor /usr/bin/vim 60 && \
#     update-alternatives --config editor && \
#     update-alternatives --install /usr/bin/vi vi /usr/bin/vim 60 && \
#     update-alternatives --config vi && \
#     update-alternatives --install /usr/bin/vim vim /usr/bin/vim 60 && \
#     update-alternatives --config vim

# Add the user, with passwordless sudo
RUN useradd --user-group \
            --create-home \
            --shell /usr/bin/zsh \
            jaan && \
    echo 'jaan ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers

# Switch to user and home directory
USER jaan
ENV HOME /home/jaan
WORKDIR /home/jaan

# Setup for ssh onto github
RUN mkdir -p $HOME/.ssh
ADD id_rsa $HOME/.ssh/id_rsa
ADD id_rsa.pub $HOME/.ssh/id_rsa.pub
RUN sudo chown jaan:jaan $HOME/.ssh
RUN sudo chmod 777 $HOME/.ssh/id_rsa
RUN sudo chmod 777 $HOME/.ssh/id_rsa.pub
RUN echo "Host github.com\n\tStrictHostKeyChecking no\n" >> $HOME/.ssh/config
RUN ssh-keyscan -t rsa github.com 2>&1 >> $HOME/.ssh/known_hosts

# Install oh-my-zsh
RUN git clone git://github.com/robbyrussell/oh-my-zsh.git .oh-my-zsh

RUN git config --global user.email jaan.altosaar@gmail.com && \
    git config --global user.name altosaar

# Clone dotfiles
RUN git clone --bare git://github.com/altosaar/dotfiles.git $HOME/.cfg
RUN echo "alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'" >> $HOME/.zshrc
# need to run config submodule update --init from within docker
# RUN /usr/bin/zsh -c "source $HOME/.zshrc"

# vim
RUN git clone https://github.com/VundleVim/Vundle.vim.git $HOME/.vim/bundle/Vundle.vim
# need to run :PluginInstall from vim
# RUN vim -c ':PluginInstall' -c 'qa!'

# Share the work volume
VOLUME ["/home/jaan"]

# Run zsh as login shell
ENTRYPOINT ["/usr/bin/zsh"]
CMD ["--login"]
