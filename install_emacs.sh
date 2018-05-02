#!/bin/bash

# Original Source: https://github.com/favadi/build-emacs

set -eu

readonly version="25.2"
install_directory="/usr/local/stow"

# install dependencies
sudo apt-get -qq update
sudo apt-get -qq install -y stow build-essential libx11-dev xaw3dg-dev \
     libjpeg-dev libpng12-dev libgif-dev libtiff5-dev libncurses5-dev \
     libxft-dev librsvg2-dev libmagickcore-dev libmagick++-dev \
     libxml2-dev libgpm-dev libotf-dev libm17n-dev \
     libgnutls-dev wget

cd /tmp

# download source package
if [[ ! -d emacs-"$version" ]]; then
    wget http://ftp.gnu.org/gnu/emacs/emacs-"$version".tar.xz
    tar xvf emacs-"$version".tar.xz
    rm emacs-"$version".tar.xz
fi

# build and install
sudo mkdir -p ${install_directory}
cd emacs-"$version"
./configure --with-xft --with-x-toolkit=lucid

make
sudo make install-arch-dep install-arch-indep prefix="${install_directory}"/emacs-"$version"

cd ${install_directory}
sudo stow emacs-"$version"

rm -r emacs-"$version"

# install source code pro font
mkdir -p ~/.fonts
cd ~/.fonts
wget 'https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.tar.gz'
tar xzf 1.050R-it.tar.gz
rm 1.050R-it.tar.gz
sudo fc-cache -f -v

echo "[Unit]
Description=Emacs: the extensible, self-documenting text editor

[Service]
Type=forking
ExecStart=/usr/local/bin/emacs --daemon
ExecStop=/usr/local/bin/emacsclient --eval \"(kill-emacs)\"
Environment=SSH_AUTH_SOCK=/run/user/1000/keyring/ssh
Restart=always
User=%i

TimeoutStartSec=0

[Install]
WantedBy=default.target" | sudo tee /etc/systemd/system/emacs@.service

sudo systemctl enable emacs@$USER
sudo systemctl start emacs@$USER
