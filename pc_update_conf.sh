#!/bin/bash

set -eu

mkdir -p "${HOME}"/log

# Install go
sudo add-apt-repository ppa:longsleep/golang-backports -y

sudo apt-get update
sudo apt-get dist-upgrade -y
sudo apt-get install -y git python3 python3-pip python-dev build-essential \
     curl ffmpeg imagemagick astyle golang-go

# Install go packages
go get github.com/rogpeppe/godef
go get golang.org/x/tools/cmd/...
go get -u github.com/nsf/gocode

# Install emacs
git clone https://github.com/AlorsCoings/emacs-configuration "${HOME}/.emacs.d"
bash "${HOME}/.emacs.d/install_emacs.sh"

# Install docker
sudo apt-get install -y apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository \
     "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
       $(lsb_release -cs) \
       stable"
sudo apt-get update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io
sudo -H usermod -aG docker ${USER}

# Install docker-compose
sudo curl -L https://github.com/docker/compose/releases/download/1.20.1/docker-compose-$(uname -s)-$(uname -m) -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# Upgrade pip
pip3 install --upgrade pip

# Make python3 default
sudo rm "`which python`"
sudo ln -s "`which python3`" "`which python`"

# Install google chrome
sudo sh -c 'echo "deb [arch=amd64] https://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list'
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
sudo apt-get update
sudo apt-get install google-chrome-stable

# Install verson 10 of nodejs
curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash -
sudo apt-get install -y nodejs

# Install python packages
sudo -H pip3 install cpplint tensorflow pandas pylint \
     yapf autopep8 jedi flake8 rope_py3k

# Install nvidia drivers
sudo ubuntu-drivers autoinstall

# Google cpplint c++ checker
sudo wget 'https://raw.githubusercontent.com/google/styleguide/gh-pages/cpplint/cpplint.py' -O /usr/local/bin/cpplint.py

sudo chmod a+x /usr/local/bin/cpplint.py

# Keyboard layout
gsettings set org.gnome.desktop.input-sources show-all-sources true
gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'fr+bepo'), ('xkb', 'fr'), ('xkb', 'us')]"

# More info in clock
gsettings set org.gnome.desktop.interface clock-show-date true
gsettings set org.gnome.desktop.interface clock-show-seconds true
gsettings set org.gnome.desktop.interface clock-show-weekday true

gsettings set org.gnome.desktop.sound event-sounds false

# Handle power setup
gsettings set org.gnome.settings-daemon.plugins.power lid-close-ac-action 'nothing'
gsettings set org.gnome.settings-daemon.plugins.power lid-close-battery-action 'nothing'
gsettings set org.gnome.desktop.screensaver lock-enabled false
gsettings set org.gnome.desktop.session idle-delay 0
gsettings set org.gnome.settings-daemon.plugins.power idle-dim false
gsettings set org.gnome.desktop.interface show-battery-percentage true

# Remove <Super> activities
gsettings set org.gnome.mutter overlay-key ''

# Custom key-bindings
gsettings set org.gnome.settings-daemon.plugins.media-keys screencast "''"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-down "['<Primary><Super>s']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-left "[]"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-right "[]"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-up "['<Primary><Super>d']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-down "['<Shift><Control><Super>s']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-left "[]"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-right "[]"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-up "['<Shift><Control><Super>d']"
gsettings set org.gnome.mutter workspaces-only-on-primary false

gsettings set org.gnome.desktop.wm.keybindings panel-main-menu "[]"
gsettings set org.gnome.desktop.wm.keybindings show-desktop "[]"
gsettings set org.gnome.settings-daemon.plugins.media-keys screensaver '<Super>l'

gsettings set org.gnome.desktop.wm.keybindings begin-move "[]"
gsettings set org.gnome.desktop.wm.keybindings begin-resize "[]"
gsettings set org.gnome.desktop.wm.keybindings cycle-group "[]"
gsettings set org.gnome.desktop.wm.keybindings cycle-group-backward "[]"
gsettings set org.gnome.desktop.wm.keybindings cycle-panels "[]"
gsettings set org.gnome.desktop.wm.keybindings cycle-panels-backward "[]"
gsettings set org.gnome.desktop.wm.keybindings cycle-windows "['<Super>Tab']"
gsettings set org.gnome.desktop.wm.keybindings cycle-windows-backward "['<Control><Super>Tab']"

gsettings set org.gnome.desktop.wm.keybindings maximize "['<Control><Super>Up']"
gsettings set org.gnome.desktop.wm.keybindings minimize "[]"
gsettings set org.gnome.desktop.wm.keybindings unmaximize "[]"

gsettings set org.gnome.desktop.wm.keybindings move-to-monitor-down "[]"
gsettings set org.gnome.desktop.wm.keybindings move-to-monitor-left "['<Control><Shift><Super>t']"
gsettings set org.gnome.desktop.wm.keybindings move-to-monitor-right "['<Control><Shift><Super>r']"
gsettings set org.gnome.desktop.wm.keybindings move-to-monitor-up "[]"

gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-1 "[]"

gsettings set org.gnome.desktop.wm.keybindings switch-applications-backward "[]"
gsettings set org.gnome.desktop.wm.keybindings switch-applications "[]"
gsettings set org.gnome.desktop.wm.keybindings switch-group-backward "[]"
gsettings set org.gnome.desktop.wm.keybindings switch-group "[]"
gsettings set org.gnome.desktop.wm.keybindings switch-input-source-backward "[]"
gsettings set org.gnome.desktop.wm.keybindings switch-input-source "['<Super>space']"
gsettings set org.gnome.desktop.wm.keybindings switch-panels-backward "[]"
gsettings set org.gnome.desktop.wm.keybindings switch-panels "[]"

gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1 "[]"

gsettings set org.gnome.desktop.wm.keybindings activate-window-menu "[]"
gsettings set org.gnome.settings-daemon.plugins.media-keys home '<Control><Super>l'
gsettings set org.gnome.settings-daemon.plugins.media-keys terminal '<Primary><Alt>c'
gsettings set org.gnome.settings-daemon.plugins.media-keys www '<Control><Super>w'

gsettings set org.gnome.settings-daemon.plugins.media-keys terminal ''
gsettings set org.gnome.settings-daemon.plugins.media-keys custom-keybindings "['/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/', '/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/', '/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/', '/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/', '/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/']"
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ name 'emacs'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ command 'emacsclient -c'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ binding '<Primary><Super>e'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/ name 'steam'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/ command 'steam'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/ binding '<Primary><Super>v'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/ name 'emacs classique'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/ command '/usr/local/bin/emacs --debug-init'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/ binding '<Primary><Shift><Super>l'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/ name 'switch audio output'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/ command '/home/toad/scripts/switchAudioOutput.sh'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/ binding '<Super>eacute'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/ name 'gnome-terminal maximize'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/ command 'gnome-terminal --window --maximize'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/ binding '<Ctrl><Alt>c'

# Make caps lock an other ctrl
gsettings set org.gnome.desktop.input-sources xkb-options "['caps:ctrl_modifier']"

# Sane defaults
gsettings set org.gtk.Settings.ColorChooser custom-colors "[(0.0, 0.0, 1.0, 1.0), (1.0, 1.0, 0.0, 1.0), (0.1803921568627451, 0.54509803921568623, 0.3411764705882353, 1.0), (0.52941176470588236, 0.80784313725490198, 0.92156862745098034, 1.0), (0.51372549019607838, 0.43529411764705883, 1.0, 1.0)]"
gsettings set org.gtk.Settings.ColorChooser selected-color "(true, 0.54117647058823526, 0.88627450980392153, 0.20392156862745098, 1.0)"
gsettings set org.gnome.desktop.background show-desktop-icons false

gsettings set org.gnome.desktop.wm.preferences audible-bell false
gsettings set org.gnome.settings-daemon.peripherals.keyboard numlock-state "'on'"
gsettings set org.gnome.settings-daemon.peripherals.keyboard remember-numlock-state true

gsettings set org.gnome.shell favorite-apps "[]"
gsettings set org.gnome.shell.keybindings toggle-application-view "[]"
gsettings set org.gnome.shell.keybindings toggle-message-tray "[]"
gsettings set org.gnome.shell.keybindings toggle-overview "[]"

gsettings set org.gnome.ControlCenter last-panel 'display'
gsettings set org.gnome.settings-daemon.plugins.color night-light-enabled true
gsettings set org.gnome.settings-daemon.plugins.color night-light-schedule-automatic false

gsettings set org.gnome.ControlCenter last-panel 'ubuntu'
gsettings set org.gnome.shell.extensions.dash-to-dock dock-fixed false

# Nautilus
gsettings set org.gnome.nautilus.list-view default-column-order "['name', 'size', 'type', 'date_modified', 'date_accessed', 'owner', 'group', 'permissions', 'mime_type', 'where']"
gsettings set org.gtk.Settings.FileChooser show-hidden true

# Background
sudo wget 'https://drive.google.com/uc?export=download&id=1OgHHoQwvhaDtEiDGu2qNbJsSLIf5Vkd8' -O "/usr/share/backgrounds/my_wallpaper.jpg"
gsettings set org.gnome.desktop.background picture-uri 'file:///usr/share/backgrounds/my_wallpaper.jpg'
gsettings set org.gnome.desktop.background primary-color '#000000'
gsettings set org.gnome.desktop.background secondary-color '#000000'

# Terminal
gsettings set org.gnome.Terminal.ProfilesList default '854571b6-9289-481c-a191-b1a1e12c8952'
gsettings set org.gnome.Terminal.ProfilesList list "['b1dcc9dd-5262-4d8d-a863-c897e6d979b9', '854571b6-9289-481c-a191-b1a1e12c8952']"
gsettings set org.gtk.Settings.ColorChooser selected-color "(true, 0.0, 0.0, 0.0, 1.0)"
dconf write "/org/gnome/terminal/legacy/profiles:/:854571b6-9289-481c-a191-b1a1e12c8952/foreground-color" "'rgb(255,255,255)'"
dconf write "/org/gnome/terminal/legacy/profiles:/:854571b6-9289-481c-a191-b1a1e12c8952/visible-name" "'toad'"
dconf write "/org/gnome/terminal/legacy/profiles:/:854571b6-9289-481c-a191-b1a1e12c8952/use-system-font" "false"
dconf write "/org/gnome/terminal/legacy/profiles:/:854571b6-9289-481c-a191-b1a1e12c8952/use-transparent-background" "true"
dconf write "/org/gnome/terminal/legacy/profiles:/:854571b6-9289-481c-a191-b1a1e12c8952/use-theme-colors" "false"
dconf write "/org/gnome/terminal/legacy/profiles:/:854571b6-9289-481c-a191-b1a1e12c8952/font" "'Source Code Pro 12'"
dconf write "/org/gnome/terminal/legacy/profiles:/:854571b6-9289-481c-a191-b1a1e12c8952/use-theme-transparency" "false"
dconf write "/org/gnome/terminal/legacy/profiles:/:854571b6-9289-481c-a191-b1a1e12c8952/bold-color-same-as-fg" "true"
dconf write "/org/gnome/terminal/legacy/profiles:/:854571b6-9289-481c-a191-b1a1e12c8952/background-color" "'rgb(0,0,0)'"
dconf write "/org/gnome/terminal/legacy/profiles:/:854571b6-9289-481c-a191-b1a1e12c8952/background-transparency-percent" "27"
dconf write "/org/gnome/terminal/legacy/profiles:/:854571b6-9289-481c-a191-b1a1e12c8952/audible-bell" "false"

# Default brightness
echo '#!/bin/sh -e
#
# rc.local
#
# This script is executed at the end of each multiuser runlevel.
# Make sure that the script will "exit 0" on success or any other
# value on error.
#
# In order to enable or disable this script just change the execution
# bits.
#
# By default this script does nothing.
echo 325 | sudo tee /sys/class/backlight/intel_backlight/brightness

exit 0' | sudo tee /etc/rc.local

# Terminal keybindings
echo "
function bindCommands {
    # Fix issue with emacs shell
    if [[ -z \"$GNOME_TERMINAL_SCREEN\" ]]
    then
        return
    fi

    echo \"Loading bindings\"
    stty lnext undef
    bind '\"\C-V\": beginning-of-line'
    bind '\"\C-L\": end-of-line'
    stty eof undef
    bind '\"\C-D\": previous-history'
    stty stop undef
    bind '\"\C-S\": next-history'
    stty rprnt undef
    bind '\"\C-R\": forward-char'
    bind '\"\er\": forward-word'
    bind '\"\C-T\": backward-char'
    bind '\"\et\": backward-word'
    bind '\"\C-E\": delete-char'
    stty eof \"^e\"
    bind '\"\ee\": kill-word'
    bind '\"\C-?\": backward-kill-word'
    # Not Working
    bind '\"\C-f\": transpose-char'
    # Not Working
    bind '\"\ef\": transpose-word'
    bind '\"\C-n\": forward-search-history'
    # bind '\"\C-\'\": backward-delete-char'
}

# Use bindCommands if the script is sourced
[[ $_ != $0 ]] && bind '\"\C-V\": beginning-of-line' 2>&1 | grep -q 'warning' || bindCommands

# go packages
export PATH=$PATH:~/go/bin

export ALTERNATE_EDITOR=""
export EDITOR=emacsclient

alias ll='ls -alh'

# export PATH=/usr/local/cuda-10.1/bin:/usr/local/cuda-10.1/NsightCompute-2019.1${PATH:+:${PATH}}
# export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/cuda-10.1/lib64${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}

" >> "${HOME}/.bashrc"

# Git config
echo "[user]
    email = gros.nicolas0@gmail.com
    name = Nicolas Gros
[core]
    editor = emacs
[color]
    ui = true
    diff = auto
    status = auto
    branch = auto
[push]
    default = simple" > "${HOME}"/.gitconfig

sudo apt-get install -y texlive-full gimp vlc

# Install docker-nvidia
sudo apt install -y cuda-drivers
distribution=$(. /etc/os-release;echo $ID$VERSION_ID)
curl -s -L https://nvidia.github.io/nvidia-docker/gpgkey | sudo apt-key add -
curl -s -L https://nvidia.github.io/nvidia-docker/$distribution/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list
sudo apt-get update && sudo apt-get install -y nvidia-container-toolkit
sudo systemctl restart docker

# Install cpu/memory monitoring
sudo apt-get install -y gir1.2-gtop-2.0 gir1.2-networkmanager-1.0  gir1.2-clutter-1.0
# Manual
# Ubuntu software
# Search: "system-monitor"
# Install

# sudo apt-get install -y gnome-tweak-tool
