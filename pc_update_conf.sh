#!/bin/bash

set -eu

mkdir -p "${HOME}"/log
sudo apt-get update
sudo apt-get dist-upgrade -y
sudo apt-get install -y git python3 python3-pip python-dev build-essential \
     curl ffmpeg imagemagick astyle

# Install emacs
git clone https://github.com/AlorsCoings/emacs-configuration "${HOME}/.emacs.d"
bash "${HOME}/.emacs.d/install_emacs.sh"

# Install docker
curl -fsSL get.docker.com -o get-docker.sh
sh get-docker.sh
rm get-docker.sh
sudo -H usermod -aG docker ${USER}

# Handle update of docker
echo "deb [arch=armhf] https://download.docker.com/linux/$(. /etc/os-release; echo "$ID") $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list

# Install docker-compose
sudo curl -L https://github.com/docker/compose/releases/download/1.20.1/docker-compose-$(uname -s)-$(uname -m) -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# Upgrade pip
pip3 install --upgrade pip

# Install google chrome
sudo sh -c 'echo "deb [arch=amd64] https://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list'
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
sudo apt-get update
sudo apt-get install google-chrome-stable

# Install verson 8 of nodejs
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
sudo apt-get install -y nodejs

# Install python packages
sudo -H pip3 install tensorflow pandas pylint youtube-dl \
     yapf autopep8 jedi flake8 rope_py3k

# Install nvidia drivers
sudo ubuntu-drivers autoinstall

# Google cpplint c++ checker
sudo wget 'https://raw.githubusercontent.com/google/styleguide/gh-pages/cpplint/cpplint.py' -O /usr/local/bin/cpplint.py

sudo chmod a+x /usr/local/bin/cpplint.py

# sudo apt-get install -y indicator-multiload
# indicator-multiload &

# gsettings set de.mh21.indicator-multiload.general autostart true
# gsettings set de.mh21.indicator-multiload.general background-color 'xosview:background'
# gsettings set de.mh21.indicator-multiload.general color-scheme 'xosview'
# gsettings set de.mh21.indicator-multiload.general indicator-expressions '['"'"''"'"', '"'"'CPU $(percent(cpu.inuse))'"'"', '"'"'Mem $(size(mem.user))'"'"', '"'"'Swap $(size(swap.used))'"'"', '"'"'Net $(speed(net.down))/$(speed(net.up))'"'"', '"'"'Load $(decimals(load.avg,2))'"'"', '"'"'Disk $(speed(disk.read))/$(speed(disk.write))'"'"']'
# gsettings set de.mh21.indicator-multiload.general menu-expressions '['"'"'CPU: $(percent(cpu.inuse)), iowait $(percent(cpu.io))'"'"', '"'"'Mem: $(size(mem.user)), cache $(size(mem.cached))'"'"', '"'"'Swap: $(size(swap.used))'"'"', '"'"'Net: down $(speed(net.down)), up $(speed(net.up))'"'"', '"'"'Load: $(decimals(load.avg,2))'"'"', '"'"'Disk: read $(speed(disk.read)), write $(speed(disk.write))'"'"']'
# gsettings set de.mh21.indicator-multiload.general width 100
# gsettings set de.mh21.indicator-multiload.graphs.disk enabled true
# gsettings set de.mh21.indicator-multiload.graphs.load enabled true
# gsettings set de.mh21.indicator-multiload.graphs.mem enabled true
# gsettings set de.mh21.indicator-multiload.graphs.net enabled true
# gsettings set de.mh21.indicator-multiload.graphs.swap enabled true
# gsettings set de.mh21.indicator-multiload.traces.cpu1 color 'xosview:cpu1'
# gsettings set de.mh21.indicator-multiload.traces.cpu2 color 'xosview:cpu2'
# gsettings set de.mh21.indicator-multiload.traces.cpu3 color 'xosview:cpu3'
# gsettings set de.mh21.indicator-multiload.traces.cpu4 color 'xosview:cpu4'
# gsettings set de.mh21.indicator-multiload.traces.disk1 color 'rgb(138,226,52)'
# gsettings set de.mh21.indicator-multiload.traces.disk2 color 'rgb(193,125,17)'
# gsettings set de.mh21.indicator-multiload.traces.load1 color 'xosview:load1'
# gsettings set de.mh21.indicator-multiload.traces.mem1 color 'xosview:mem1'
# gsettings set de.mh21.indicator-multiload.traces.mem2 color 'xosview:mem2'
# gsettings set de.mh21.indicator-multiload.traces.mem3 color 'xosview:mem3'
# gsettings set de.mh21.indicator-multiload.traces.mem4 color 'xosview:mem4'
# gsettings set de.mh21.indicator-multiload.traces.net1 color 'rgb(255,255,0)'
# gsettings set de.mh21.indicator-multiload.traces.net2 color 'rgb(0,0,255)'
# gsettings set de.mh21.indicator-multiload.traces.net3 color 'rgb(0,0,0)'
# gsettings set de.mh21.indicator-multiload.traces.swap1 color 'xosview:swap1'

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

export RASPBERRY=pi@192.168.1.89
export PI3=pi@192.168.1.4

export ALTERNATE_EDITOR=""
export EDITOR=emacsclient

alias ll='ls -alh'

# [ -f ${HOME}/.Xmodmap ] && xmodmap ${HOME}/.Xmodmap 2> /dev/null
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
