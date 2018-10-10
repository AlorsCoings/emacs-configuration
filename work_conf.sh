#!/bin/bash

set -eu

sudo apt-get update
sudo apt-get install -y openvpn libboost-dev libboost-program-options-dev doxygen graphviz

# Install dart sdk stable
sudo apt-get install -y apt-transport-https
sudo sh -c 'curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -'
sudo sh -c 'curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list'
sudo apt-get update
sudo apt-get install -y dart

echo "@reboot sudo openvpn --config /etc/openvpn/${USER}@encelade01.u-bourgogne.fr.ovpn
" | sudo crontab -u root -
