#!/bin/bash
set -ex

# on raspbian, build the program and reboot to it

./build.sh

sudo cp BCM43430A1.hcd /boot
sudo cp bluetooth-dev-bluetoothtest-kernel-RPI3.img /boot
sudo cp *-config.txt *-cmdline.txt /boot
sudo cp /boot/bluetooth-dev-bluetoothtest-config.txt /boot/config.txt
sleep 2
sudo reboot
