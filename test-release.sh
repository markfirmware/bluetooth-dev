#!/bin/bash
set -x

mkdir -p testing
rm -rf testing/*
cp release/*.zip testing
pushd testing
unzip *.zip
rm *.zip
sudo cp bluetooth-dev-bluetoothtest-* config.txt /boot
df /boot
sleep 5
sudo reboot
