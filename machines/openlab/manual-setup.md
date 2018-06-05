# Manual setup for labtops

## Poor man’s setup

### Intro

- download newest nixos setup
- write to USB stick
- boot live system

### Install vanilla nixos

- use parted to setup msdos table (mktable) & full-disk partition (mkpart 0% 100%)
- mkfs.ext4 -L /dev/sda1
- mount /dev/disk/by-label/labtop /mnt
- nixos-generate-config --root mnt, comment out a few things in /mnt/etc/nixos/configuration.nix
- nixos-install
- set temporary root password (123456), will be overwritten by vuizvui
- reboot

### Install vuizvui

- boot, login as root
- follow “installing a machine” documentation from vuizvui wiki
