#!/bin/sh

## TO ADD:
## - Clipboard passthrough
## - test ssh outgoing, pwn box?
## - snapshot every 30mins
##   - 2 options (in chgpt)
##     - send savevm to socket
##     - send savevm to stdin in /proc/<processname>/fd/0
##     (maybe only thing i need?, along with -snapshots?)
##   - enable -daemonize for good measure?




## Usage: run-hive24-vm.sh [qcow2Image]
#
## NECESSARY SETUP:
#   - A .ova is basically just a tar file containing the disk image and some
#   configs, so the inflated disk image can be converted from a .vmdk to a .qcow2
#   and then ran in qemu. An XML file containing virtual hardware configs is also
#   inflated, which can be translated by hand to qemu run args (done by me).
#
## COMMANDS TO RUN FIRST:
# $ tar -xvf yourMintImage.ova # will inflate ovf, mf, and vmdk files (only need vmdk)
# $ qemu-img convert -f vmdk -O qcow2 yourMintImage.vmdk yourMintImage.qcow2
# $ ./run-hive24-vm.sh [qcow2Image]

# modify this as appropriate
defaultFile="$HOME/code/ctf/hivestorm24/competition/Hive24_mint_official.qcow2"

# default to $defaultFile if arg 1 is unspecified
pathToFile="${1:-$defaultFile}"
[ -f "$pathToFile" ] || { echo "Error, file $pathToFile not found"; exit 1; }

socket_path="/tmp/qemu-monitor.sock"

# args
args=(
    -smp cpus=2,sockets=1,cores=2,threads=1  # dual core
    -cpu host  # improve performance by using host's cpu features
    -m 3072  # ram
    -device usb-ehci,id=ehci  # EHCI USB controller
    -device usb-tablet  # improve cursor accuracy with USB tablet device
    -device virtio-scsi-pci,id=scsi0  # Virtio SCSI controller for I/O
    -drive file=${pathToFile},if=none,id=drive-0  # drive
    -device scsi-hd,id=drive-0,drive=drive-0,bus=scsi0.0  # SCSI hard disk device
    -netdev user,id=net0  # user-mode network
    -device virtio-net,netdev=net0  # more standard network adapter
    -device virtio-vga  # Virtio VGA graphics driver
    -enable-kvm  # KVM acceleration
    -machine q35,mem-merge=on  # Q35 chipset and memory merging
    -audiodev pa,id=pulse,out.buffer-length=50000  # pulseaudio host (tweakable buffer-length)
    -device intel-hda -device hda-output,audiodev=pulse  # use intel HDA & pulse
    -daemonize  # prevent losing progress if close gui
    # Socket access to save and load snapshots:
    # $ socat - UNIX-CONNECT:"/tmp/qemu-monitor.sock"
    # $ info snapshots
    # $ savevm <snap>
    # $ loadvm <snap>
    # $ <Ctrl-d> # exit
    -monitor unix:${socket_path},server,nowait # socket for snapshot automation scripting
)

# run
qemu-system-x86_64 "${args[@]}" &

# qemu-system-x86_64 \
#     -smp cpus=2,sockets=1,cores=2,threads=1 \
#     -cpu host \
#     -m 3072 \
#     -device usb-ehci,id=ehci \
#     -device usb-tablet \
#     -device virtio-scsi-pci,id=scsi0 \
#     -drive file=${pathToFile},if=none,id=drive-0 \
#     -device scsi-hd,id=drive-0,drive=drive-0,bus=scsi0.0 \
#     -netdev user,id=net0 \
#     -device vmxnet3,netdev=net0 \
#     -device ac97 \
#     -device virtio-vga \
#     -enable-kvm \
#     -machine q35,mem-merge=on



