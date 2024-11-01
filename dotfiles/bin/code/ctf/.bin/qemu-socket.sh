#!/bin/sh

socat - UNIX-CONNECT:"/tmp/qemu-monitor.sock"
