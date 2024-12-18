#!/bin/bash

find /tmp -maxdepth 1 -type f -atime 1 -user "$(whoami)" -delete
