#!/bin/bash

coursier launch "com.github.rcmartins:blinky-cli_2.12:0.3.1+39-19f3193e-SNAPSHOT" -r sonatype:snapshots -- "$1"
