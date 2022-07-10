#!/bin/bash

coursier launch "com.github.rcmartins:blinky-cli_2.13:0.4.0+4-40ff3115-SNAPSHOT" -r sonatype:snapshots -- $1
