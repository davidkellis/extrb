#!/bin/sh

rake clobber_package
rake gem
sudo gem install pkg/extrb-0.0.1.gem
