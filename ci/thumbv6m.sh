#!/bin/bash

# This is from https://github.com/KodrAus/rust-no-std
# Its purpose is to cause Github to build the crate in a no-std environment.

set -o errexit -o nounset

rustup target add thumbv6m-none-eabi

cargo build --target=thumbv6m-none-eabi
