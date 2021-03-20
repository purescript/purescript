#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
IFS=$'\n\t'

readonly hlint_version=2.2.11
readonly build_dir="${BUILD_DIR:?Must provide a directory to build in}"
readonly bin_dir="${BIN_DIR:?Must provide a directory to install binaries}"

function cleanup() {
    local exit_code="${?}"

    exit "${exit_code}"
}

trap cleanup EXIT

function download_for_unix() {
    local os="${1}"
    local url="https://github.com/ndmitchell/hlint/releases/download/v${hlint_version}/hlint-${hlint_version}-x86_64-${os}.tar.gz"

    mkdir -p "${build_dir}"
    pushd "${build_dir}"
    curl --location "${url}" --output hlint.tar.gz
    tar -xzf hlint.tar.gz --strip-components=1
    popd

    mkdir -p "${bin_dir}/data"
    cp -r "${build_dir}/data" "${bin_dir}"
    cp "${build_dir}/hlint" "${bin_dir}"
}

function download_for_windows() {
    local url="https://github.com/ndmitchell/hlint/releases/download/v${hlint_version}/hlint-${hlint_version}-x86_64-windows.zip"

    mkdir -p "${build_dir}"
    pushd "${build_dir}"
    curl --location "${url}" --output hlint.zip
    7z e -r hlint.zip
    popd

    mkdir -p "${bin_dir}/data"
    cp -r "${build_dir}/data" "${bin_dir}"
    cp "${build_dir}/hlint.exe" "${bin_dir}"
}

function main() {
    # The OS environment variable is set to 'Windows_NT' on Windows NT systems.
    # This should work for all recent Windows versions including:
    # NT, 2000, XP, Server, Vista, 7, 8, 8.1, and 10.
    case "${OS:-$(uname)}" in
        'Darwin')
            download_for_unix 'osx';;
        'Linux')
            download_for_unix 'linux';;
        'Windows_NT')
            download_for_windows;;
        *)
            echo 'Unknown Platform. Only Linux, macOS, and Windows are supported';
            exit 1;;
    esac
}

main
