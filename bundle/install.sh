set -e

clear

echo " ____                 ____            _       _   "
echo "|  _ \ _   _ _ __ ___/ ___|  ___ _ __(_)_ __ | |_ "
echo "| |_) | | | | '__/ _ \___ \ / __| '__| | '_ \| __|"
echo "|  __/| |_| | | |  __/___) | (__| |  | | |_) | |_ "
echo "|_|    \__,_|_|  \___|____/ \___|_|  |_| .__/ \__|"
echo "                                       |_|        "
echo 
echo "This script will "
echo 
echo "  - Copy binary files to /usr/local/purescript/bin/"
echo "  - Copy source code to ~/.purescript/prelude/"
echo
echo "PureScript is distributed under the MIT license:"
echo
echo "Permission is hereby granted, free of charge, to any person obtaining a copy of"
echo "this software and associated documentation files (the 'Software'), to deal in"
echo "the Software without restriction, including without limitation the rights to"
echo "use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of"
echo "the Software, and to permit persons to whom the Software is furnished to do so,"
echo "subject to the following conditions:"
echo
echo "The above copyright notice and this permission notice shall be included in all"
echo "copies or substantial portions of the Software."
echo 
echo "THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR"
echo "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS"
echo "FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR"
echo "COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER"
echo "IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN"
echo "CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."
echo
echo "Press any key to continue or ^C to exit."
echo

read

echo "Copying binary files to /usr/local/purescript/bin/ ..."

mkdir -p /usr/local/purescript/bin/
cp psc psci psc-make docgen /usr/local/purescript/bin/

echo "Copying source code to ~/.purescript/prelude/ ..."

mkdir -p ~/.purescript/prelude
cp prelude.purs ~/.purescript/prelude/

echo "You can add the PureScript binaries to your PATH variable as follows:"
echo
echo "  echo \"export PATH=/usr/local/purescript/bin/:\\\$PATH\" >> ~/.bash_profile"
echo "  source ~/.bash_profile"

