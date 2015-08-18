@echo off
call npm install
call node_modules\.bin\bower install --config.interactive=false
