#!/usr/bin/env bash

# bash
sudo npm install -g bash-language-server

# CSS/LessCSS/SASS/SCSS
sudo npm install -g vscode-css-languageserver-bin

# Golang
export GO111MODULE=on
go get -v golang.org/x/tools/gopls@latest

# HTML
sudo npm install -g vscode-html-languageserver-bin

# JavaScript/TypeScript
sudo npm i -g typescript-language-server
sudo npm i -g typescript

# JSON
sudo npm i -g vscode-json-languageserver

# Python
sudo pip3 install python-language-server[all] pyls-mypy pyls-isort pyls-black -i https://mirrors.aliyun.com/pypi/simple/

# Vue.js
sudo npm install -g vue-language-server
