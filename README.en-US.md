[简体中文版本](./README.md)

# About this project


This is `NOT` a part of [GNU Emacs](https://www.gnu.org/software/emacs/) but a personal configuration project to make it more convenient to me.

This project belongs to © Cabins from China.

## About Programming language

I mainly use [Python](https://www.python.org/) & [Go](https://golang.org/) in my work.  So I set up the programming environments for this two languages with [LSP](https://emacs-lsp.github.io/lsp-mode/) - the protocol from Microsoft.  If you use other languages, such as JavaScript, C++, and so on, you can easily set up for them with LSP - sometimes you just install the language servers, Emacs config code does NOT need to change.

## About the default FONT
I use Courier & SimKai, 'cause they are the only fonts not make my Windows version slow, change the `lisp/init-ui.el` to your preferences, if you do NOT use CJK fonts.

## Installation

Launch a terminal, such as:

- `Terminal` App or `iTerm2` on macOS
- `GNOME Terminal` or `Konsole` on GNU/Linux
- `CMD` or `PowerShell` or `Windows Terminal` on Windows 10

Run the code below, and then launch your Emacs, enjoy it.

```bash
git clone https://github.com/cabins/.emacs.d ~/.emacs.d
```

If you has already upgrade your emacs to 27.1, you also can clone this repo like this:

```bash
git clone https://github.com/cabins/.emacs.d ~/.config/emacs
```

> Note: If you use Windows 10,  you should set an environment variable named `HOME`,  and set its value to your user directory,  like `C:/Users/<your_name>`. Otherwise emacs will use `%Appdata%` as home directory by default.

## Testing

This project is tested on

- macOS,  11.1,  GUI mode
- Fedora Linux 31/32/33, both Workstation & Server Edition
- Windows 10,  1909(18363) (Native GUI & msys2)
- Ubuntu 20.04.1 on WSL2

it should run on other platforms, please note that.

## About bugs

Any issue will be welcomed, just send me an issue if you find something not friendly to you.
