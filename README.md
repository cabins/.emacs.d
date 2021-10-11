[English Version](./README.en-US.md)

# 关于项目

本项目非 GNU Emacs 官方配置，亦非 GNU Emacs 的插件，仅作为个人配置项目使用。配置由 ©Cabins 进行维护。

# 主要目标
保持足够的简洁、简单（对，就是字面意思）。尽可能的趋向于Vanilla Emacs。所以你在我的配置中看不到很多的第三方插件（除非真的是迫不得已），而更多的去使用内置的包——这样的后果就是，很多时候使用起来没有那么的方便。

这对于想学习Emacs的人（比如我）来说，是更有优势，更希望的。当前的配置中使用的第三方插件有：

- company，用来进行代码补全
- exec-path-from-shell，解决环境变量的问题
- -projectile-，仅Emacs 28以下的版本使用，28使用内置的project
- rainbow-delimiters，彩虹括号（其实这个包也可以删除掉）
- highlight-parentheses，括号高亮
- which-key，按键提示
- yasnippet，代码片段
- crux，一些快捷操作
- hungry-delete，优化的删除操作
- drag-stuff，代码的行、块移动
- format-all，代码格式化
- 编程语言相关
  - lsp-mode / lsp-ui
  - go-mode
  - rust-mode
  - vue-mode
  - web-mode / emmet-mode
  - json-mode
  - markdown-mode
  - protobuf-mode
  - restclient
  - yaml-mode

# 编程语言

使用的是 LSP 来完成配置。你只需要安装 Language Server 的方式来自动完成配置，而不需要额外编写默认配置代码（个性化配置的话还需要特殊写代码）。

# 字体配置

这一块非常的个性化，而且我在不同的机器上测试发现，即便同样是 Windows，在不同的机器上配置也不通用。所以你在使用我的这个配置的时候，建议自行调整显示效果。配置文件位于`list/init-ui.el`中。

另外，由于不确定使用者的机器上安装了哪些字体，默认会在一个列表中进行查找。先找到哪个就用哪个。你可以把你喜欢的字体，放在列表的最开始。

字号，一定要调。不同的分辨率的机器上，完全不通用。

与其费大力气去找一个通用的方案，不如直接使用这种方法来配置，没有必要浪费这个时间。

# 配置安装

通过命令行进行安装，把以下代码粘贴到终端中运行即可：

```bash
git clone https://github.com/cabins/.emacs.d ~/.emacs.d
```

如果你使用的是 27+版本，你也可以运行以下代码来安装：

```bash
git clone https://github.com/cabins/.emacs.d ~/.config/emacs
```

> 注意: 如果你使用的是 Windows 平台的话，你需要自行设置一个 HOME 环境变量，否则默认安装到`%AppData%`下。

## 代码测试

本项目已在以下平台测试通过：

- macOS, 11.1+, GUI 模式
- Fedora Linux 34, Workstation & Server Edition
- Windows 10, 1909(18363) (Native GUI & msys2)

理论上说，应该也通行于其他的平台，如果有问题，可以随时提 Issue。

## 问题排查

> Windows上如果出现闪屏

如果你在Windows 10上发现界面存在闪屏的情况（比如移动光标的时候，或者键入的时候），请检查是否开启了MacType。如果是的话，将Emacs的进程添加到MacType的排除列表中即可，例如在MacType的ini文件中添加如下的代码：
```ini
[UnloadDll]
emacs.exe
runemacs.exe
```

> 如果出现乱码

请使用all-the-icon进行字体的补全安装。另外如果是Windows的话，请额外安装Symbola。
