[English Version](./README.en-US.md)

# 关于项目

本项目非 GNU Emacs 官方配置，亦非 GNU Emacs 的插件，仅作为个人配置项目使用。配置由 ©Cabins 进行维护。

# 编程语言配置

笔者日常使用 Python 和 Golang 作为主力语言，所以这两种语言的配置是相对比较完善的（使用的是 LSP 来完成配置）。如果你使用的是其他的编程语言，你也可以通过安装 Language Server 的方式来自动完成配置，而不需要额外编写默认配置代码（个性化配置的话还需要特殊写代码）。

# 字体配置

这一块非常的个性化，而且我在不同的机器上测试发现，即便同样是 Windows，在不同的机器上配置也不通用。所以你在使用我的这个配置的时候，建议自行调整显示效果。配置文件位于`list/init-ui.el`中。

另外，由于不确定使用者的机器上安装了哪些字体，默认会在一个列表中进行查找。先找到哪个就用哪个。你可以把你喜欢的字体，放在列表的最开始。

字号，一定要调。不同的分辨率的机器上，完全不通用。

# 配置安装

通过命令行进行安装，常见的命令终端有：

- macOS 平台：Terminal 或者 iTerm2
- Windows 平台：CMD, Powershell, Windows Terminal
- GNU/Linux 平台：Gnome Terminal 或者 Konsole

把以下代码粘贴到终端中运行即可：

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

- macOS, 11.1, GUI 模式
- Fedora Linux 31/32/33/34, Workstation & Server Edition
- Windows 10, 1909(18363) (Native GUI & msys2)
- Ubuntu 20.04.1 on WSL2

理论上说，应该也通行于其他的平台，如果有问题，可以随时提 Issue。
