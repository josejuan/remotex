# remotex

I use it to control my HTPC from my touch device.

I made this version because Java consumes a lot of memory (see https://github.com/josejuan/remotex11)

## Currently

1. hardware mouse and keyboard emulation.
1. only X11 server (use Java version for window users).
1. any touch device (through web browser).
1. easy keyboard layout keys recognition.
1. voice commands

## Voice Commands

### Config

Into your home `~/remotex/voicecommands` place executable **batch files** with like

```
#!/bin/bash
#remotex.title Lista Ficheros
#remotex.description Hace un listado de ficheros
#remotex.outputmode html
...

```

where `outputmode` could `html`, `text` or `none` and the exit code is used to get `success` state.

If no output, simply an `ok` reaction will be shown to user.
