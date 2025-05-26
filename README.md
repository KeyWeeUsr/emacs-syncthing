# emacs-syncthing
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![CI][ci-badge]][ci-workflow]
[![Coverage Status][cover-badge]][cover-link]
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

Emacs client for [Syncthing](https://syncthing.net). Currently it tries to port
all viewing functionality, later on even with editing/writing.

## How to

Install it from [Melpa](https://melpa.org/#/getting-started) or clone and
install manually, then simply <kbd>M</kbd>-<kbd>x</kbd> <kbd>syncthing</kbd>.

Note that if you have `debug-on-error` enabled on your first run, the customize
buffer for setting up the API token might be somewhere in the background. Use
<kbd>M</kbd>-<kbd>x</kbd> <kbd>customize-group</kbd> <kbd>syncthing</kbd> for
setting it manually, then re-run.

## Key bindings

Ensure you are in the Syncthing buffer:
<kbd>C</kbd>-<kbd>h</kbd><kbd>m</kbd> or <kbd>?</kbd> for all bindings.

## Customization

Check the manual for Emacs Customization: <kbd>C</kbd>-<kbd>h</kbd><kbd>i</kbd>
<kbd>m</kbd><kbd>Emacs</kbd><kbd>m</kbd><kbd>Customization</kbd> and group
<kbd>M</kbd>-<kbd>x</kbd> <kbd>customize-group</kbd> <kbd>syncthing</kbd>.

## Example

You can run a demo without having Syncthing installed on your system. You'll
need [Eldev](https://github.com/emacs-eldev/eldev) and afterwards you can issue
`eldev demo` command.

## Screenshots

![Screenshot][demo]![Screenshot][demo-term]

[melpa-badge]: https://melpa.org/packages/syncthing-badge.svg
[melpa-package]: https://melpa.org/#/syncthing
[melpa-stable-badge]: https://stable.melpa.org/packages/syncthing-badge.svg
[melpa-stable-package]: https://stable.melpa.org/#/syncthing
[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[demo]: https://raw.githubusercontent.com/KeyWeeUsr/emacs-syncthing/master/screenshot.png
[demo-term]: https://raw.githubusercontent.com/KeyWeeUsr/emacs-syncthing/master/screenshot-term.png
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr
[ci-badge]: https://github.com/KeyWeeUsr/emacs-syncthing/actions/workflows/test.yml/badge.svg
[ci-workflow]: https://github.com/KeyWeeUsr/emacs-syncthing/actions/workflows/test.yml
[cover-badge]: https://coveralls.io/repos/github/KeyWeeUsr/emacs-syncthing/badge.svg?branch=master
[cover-link]: https://coveralls.io/github/KeyWeeUsr/emacs-syncthing?branch=master
