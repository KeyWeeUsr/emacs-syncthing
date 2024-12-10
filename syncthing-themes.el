;;; syncthing-themes.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(defconst syncthing-theme--default-ascii
  '(:download "(v)"
    :upload "(^)"
    :files "(f)"
    :folder "(F)"
    :drive "(S)"
    :network "(N)"
    :sign "(d)"
    :watch "(w)"
    :qr "(I)"
    :tag "(v)"
    :laptop "(D)"
    :envelope "(L)"
    :hourglass "(t)"
    :info "(i)"
    :folder-open "(o)"
    :world "(g)"
    :house "(h)"
    :sync "(s)"
    :eye "(e)"
    :lift "(p)"
    :share "(s)"
    :swap "(T)"
    :cloud "(C)"
    :link "(l)"
    :signal "(5)"
    :switch "(m)"
    :press "(P)"))

(defconst syncthing-theme-default
  `(:icons (:download ""
            :upload ""
            :files ""
            :folder ""
            :drive ""
            :network ""
            :sign ""
            :watch ""
            :qr ""
            :tag ""
            :laptop ""
            :envelope "✉"
            :hourglass "⌛"
            :info ""
            :folder-open ""
            :world ""
            :house ""
            :sync ""
            :eye ""
            :lift ""
            :share ""
            :swap "⇄"
            :cloud ""
            :link ""
            :signal ""
            :switch ""
            :press "")
    :text ,syncthing-theme--default-ascii)
  "Default theme, working with DejaVu Sans Mono.")

(defconst syncthing-theme-emoji-one
  `(:icons (:download "⬇️"
            :upload "⬆️"
            :files "📄"
            :folder "📂"
            :drive "💾"
            :network "📡"
            :sign "🚦"
            :watch "⌚"
            :qr "🧩"
            :tag "🏷️"
            :laptop "💻"
            :envelope "✉️"
            :hourglass "⏳"
            :info "ℹ️"
            :folder-open "🗂️"
            :world "🌍"
            :house "🏠"
            :sync "🔄"
            :eye "👁️"
            :lift "🚀"
            :share "🔗"
            :swap "↔️"
            :cloud "☁️"
            :link "🔗"
            :signal "📶"
            :switch "↕️"
            :press "👇")
    :text ,syncthing-theme--default-ascii))

(defconst syncthing-theme-emoji-two
  `(:icons (:download "📥"
            :upload "📤"
            :files "📑"
            :folder "🗂️"
            :drive "🖴"
            :network "🌐"
            :sign "❌"
            :watch "🕰️"
            :qr "🔲"
            :tag "🏷"
            :laptop "🖥️"
            :envelope "📧"
            :hourglass "⏰"
            :info "📖"
            :folder-open "📂"
            :world "🌎"
            :house "🏡"
            :sync "🔃"
            :eye "👀"
            :lift "🏗️"
            :share "📢"
            :swap "🔁"
            :cloud "🌩️"
            :link "🌍"
            :signal "📡"
            :switch "🔛"
            :press "🖲️")
    :text ,syncthing-theme--default-ascii))

(defcustom syncthing-theme
  'syncthing-theme-default
  "Current theme."
  :group 'syncthing-themes
  :type '(choice (const :tag "Default" syncthing-theme-default)
                 (const :tag "Emoji One" syncthing-theme-emoji-one)
                 (const :tag "Emoji Two" syncthing-theme-emoji-two)
                 (symbol :tag "Custom (symbol)")))

(provide 'syncthing-themes)
;;; syncthing-themes.el ends here
