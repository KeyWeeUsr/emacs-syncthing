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

(condition-case err
    (require 'all-the-icons)
  (error (defun all-the-icons-faicon (&rest _) "N/A") t))
(defconst syncthing-theme-external-ati
  `(:icons (:download ,(all-the-icons-faicon "cloud-download")
            :upload ,(all-the-icons-faicon "cloud-upload")
            :files ,(all-the-icons-faicon "files-o")
            :folder ,(all-the-icons-faicon "folder-o")
            :drive ,(all-the-icons-faicon "hdd-o")
            :network ,(all-the-icons-faicon "sitemap")
            :sign ,(all-the-icons-faicon "map-signs")
            :watch ,(all-the-icons-faicon "clock-o")
            :qr ,(all-the-icons-faicon "qrcode")
            :tag ,(all-the-icons-faicon "tag")
            :laptop ,(all-the-icons-faicon "laptop")
            :envelope ,(all-the-icons-faicon "envelope-o")
            :hourglass ,(all-the-icons-faicon "hourglass-o")
            :info ,(all-the-icons-faicon "info-circle")
            :folder-open ,(all-the-icons-faicon "folder-open-o")
            :world ,(all-the-icons-faicon "globe")
            :house ,(all-the-icons-faicon "home")
            :sync ,(all-the-icons-faicon "refresh")
            :eye ,(all-the-icons-faicon "eye")
            :lift ,(all-the-icons-faicon "laptop")
            :share ,(all-the-icons-faicon "share-alt")
            :swap ,(all-the-icons-faicon "exchange")
            :cloud ,(all-the-icons-faicon "cloud")
            :link ,(all-the-icons-faicon "link")
            :signal ,(all-the-icons-faicon "signal")
            :switch ,(all-the-icons-faicon "random")
            :press ,(all-the-icons-faicon "compress"))
    :text ,syncthing-theme--default-ascii))

(defcustom syncthing-theme
  'syncthing-theme-default
  "Current theme."
  :group 'syncthing-themes
  :type '(choice (const :tag "Default" syncthing-theme-default)
                 (const :tag "Emoji One" syncthing-theme-emoji-one)
                 (const :tag "Emoji Two" syncthing-theme-emoji-two)
                 (const :tag "All The Icons (external)"
                        syncthing-theme-external-ati)
                 (symbol :tag "Custom (symbol)")))

(provide 'syncthing-themes)
;;; syncthing-themes.el ends here
