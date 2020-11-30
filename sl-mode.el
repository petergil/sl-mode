;;; sl-mode.el --- Real time SL (Stockholms Lokaltrafik) departures info.

;; Copyright (C) 2020 Peter Gille

;; Author: Peter Gille
;; Version: 0.9.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; User variables:
(defgroup sl nil
  "Major mode viewing real-time SL departures info"
  :prefix "sl-"
  :group 'applications)

;; Reading station info from separate file due to the size of the
;; variable.
(require 'sl-mode-stations)

(defcustom sl-station "T-Centralen"
  "Currently selected station"
  ;; TODO: Make selection groups for this
  :group 'sl)

(defcustom sl-api-endpoints
  '(("SL Hållplatser och Linjer 2" . "http://api.sl.se/api2/LineData.json")
    ("SL Realtidsinformation 4" . "https://api.sl.se/api2/realtimedeparturesV4.json"))
  "List of API - endpoint pairs for the various SL api:s.
See https://www.trafiklab.se/api"
  :type '(alist :key-type (string :tag "API name")
                :value-type (string :tag "API url endpoint"))
  :group 'sl)

(defcustom sl-api-keys
  '(("SL Realtidsinformation 4" . nil)
    ("SL Hållplatser och Linjer 2" . nil))
  "Api keys for the SL api:s, get at https://www.trafiklab.se/api/.
Minimum required is for 'SL Realtidsinformation 4'. To also
update station list you also need one for 'SL Hållplatser och
Linjer 2'."
  :type '(alist :key-type (string :tag "API name")
                :value-type (string :tag "API key"))
  :group 'sl)

(defcustom sl-timewindow "10"
  "Default time window for api searches"
  :group 'sl)

(defcustom sl-update-interval 300
  "Display update interval in seconds."
  :type 'integer
  :group 'sl)

(defcustom sl-manage-window nil
  "If non-nil actively manage the SL buffer in a separate fitted window."
  :type 'boolean
  :group 'sl)

(defcustom sl-interesting-stops '("Metros" "Buses" "Trains" "Trams" "Ships")
  "List of stop types to show"
  :group 'sl)

(defface sl-bold
  '((t :weight ultra-bold))
  "sl-mode bold face."
  :group 'sl-faces)

(defface sl-underline
  '((t :weight ultra-bold
       :underline t))
  "sl-mode bold face."
  :group 'sl-faces)

(defface sl-blue
  '((((class color) (background light)) (:foreground "#0099cc"))
    (((class color) (background dark)) (:foreground "#0099cc")))
  "Sl blue face."
  :group 'sl-faces)

(defface sl-green
  '((((class color) (background light)) (:foreground "#339933"))
    (((class color) (background dark)) (:foreground "#339933")))
  "Sl green face."
  :group 'sl-faces)

(defface sl-red
  '((((class color) (background light)) (:foreground "#ff0000"))
    (((class color) (background dark)) (:foreground "#ff0000")))
  "Sl red face."
  :group 'sl-faces)

(defface sl-yellow
  '((((class color) (background light)) (:foreground "#ffff33"))
    (((class color) (background dark)) (:foreground "#ffff33")))
  "Sl yellow face."
  :group 'sl-faces)

(defface sl-orange
  '((((class color) (background light)) (:foreground "#ff9933"))
    (((class color) (background dark)) (:foreground "#ff9933")))
  "Sl orange face."
  :group 'sl-faces)

(defface sl-header
  '((t :inherit (header-line-highlight)))
  "Face for sl header"
  :group 'sl-faces)

(defface sl-header-bold
  '((t :inherit (sl-header sl-bold)))
  "Face for sl bold header"
  :group 'sl-faces)

(require 'sl-mode-utils)

;; Internal variables:
(defvar sl--realtime-buffer nil
  "The sl info display buffer.")

(defvar sl--realtime-update-timer nil
  "Timer used for updating display buffer.")

(defvar sl--initial-window-height 10
  "Initial window height, the window gets re-sized after each update.")

(defvar sl--face-map #s(hash-table size 30 test equal data ("BLUE" sl-blue
                                                            "GREEN" sl-green
                                                            "RED" sl-red
                                                            "YELLOW" sl-yellow
                                                            "ORANGE" sl-orange
                                                            ))
  "Maps colors to sl-mode faces")

(defun sl-select-station ()
  "Interactivly select SL station."
  (interactive)
  (let ((station (completing-read "station: " (mapcar 'car sl-stations))))
    (when station
      (setq sl-station station)
      (when sl--realtime-buffer
        (sl-update)))))

(defun sl-quit ()
  "Exit sl-mode."
  (interactive)
  (sl--cleanup))

(defun sl-update ()
  "Update the SL real time data."
  (interactive)
  (sl--realtime-request))

(defun sl--make-color-square (color)
  (propertize (char-to-string ?\x25A0) 'font-lock-face (gethash color sl--face-map 'sl-red)))

(defun sl--bold (str)
  (propertize str 'font-lock-face 'sl-bold))

(defun sl--underline (str)
  (propertize str 'font-lock-face 'sl-underline))

(defun sl--header (str)
  (propertize str 'font-lock-face 'sl-header))

(defun sl--header-bold (str)
  (propertize str 'font-lock-face 'sl-header-bold))

(defun sl--realtime-update-buffer (station json)
  "Update the current buffer using the sl data JSON."
  (read-only-mode -1)
  (erase-buffer)
  (if (= 0 (gethash "StatusCode" json))
      (let* ((response (gethash "ResponseData" json))
             (time (gethash "LatestUpdate" response))
             (deviations (gethash "StopPointDeviations" response)))
        (insert (concat (sl--header-bold (format " %s" station))
                        (sl--header " Departures as of ")
                        (sl--header-bold  time)
                        "\n"))
        (dolist (transportation-type sl-interesting-stops)      
          (let* ((departures (gethash transportation-type response)))
            (when (> (length departures) 0)
              (insert (concat "\n" (sl--underline transportation-type) "\n"))
              (insert
               (mapconcat (lambda (departure)
                            ;; for each departure make a line with information
                            (concat
                             (sl--make-color-square "GREEN") ;; TODO: Make this depend on time until leaving
                             (format "%7s" (sl--bold (gethash "DisplayTime" departure)))
                             (format "%-30s"
                                     (concat (format "%3s"(gethash "LineNumber" departure)) " "
                                             (sl--bold (gethash "Destination" departure))))
                             (when (not (eq (gethash "StopPointDesignation" departure) :null))
                               (concat " Platform: " (gethash "StopPointDesignation" departure)))
                             (when (not (eq (gethash "GroupOfLine" departure) :null))
                               (concat " (" (gethash "GroupOfLine" departure) ") "))))
                          departures
                          "\n")))))
        (when (> (length deviations) 0)
          (insert (concat "\n" (sl--underline"Deviations") "\n"))
          (insert
           (mapconcat (lambda (deviation)
                        (concat
                         (gethash "StopAreaName" (gethash "StopInfo" deviation)) " "
                         (gethash "GroupOfLine" (gethash "StopInfo" deviation)) ": "
                         (gethash "Text" (gethash "Deviation" deviation)) 
                         (when (not (eq (gethash "Consequence" deviation) :null))
                           ;; TODO: find som example of when this is relevant
                           (concat " " (gethash "Consequence" deviation)))
                         ))
                      deviations
                      "\n"))))
    (insert (concat "Error fetching data from API:\n\n"
                    "Status: " (number-to-string (gethash "StatusCode" json)) "\n"
                    "Message: " (gethash "Message" json) "\n")))
  (insert "\n")
  (goto-char 1)
  (read-only-mode 1)
  (when sl-manage-window
    (fit-window-to-buffer (get-buffer-window (current-buffer)))))

(defvar sl-mode-map
  (let ((map (make-sparse-keymap 'sl-mode-map)))
    (define-key map (kbd "s") 'sl-select-station)
    (define-key map (kbd "q") 'sl-quit)
    (define-key map (kbd "g") 'sl-update)
    map)
  "The sl-mode keymap.")

(defun sl ()
  "Display realtime SL departure information."
  (interactive)
  (sl--cleanup)
  (setq sl--realtime-buffer (get-buffer-create "*SL Departures*"))
  (if sl-manage-window
      (let ((w (get-largest-window)))
        (setq w (split-window w
                              (- (window-height w)
                                 sl--initial-window-height 2)
                              nil))
        (set-window-buffer w sl--realtime-buffer)
        (select-window w)
        (sl-mode))
    (with-current-buffer sl--realtime-buffer
      (sl-mode))
    (display-buffer  sl--realtime-buffer nil)))

(defun sl--realtime-request-callback (station json)
  "The sl-mode timer callback.
json is the data received from `url-retrieve'"
  (if (buffer-live-p sl--realtime-buffer)
      (with-current-buffer sl--realtime-buffer
        (sl--realtime-update-buffer station json)))
  ;(sl--cleanup)
  )

(defun sl--realtime-request (&optional station timewindow)
  "Perform a SL real-time data request for STATION."
  (let ((station (or station sl-station)))
    (sl--api-request "SL Realtidsinformation 4"
                     (list (cons "SiteId" (cadr (assoc station sl-stations)) )
                           (cons "TimeWindow" (or timewindow sl-timewindow)))
                     `(lambda (json) (sl--realtime-request-callback ,station json )))))

(defun sl--cleanup ()
  "Cleanup sl-mode state."
  (when sl--realtime-update-timer
    (cancel-timer sl--realtime-update-timer)
    (setq sl--realtime-update-timer nil))
  (when (buffer-live-p sl--realtime-buffer)
    (delete-windows-on sl--realtime-buffer)
    (kill-buffer sl--realtime-buffer)
    (setq sl--realtime-buffer nil)))

(defun sl--realtime-buffer-killed-hook-fn ()
  "Cleanly exit sl-mode state when buffer is killed."
  (when (eq (current-buffer) sl--realtime-buffer)
    (setq sl--realtime-buffer nil)
    (sl--cleanup)))

(define-derived-mode sl-mode special-mode "SL"
  "Mode for displaying real-time SL departures"
  (if (called-interactively-p nil)
      (error "Use M-x sl")
    (add-hook 'kill-buffer-hook 'sl--realtime-buffer-killed-hook-fn)
    (setq truncate-lines t)
    (read-only-mode 1)
    (setq sl--realtime-buffer (current-buffer))
    (setq sl--realtime-update-timer
          (run-at-time t sl-update-interval 'sl-update))
    (sl-update)))

(provide 'sl-mode)
