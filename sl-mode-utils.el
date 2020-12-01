;;; sl-mode-utils.el --- Util functions for use with sl-mode.el

;; Copyright (C) 2020 Peter Gille

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'url)
(require 'json)

(defun sl--api-build-url (api keys)
  "Given an API name and key:value-pairs, build a request url"
  (concat (cdr (assoc api sl-api-endpoints))
          "?"
          (mapconcat (lambda (x)
                       (concat (car x) "=" (cdr x)))
                     (cons (cons "key" (cdr (assoc api sl-api-keys))) keys)
                     "&")))

(defun sl--api-request (api keys callback)
  "Perform a request to the SL api.
API is the name of the SL api.  KEYS are the url key:values. callback is the callback function"
  (url-retrieve (sl--api-build-url api keys)
                (lambda (status callback)
                  ;; Start of buffer
                  (goto-char (point-min))
                  ;; Move past http headers
                  (re-search-forward "^$")
                  ;; Now we are at the actual data
                  (let* ((json-object-type 'hash-table)
                         (json-array-type 'list)
                         (json-key-type 'string)
                         (json (json-parse-buffer)))
                    (funcall callback json)))
                  (list callback)))

(defun sl--extract-station-data (json)
  "Extract SL station data from a json response to an api query
against the station information api with the argument model=site"
  (mapcar (lambda (station)
            (list (gethash "SiteName" station) (gethash "SiteId" station)))
          (gethash "Result" (gethash "ResponseData" json))))

(defun sl--fetch-and-set-station-data (var)
  "Fetch (and set to a variable) SL station data from the SL api"
  (sl--api-request "SL Hållplatser och Linjer 2"
                   '(("model" . "site"))
                   `(lambda (json)
                      (setq ,var
                         (sl--extract-station-data json)))))

(defun sl--read-station-data (filename)
  "Read SL station data from a file containing the json response
  from the SL station api."
   (let* ((json-object-type 'hash-table)
          (json-array-type 'list)
          (json-key-type 'string)
          (json (json-read-file filename)))
     (sl--extract-station-data json)))

(provide 'sl-mode-utils)
