;;; codex-completion.el --- OpenAI Codex powered Code Completion -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum AT gmail DOT com>
;; Description: Generate code from program description and code
;; Keywords: abbrev, matching, auto-complete, programming
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/debanjum/codex-completion

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package enables code generation, completion using Codex by OpenAI
;; The models code generation capabilities are exposed via an HTTP API
;; Pass instructions as code, code comments, code skeleton or a combination
;; The model will generate code based on your instructions

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

(defvar url-http-end-of-headers)

(defcustom codex-completion-openai-api-token nil
  "The Token to access the OpenAI API."
  :group 'codex-completion
  :type 'string)

(defcustom codex-completion-openai-completion-url "https://api.openai.com/v1/completions"
  "The URL to access the OpenAI API completion endpoint."
  :group 'codex-completion
  :type 'string)

(defcustom codex-completion-openai-model "code-davinci-002"
  "The OpenAI code completion model."
  :group 'codex-completion
  :type 'string)

(defun codex-completion--get-current-paragraph-until-point ()
  "Return text from point to previous empty line."
  (interactive)
  (replace-regexp-in-string
   "^[ \t]*\n" ""
   (buffer-substring-no-properties
    (point)
    (save-excursion
      (backward-paragraph)
      (point)))))

(defun codex-completion--get-current-paragraph-after-point ()
  "Return text from point to next empty line."
  (interactive)
  (replace-regexp-in-string
   "^[ \t]*\n" ""
   (buffer-substring-no-properties
    (point)
    (save-excursion
      (forward-paragraph)
      (point)))))

(defun codex-completion--get-completion-from-api ()
  "Call OpenAI API and return suggested completion from response."
  (car
   (with-current-buffer
       (url-retrieve-synchronously
        codex-completion-openai-completion-url)
     (goto-char url-http-end-of-headers)
     ;; extract completion from response
     (codex-completion--get-completions-from-response (json-read)))))

(defun codex-completion--get-completions-from-response (json)
  "Get the completion suggestions from the JSON response."
  (let ((completions (cdr (assoc 'choices json))))
    (mapcar (lambda (completion)
              (cdr (assoc 'text completion)))
            completions)))

;;;###autoload
(defun codex-completion-complete-region (beginning end)
  "Make OpenAI Codex generate code completion.
Take current active region from BEGINNING to END as context."
  (interactive "r")
  (let* ((region (buffer-substring-no-properties beginning end))
         (bearer-token (format "Bearer %s" codex-completion-openai-api-token))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,bearer-token)))
         (url-request-data
          (json-encode `(("prompt" . ,region)
                         ("model" . ,codex-completion-openai-model)
                         ("max_tokens" . 64)
                         ("temperature" . 0)))))
    (insert
     (codex-completion--get-completion-from-api))))

;;;###autoload
(defun codex-completion-query (query)
  "Query OpenAI Codex to generate code.
Take QUERY passed by user as context."
  (interactive "sQuery: ")
  (let* ((bearer-token (format "Bearer %s" codex-completion-openai-api-token))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,bearer-token)))
         (url-request-data
          (json-encode `(("prompt" . ,query)
                         ("model" . ,codex-completion-openai-model)
                         ("max_tokens" . 64)
                         ("temperature" . 0)))))
    (insert
     (codex-completion--get-completion-from-api))))

;;;###autoload
(defun codex-completion-complete ()
  "Make OpenAI Codex generate code completion.
Provide current paragraph split by point as context."
  (interactive)
  (let* ((prefix (codex-completion--get-current-paragraph-until-point))
         (suffix (codex-completion--get-current-paragraph-after-point))
         (bearer-token (format "Bearer %s" codex-completion-openai-api-token))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,bearer-token)))
         (url-request-data
          (json-encode `(("prompt" . ,prefix)
                         ("suffix" . ,suffix)
                         ("model" . ,codex-completion-openai-model)
                         ("max_tokens" . 256)
                         ("temperature" . 0)))))
    (insert
     (codex-completion--get-completion-from-api))))

(provide 'codex-completion)

;;; codex-completion.el ends here
