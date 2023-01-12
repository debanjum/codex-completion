;;; codex-completion.el --- OpenAI Codex powered Text Completion -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum AT gmail DOT com>
;; Description: Complete, edit code and text with Codex as your AI assistant
;; Keywords: abbrev, matching, auto-complete, programming
;; Version: 0.3.0
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

;; This package enables text and code completion and editing using Codex by OpenAI
;;
;; 1. Text around point, active region and direct instructions provide context
;; 2. This context is passed to online OpenAI models to generate text or code
;; 3. The model generated text and code is edited or inserted into the buffer

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

(defcustom codex-completion-openai-text-model "text-davinci-003"
  "The OpenAI text completion model."
  :group 'codex-completion
  :type 'string)

(defcustom codex-completion-openai-edit-url "https://api.openai.com/v1/edits"
  "The URL to access the OpenAI API edit endpoint."
  :group 'codex-completion
  :type 'string)

(defcustom codex-completion-openai-edit-model "code-davinci-edit-001"
  "The OpenAI code edit model."
  :group 'codex-completion
  :type 'string)

(defcustom codex-completion-openai-text-edit-model "text-davinci-edit-001"
  "The OpenAI text edit model."
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

(defun codex-completion--get-edit-from-api ()
  "Call OpenAI API and return suggested edit from response."
  (car
   (with-current-buffer
       (url-retrieve-synchronously
        codex-completion-openai-edit-url)
     (goto-char url-http-end-of-headers)
     ;; extract edit from response
     (codex-completion--get-completions-from-response (json-read)))))


(defun codex-completion--complete (prompt &optional suffix max-tokens)
  "Make OpenAI Codex generate code or text completion from PROMPT.
Optionally specify SUFFIX, MAX-TOKENS."
  (let* ((model (if (derived-mode-p 'prog-mode) codex-completion-openai-model codex-completion-openai-text-model))
         (temperature (if (derived-mode-p 'prog-mode) 0 0.9))
         (max-tokens (or max-tokens 64))
         (bearer-token (format "Bearer %s" codex-completion-openai-api-token))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,bearer-token)))
         (url-request-data
          (json-encode `(("prompt" . ,prompt)
                         ("model" . ,model)
                         ("suffix" . ,suffix)
                         ("max_tokens" . ,max-tokens)
                         ("temperature" . ,temperature)))))
    (insert
     (codex-completion--get-completion-from-api))))

;;;###autoload
(defun codex-completion-complete-region (beginning end)
  "Make OpenAI Codex generate code or text completion.
Take current active region from BEGINNING to END as context."
  (interactive "r")
  (let ((region (buffer-substring-no-properties beginning end)))
     (codex-completion--complete region)))

;;;###autoload
(defun codex-completion-complete ()
  "Make OpenAI Codex generate code or text completion.
Provide current paragraph split by point as context."
  (interactive)
  (let ((prefix (codex-completion--get-current-paragraph-until-point))
         (suffix (codex-completion--get-current-paragraph-after-point))
         (max-tokens 256))
    (codex-completion--complete prefix suffix max-tokens)))

;;;###autoload
(defun codex-completion-instruct (instruction)
  "Instruct OpenAI Codex to generate or edit (highlighted) code or text.
Take INSTRUCTION passed by user and current active region (if any) as context."
  (interactive "sInstruction: ")
  (let* ((region (if (region-active-p) (buffer-substring-no-properties (region-beginning) (region-end)) ""))
         (model (if (derived-mode-p 'prog-mode) codex-completion-openai-edit-model codex-completion-openai-text-edit-model))
         (temperature (if (derived-mode-p 'prog-mode) 0 0.9))
         (bearer-token (format "Bearer %s" codex-completion-openai-api-token))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,bearer-token)))
         (url-request-data
          (json-encode `(("instruction" . ,instruction)
                         ("input" . ,region)
                         ("model" . ,model)
                         ("temperature" . ,temperature)))))
    (if (region-active-p)
        (save-excursion
          (delete-region (region-beginning) (region-end))
          (insert (codex-completion--get-edit-from-api)))
      (codex-completion--complete instruction))))

;;;###autoload
(defun codex-completion (&optional instruct)
  "Make OpenAI Codex generate or edit (highlighted) code or text.
If INSTRUCT prefix set, ask user for instruction as context.
Else if region active, use current region as context.
Else use current paragraph as context."
  (interactive "P")
  (let ((instruction (if instruct (read-string "Instruction: ") "")))
    (cond
     (instruct
      (codex-completion-instruct instruction))
     ((region-active-p)
      (codex-completion-complete-region (region-beginning) (region-end)))
     (t
      (codex-completion-complete)))))

(provide 'codex-completion)

;;; codex-completion.el ends here
