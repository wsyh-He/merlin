;;; merlin-cap.el --- Merlin and completion-at-point integration.   -*- coding: utf-8 -*-
;; Licensed under the MIT license.

;; Author: Simon Castellan <simon.castellan(_)iuwt.fr>
;;         Frédéric Bour <frederic.bour(_)lakaban.net>
;;         Thomas Refis <thomas.refis(_)gmail.com>
;; Created: 15 May 2015
;; Version: 0.1
;; Keywords: ocaml languages
;; URL: http://github.com/the-lambda-church/merlin

(require 'merlin)

;; Call merlin-completion-at-point when you want merlin guided completion-at-point.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME Some internal functions from merlin are used in this file:
;; - merlin--completion-format-entry
;; - merlin--completion-full-entry-name
;; - merlin--completion-prefix
;; - merlin--completion-data
;;
;; It would be nice to define a proper (somewhat stable) interface in merlin.el
;; to be used by other modules.

;; Internal variables

(defvar-local merlin-cap--table nil
  "Hold a table mapping completion candidates to their types.")

(defvar-local merlin-cap--cache (cons "" 0)
  "The cache for calls to completion-at-point so that it does not
trigger useless merlin calls.")

;; Internal functions

(defun merlin-cap--table (string pred action)
  "Implement completion for merlin using `completion-at-point' API."
  (if (eq 'metadata action)
      (when merlin-completion-types
        '(metadata ((annotation-function . merlin-cap--annotate)
                    (exit-function . merlin-cap--lookup))))
    (complete-with-action action merlin-cap--table string pred)))


;; Public functions

(defun merlin-cap ()
  "Perform completion at point with merlin."
  (lexical-let*
      ((bounds       (merlin/completion-bounds))
       (start        (car bounds))
       (end          (cdr bounds))
       (prefix       (merlin/buffer-substring start end))
       (compl-prefix (merlin--completion-prefix prefix)))
    (when (or (not merlin-cap--cache)
              (not (equal (cons prefix start) merlin-cap--cache)))
      (setq merlin-cap--cache (cons prefix start))
      (merlin/sync-to-point)
      (setq merlin-cap--table
            (mapcar
              (lambda (a)
                (cons (merlin--completion-full-entry-name compl-prefix a)
                      (concat ": " (merlin--completion-format-entry a))))
              (merlin--completion-data prefix))))
    (list start end #'merlin-cap--table
          . (:exit-function #'merlin-cap--lookup
             :annotation-function #'merlin-cap--annotate))))

(defalias 'merlin-completion-at-point 'merlin-cap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register into completion-at-point ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-cap--setup ()
  (add-hook 'completion-at-point-functions #'merlin-completion-at-point nil 'local))

(add-hook 'merlin-mode-hook 'merlin-cap--setup)

(provide 'merlin-cap)
;;; merlin-cap.el ends here