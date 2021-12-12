;;; editing.el --- Dan's editing customizations
;;; lisp/editing.el -*- lexical-binding: t; -*-

;;; Commentary:
;;; This package provides some basic editing adjustments.

;;; Code:

(defun start-of-line-at-pos (p)
  "Return the position of the start of the line at position P."
  (save-excursion
    (goto-char p)
    (line-beginning-position)))

(defun end-of-line-at-pos (p)
  "Return the position of the end of the line at position P."
  (save-excursion
    (goto-char p)
    (line-end-position)))

;;; Hooks

(add-hook! clojurescript-mode-map #'paredit-mode)

;;; Keybindings:

(map! :map smartparens-mode-map
      :nvie "C->" #'sp-forward-slurp-sexp
      :nvie "C-<" #'sp-forward-barf-sexp
      :nvie "C-{" #'sp-backward-slurp-sexp
      :nvie "C-}" #'sp-backward-barf-sexp)

(map! :map paredit-mode-map
      :nvie ")" #'paredit-close-round)
