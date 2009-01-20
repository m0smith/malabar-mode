;;; malabar-mode.el --- A better Java mode for Emacs
;;
;; Copyright (c) 2009 Espen Wiborg <espenhw@grumblesmurf.org>
;;
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.
;;

(require 'wisent-java-malabar-wy)

(define-derived-mode malabar-mode java-mode "malabar"
  "A new, better, Java mode."
  ;; Funky stuff here
  (setq semantic-lex-analyzer 'wisent-java-malabar-lexer)
  (wisent-java-malabar-wy--install-parser)
  (remove-hook 'java-mode-hook 'wisent-java-default-setup)
  )

(provide 'malabar-mode)
