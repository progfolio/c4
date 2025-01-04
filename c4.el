;;; c4.el --- Connect 4                              -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Nicholas Vollmer

;; Author: Nicholas Vollmer <nv@parenthetic.dev>
;; Keywords: games
;; Package-Requires: ((emacs "29") (imp "0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;grid = 7 across x 6 down ;;21 each color
;; gravity

;; +1-2-3-4-5-6-7+
;; |x o x o x o x|
;; |x o x o x o x|
;; |x o x o x o x|
;; |x o x o x o x|
;; |x o x o x o x|
;; |x o x o x o x|
;; +-------------+

;;; Code:
(require 'imp)
(defvar-local c4-grid nil)
(defvar c4--server-process nil)
(defvar c4--server-p nil)
(defvar c4--server-buffer nil)
(defvar c4--current-player 0)
(defvar c4--players nil)
(defconst c4-buffer "*c4*")
(defconst c4-log-buffer "*c4 client log*")
(defconst c4--grid '((0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)))

(defun c4--print-invite ()
  "Print invite from server."
  (add-hook 'imp-server-hook
            (lambda ()
              (with-temp-buffer
                (yank)
                (message "IMP INVITE: (c4 %S)" (buffer-string))))))

(defun c4-server ()
  "Start c4 server."
  (interactive)
  (setq c4--server-process
        (make-process
         :name "c4-server"
         :buffer "*c4-server*"
         :command (list (concat invocation-directory invocation-name)
                        "-l" (locate-library "imp")
                        "-l" (locate-library "c4")
                        "-q"
                        "--fg-daemon=c4"
                        "--eval" "(setq c4--server-p t)"
                        "--eval" "(c4)"
                        "--eval" "(c4--print-invite)")))
  (with-current-buffer (process-buffer c4--server-process)
    (display-buffer (current-buffer))))

(defun c4--send (message)
  "Send MESSAGE."
  (when (buffer-live-p c4--server-buffer)
    (with-current-buffer c4--server-buffer
      (erc-send-message (format "%S" message)))))

(defun c4--check-grid (grid)
  "Check game GRID."
  (cl-loop
   named loop
   with rowcount = (length grid)
   for r below rowcount
   for row = (nth r grid) do
   (cl-loop
    with colcount = (length row)
    for c below colcount
    for col = (nth c row) do
    (cond
     ((zerop col) nil)
     ;; horizontal win
     ((and (<= c (- colcount 4))
           (apply #'= (cl-subseq row c (+ 4 c))))
      (cl-return-from loop (list 'winner col)))
     ((and (<= r (- rowcount 4))
           (or
            ;; vertical win
            (apply #'= (cl-loop for row in (cl-subseq grid r (+ 4 r))
                                collect (nth c row)))
            (and (<= c (- colcount 4))
                 ;; diagonals
                 (cl-loop with right = (1- c)
                          with left = (1+ c)
                          for row in (cl-subseq grid r (+ 4 r))
                          collect (nth (cl-incf right) row) into rights
                          when (>= c 3) collect (nth (cl-decf left) row) into lefts
                          finally return (or (unless (memq 0 rights)
                                               (when rights (apply #'= rights)))
                                             (unless (memq 0 lefts)
                                               (when lefts (apply #'= lefts))))))))
      (cl-return-from loop (list 'winner col)))
     (t (unless (memq 0 (flatten-tree grid))
          (cl-return-from loop (list 'draw))))))))

(defun c4--drop (n col grid)
  "Put N in lowest available row at GRID COL."
  (cl-loop with col = (1- col)
           for i downfrom (length grid) to 0
           for row = (nth i grid)
           when row do
           (when (zerop (nth col row))
             (setf (nth col row) n)
             (cl-return grid))
           finally (user-error "Invalid move")))

(defun c4--draw (grid)
  "Draw GRID."
  (with-temp-buffer
    (insert "╔1═2═3═4═5═6═7╗\n")
    (cl-loop
     for row in grid
     for rowlen = (length row)
     for limit = (1- rowlen)
     do
     (insert "║")
     (cl-loop for i below rowlen
              for col = (nth i row)
              do (insert (alist-get col `((1 . ,(propertize "■" 'face 'success))
                                          (2 . ,(propertize "■" 'face 'error)))
                                    "☐"))
              (insert (if (= i limit) "║\n" " "))))
    (insert "╚═════════════╝")
    (buffer-string)))

(defun c4--sender (message)
  "Return sender's user name from MESSAGE."
  (car (split-string (erc-response.sender message) "!~")))

(defun c4-drop-piece (n)
  "Drop a piece into Nth column."
  (interactive "P")
  (unless (derived-mode-p 'c4-mode) (user-error "Buffer not in c4-mode"))
  (when (null n) (user-error "Missing column number"))
  (with-current-buffer c4--server-buffer
    (c4--send (list imp-name (min (max 0 n) 7)))))

(defvar c4-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") 'c4-drop-piece)
    m))

(define-derived-mode c4-mode special-mode "c4-mode"
  "Major mode for playing connect 4.")

(defun c4--current-player ()
  "Return current player's name."
  (nth (mod c4--current-player 2) c4--players))

(defun c4--server-join-player (message)
  "Add players to game via MESSAGE."
  (when-let ((user (c4--sender message))
             ((string-prefix-p "c4-player" user)))
    (cond ((= (length c4--players) 2) (c4--send (list user 'spectator)))
          ((= (length c4--players) 1)
           (cl-pushnew user c4--players)
           (setq c4--current-player (random 2))
           (c4--send (list 'state (c4--current-player) c4--grid)))
          ((null c4--players) (push user c4--players)))
    (message "PLAYERS: %S" c4--players)))

(defun c4--server-read-move (message)
  "Read player move from MESSAGE."
  (when-let ((message (ignore-errors (read (erc-response.contents message))))
             (current-player (c4--current-player)))
    (cond ((equal (car-safe message) current-player) ;;move response
           (condition-case _
               (progn
                 (setq c4--grid (c4--drop
                                 (1+ (cl-position current-player c4--players :test #'equal))
                                 (cadr message)
                                 c4--grid))
                 (cl-incf c4--current-player)
                 (c4--send (list 'state (c4--current-player) c4--grid))
                 (when-let ((conclusion (c4--check-grid c4--grid)))
                   (unless (eq (car conclusion) 'draw)
                     (setf (cadr conclusion)
                           (nth (1- (cadr conclusion)) c4--players)))
                   (c4--send conclusion)))
             ((user-error) (c4--send (list current-player 'invalid-move))))))))

(defun c4--client-message-handler (message)
  "Handle server MESSAGE."
  (with-current-buffer (get-buffer-create c4-log-buffer)
    (goto-char (point-max))
    (insert (with-slots (sender contents) message
              (format "%s: %S \n" sender contents))))
  (when-let ((message (ignore-errors (read (erc-response.contents message))))
             ((listp  message))
             (client (with-current-buffer c4--server-buffer imp-name)))
    (with-current-buffer c4-buffer
      (when (eq (car-safe message) 'state)
        (erase-buffer)
        (insert (c4--draw (car (last message)))))
      (when (eq (cadr message) 'invalid-move) (message "Invalid move"))
      (when-let  (((eq (car-safe message) 'winner))
                  (winner (cadr message)))
        (with-silent-modifications
          (animate-string
           (format "YOU %s!" (if (equal winner client) "WON" "LOST"))
           0)))
      (when-let  (((eq (car-safe message) 'draw)))
        (with-silent-modifications (animate-string "DRAW!" 0)))
      (when (eq (car-safe message) 'state)
        (setq header-line-format
              (if (equal (nth 1 message) client)
                  (concat "Your Turn. Press 1-7 followed by "
                          (substitute-command-keys "\\<c4-mode-map>\\[c4-drop-piece]"))
                (format "Waiting for %s..." (nth 1 message))))))))

(defun c4-read-message (message)
  "Read game MESSAGE."
  (if c4--server-p
      (let ((command (erc-response.command message)))
        (message "%S" message)
        (cond ((equal command "JOIN") (c4--server-join-player message))
              ((equal command "PRIVMSG") (c4--server-read-move message))))
    (c4--client-message-handler message)))

(defun c4-quit (&optional _)
  "Kill c4 related buffers.
If SERVER is non-nil, also attempt to kill server process/buffer."
  ;;@FIX; make extensible, don't hard code libera
  (interactive (list current-prefix-arg))
  (cl-loop for buffer in (list c4--server-buffer c4-log-buffer
                               "Libera.Chat" "*c4-server*" "*c4*")
           for b = (get-buffer buffer)
           when (buffer-live-p b) do (kill-buffer b)))

;;;###autoload
(defun c4 (&optional invite)
  "Play c4. If INVITE is non-nil, join that game."
  (interactive)
  (if (and (not (or invite c4--server-p))
           (yes-or-no-p "Start c4 server?"))
      (c4-server) ;;@TODO: autojoin as client through process filter?
    (let* ((tokens (and invite (split-string (string-trim invite) "@")))
           (channel (or (car tokens) (format "#c4-server-%s" (imp--key))))
           (key (or (cadr tokens) (imp--key)))
           (name (format "c4-%s-%s" (if c4--server-p "server" "player") (imp--key))))
      (add-hook 'imp-server-hook
                (lambda ()
                  (when (equal imp-channel-name channel)
                    (add-hook 'imp-message-functions #'c4-read-message nil t)
                    (put 'imp-message-functions 'permanent-local-hook t)
                    (let ((b (current-buffer)))
                      (with-current-buffer (get-buffer-create c4-buffer)
                        (with-silent-modifications
                          (c4-mode)
                          (erase-buffer)
                          (setq c4--server-buffer b)
                          (put 'c4--server-buffer 'permanent-local t)
                          (insert "Waiting for opponent ...")
                          (pop-to-buffer (current-buffer))))))))
      (imp-client channel key name (not invite)))))

(provide 'c4)
;;; c4.el ends here
