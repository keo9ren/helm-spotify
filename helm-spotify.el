;;; helm-spotify.el --- Control Spotify with Helm.
;; Copyright 2013 Kris Jenkins
;;
;; Author: Kris Jenkins <krisajenkins@gmail.com>
;; Maintainer: Kris Jenkins <krisajenkins@gmail.com>
;; Keywords: helm spotify
;; URL: https://github.com/krisajenkins/helm-spotify
;; Created: 14th October 2013
;; Version: 0.1.1
;; Package-Requires: ((helm "0.0.0") (multi "2.0.0"))

;;; Commentary:
;;
;; A search & play interface for Spotify.
;;
;; Currently supports OSX, Linux & Windows.
;;
;; (Want support for another platform? There's a guide in the github README.)

;;; Code:

;;; API Reference: https://developer.spotify.com/technologies/web-api/
(require 'url)
(require 'json)
(require 'helm)
(require 'multi)
(require 'find-file)
(require 'levenshtein)
(require 'cl-lib)
(require 'seq)

(defcustom spotify-server
  "spotify"
  "Locaton of SPOTIFY-SERVER."
  :group 'helm-spotify
  :type 'file)

(defconst spotify-server-buffer-name "*spotify*" "The spotify buffer name.")

(defun alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (alist-get (cdr symbols)
		 (assoc (car symbols) alist))
    (cdr alist)))

(defmulti spotify-play-href (href)
  "Get the Spotify app to play the object with the given HREF."
  system-type)

(defmulti-method spotify-play-href 'darwin
  (href)
  (shell-command (format "osascript -e 'tell application %S to play track %S'"
			 "Spotify"
			 href)))

(defmulti-method spotify-play-href 'gnu/linux
  (href)
  ;;need to pause before open uri/spotify app curiosity
  (shell-command (format "dbus-send --session --type=method_call --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Pause"))
  (shell-command (format "dbus-send --session --type=method_call --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.OpenUri \"string:%s\""
			 href)))

(defmulti-method spotify-play-href 'windows-nt
  (href)
  (shell-command (format "explorer %S" href)))

(defmulti-method-fallback spotify-play-href
  (href)
  (message "Sorry, helm-spotify does not support playing tracks on %S." system-type))

(defun spotify-play-track (track)
  "Get the Spotify app to play the TRACK."
  (spotify-play-href (alist-get '(uri) track)))

(defun spotify-play-album (track)
  "Get the Spotify app to play the album for this TRACK."
  (spotify-play-href (alist-get '(album uri) track)))


(defun spotify-search (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
(let ((a-url (format "http://api.spotify.com/v1/search?type=track&q=%s" search-term)))
    (with-current-buffer
	(url-retrieve-synchronously a-url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun spotify-format-track (track)
  "Given a TRACK, return a a formatted string suitable for display."
  (let ((track-name   (alist-get '(name) track))
    (track-length (/ (alist-get '(duration_ms) track) 1000)) 
	(album-name   (alist-get '(album name) track))
	(artist-names (mapcar (lambda (artist)
				(alist-get '(name) artist))
			      (alist-get '(artists) track))))
    (format "%s (%dm%0.2ds)\n%s - %s"
	    track-name
	    (/ track-length 60) (mod track-length 60)
	    (mapconcat 'identity artist-names "/")
	    album-name)))

(defun spotify-search-formatted (search-term)
  (mapcar (lambda (track)
	    (cons (spotify-format-track track) track))
	  (alist-get '(tracks items) (spotify-search search-term))))


(defun helm-spotify-search ()
  (spotify-search-formatted helm-pattern))

(defun helm-spotify-actions-for-track (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (alist-get '(name) track))       . spotify-play-track)
    (,(format "Play Album - %s" (alist-get '(album name) track)) . spotify-play-album)
    ("Show Track Metadata" . pp)))

(defun helm-spotify-interface ()
  "Bring up a Spotify search interface in helm."
  (setq debug-on-error t)
  (helm :sources '(helm-source-spotify-track-search)
        :buffer "*helm-spotify*")
  (setq debug-on-error nil)
)

;;;###autoload
(defvar helm-source-spotify-track-search
  '((name . "Spotify")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern . 2)
    (candidates-process . helm-spotify-search)
    (action-transformer . helm-spotify-actions-for-track)))


;;;###autoload
(defun helm-spotify ()
  "HELM-SPOTIFY."
  (interactive)
  ;(helm-spotify-start)
  (helm-spotify-interface)
)

;;;###autoload
(defun helm-spotify-start ()
  "HELM-SPOTIFY.."
  (interactive)
  (when (featurep 'helm)
    (unless (spotify--process-running-p spotify-server)
      (let ((buf (get-buffer-create spotify-server-buffer-name)))
        (with-current-buffer buf (start-process "spotify" (current-buffer)
                                                spotify-server))))))

(defun spotify--process-running-p (name)
  "If a process called NAME is running or not."
  (or (get-process name) (spotify--system-process-running-p name)))

(defun spotify--filter (pred seq)
  "Apply PRED to filter SEQ."
  (delq nil
        (mapcar (lambda (x) (and (funcall pred x) x)) seq)))

(defun spotify--system-process-running-p (name)
  "If a process called NAME is running on the system."
  (let* ((all-args (mapcar (lambda (x) (cdr (assq 'args (process-attributes x)))) (list-system-processes)))
         (match-args (spotify--filter (lambda (x) (spotify--string-match (concat "\\b" name "\\b") x)) all-args))
         )
    (not (null match-args))))

(defun spotify--string-match (regexp name)
  "Wrap 'string-match' of REGEXP and NAME to make sure we don't pass it a nil string."
  (when name
    (string-match regexp name)))



(provide 'helm-spotify)
;;; helm-spotify.el ends here
