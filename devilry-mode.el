;; Change "nil" to "t" if you have used Gard's python script
;;  "sort_deliveries.py" on the delivery folder. In other words if your
;; files are arranged as shown below:

;; -username1_folder
;; ----Source_file1.java
;; ----Source_file2.java
;; ----README.txt
;; -username2_folder
;; ----Source_file1.java
;; ----Source_file2.java
;; ----README.txt
(setq devilry-easy-file-system nil)


;; Java compilation
;; Change "t" to "nil" if you don't want java compilation
(setq devilry-java-compilation t)

;; Deletion of .class files after compilation
;; (only applicable if devilry-java-compilation is t)
;; Change "nil" to "t" if you want to delete .class files after compilation
(setq devilry-rm-output-files t)

;; Automatic indentation
;; Change "nil" to "t" if you want to automatic indet and remove
;; white space every time you start correcting an oblig
(setq devilry-indent-code nil)

;; Fetching devilry-mode's file location
(setq devilry-mode-location (file-name-directory load-file-name))

;; To tidy up a buffer, created by simenheg
(defun tidy ()
  "Ident, untabify and unwhitespacify current buffer, or region if active."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (indent-region beg end)
    (whitespace-cleanup)
    (untabify beg (if (< end (point-max)) end (point-max)))))


;; Yank inside devilry-markdown code block
(defun devilry-yank-java-block()
  (interactive)
  (insert "``` java")
  (newline)(yank)(newline)
  (insert "```")(newline))


;; Smart for getting stuff on devilry, but not on disk
(defun devilry-add-old-feedback()
  (interactive)
  (save-buffer)
  (kill-buffer)
  ;; Get username and create feedback-file
  (setq username (read-string "Skriv inn brukernavn: "))
  ;; Ask for a real username
  (while (string= username "")
    (setq username (read-string "Skriv inn et ordentlig brukernavn: ")))

  ;; Open the file in the new window
  (find-file (concat feedback-dir-path username "/<oblignr>.txt")))


;; Activating markdown-mode if installed
(defun safe-markdown-mode()
  (when (require 'markdown-mode nil 'noerror)
    (markdown-mode)))

;; Kill everything without saving
(defun desktop-hard-clear ()
  (interactive)
  (when (y-or-n-p (concat "Close all buffers without saving?"))
    (dolist (buffer (buffer-list))
      (let ((process (get-buffer-process buffer)))
        (when process (process-kill-without-query process))
        (set-buffer buffer)
        (set-buffer-modified-p nil)
        (kill-this-buffer)))
    (delete-other-windows)))


;; Shows readme buffer if it exists
;; This is kind of horrible
(defun devilry-show-readme()
  (interactive)
  (cond
   ((get-buffer "README.txt") (switch-to-buffer "README.txt"))
   ((get-buffer "Readme.txt") (switch-to-buffer "Readme.txt"))
   ((get-buffer "readme.txt") (switch-to-buffer "readme.txt"))
   ((get-buffer "README.TXT") (switch-to-buffer "README.TXT"))
   ((get-buffer "ReadMe.txt") (switch-to-buffer "ReadMe.txt"))
   (t (message "Could not find README.txt"))))


;; Inserts the template and adds username et end of first line
(defun insert-devilry-template (username file-path)
  (insert-file-contents file-path)
  (move-end-of-line nil)
  (insert " - " username)
  (end-of-buffer)
  (insert (concat "\nRettet: " (current-time-string)))
  (move-beginning-of-line nil))


;; Create a new feedback file in the right folder
;; Splits windows and shows the two previous feedback files
(defun devilry-create-new-and-show-old-feedback()

  ;; Getting username from the the file path of the current buffer
  ;; Ask until we get a valid username
  (setq username (devilry-get-username))
  (while (not (yes-or-no-p (concat "Correcting \"" username "\" Is this a valid username?")))
    (if devilry-easy-file-system
        (if (yes-or-no-p "The variable \"devilry-easy-file-system\" is currently \"t\""
			 "but the files are not organized this way. Change to normal file mode?")
            (progn
              (setq devilry-easy-file-system nil)
              (read-string "File mode changed to normal. Consider to set the variable \""
			   "devilry-easy-file-system to \"nil\" in devilry-mode.el")
              (setq username (devilry-get-username)))
          (setq username (read-string "Type the correct username: ")))
      (setq username (read-string "Type the correct username: "))))

  (while (not (file-exists-p (concat feedback-dir-path username)))
    (if (yes-or-no-p (concat "Can't find directory " feedback-dir-path username ". Create it?"))
        (make-directory (concat feedback-dir-path username) t)
      (setq username (read-string
                      (concat "Please give a valid username. (Must be a folder in path "
                              feedback-dir-path "): ")))))

  ;; Calculate paths to new and the two previous feedback files
  (setq newFilePath (concat feedback-dir-path username "/" oblig-number ".txt"))
  (setq prevFilePath (concat feedback-dir-path username "/"
                             (number-to-string (- (string-to-number oblig-number) 1)) ".txt"))
  (setq oldFilePath (concat feedback-dir-path username "/"
                            (number-to-string (- (string-to-number oblig-number) 2)) ".txt"))

  ;; Create new feedback-file or open it if it exists
  (find-file newFilePath)

  (with-selected-window (split-window-right)
    (find-file newFilePath)

    ;; Check if we have been editing this feedback file before
    (when (eq (buffer-size) 0)
      (insert-devilry-template username feedback-template-path))

    (safe-markdown-mode)

    ;; Split window again if old feedback-file exists
    (when (file-exists-p prevFilePath)
      (with-selected-window (split-window-below)
        (find-file-read-only prevFilePath)
        (end-of-buffer)

        ;; Activating markdown-mode
        (safe-markdown-mode)

        (when (file-exists-p oldFilePath)
          (with-selected-window (split-window-below)
            (find-file-read-only oldFilePath)
            (end-of-buffer)

            ;; Activating markdown-mode
            (safe-markdown-mode)))))))


;; Try to compile .java files, shows eventual errors in new split window below.
;; It is the last modified window or something, tried to hack it. It somehow works
(defun devilry-compile-all-open-java-files ()
  ;; Find the first java buffer, switch to it, comiple all from there
  ;;  to make sure we are in the correct directory
  (dolist (buf (buffer-list))
    (let ((buf-name (buffer-name buf)))
      (when (string= (substring buf-name -5) ".java")
        (with-current-buffer buf
          (shell-command "javac *.java")

          ;; Delete output files after compilation. Is set at top of this file
          (when devilry-rm-output-files
            (message "Deleting output files.")
            (if (eq system-type 'windows-nt)
                (shell-command "del *.class")
              (shell-command "rm *.class"))))

        ;; Show eventual errors from compilation in window below
        (if (> (buffer-size (get-buffer "*Shell Command Output*")) 0)
	    (let ((output-window (split-window-below)))
	      (progn
		(message "Compilation gave errors.")
		(with-selected-window output-window
		  (pop-to-buffer-same-window "*Shell Command Output*"))))
	  (message "Compilation completed sucessfully."))
        (return)))))


;; Compile all java files
;; Remove output .class files
;; Find README and switch to that buffer
(defun devilry-do-oblig ()
  (interactive)
  (delete-other-windows)

  ;; Indent code automatically. Is set at the top of this file.
  (when devilry-indent-code (tidy-all-buffers))

  ;; Create new and show previous feedback files on the right
  (devilry-create-new-and-show-old-feedback)

  ;; Show readme if it exists
  (devilry-show-readme)

  ;; Compile .java files
  (when devilry-java-compilation
    (devilry-compile-all-open-java-files)))

;; Writes updated data to file
(defun write-data ()
    
  ;; Construct data-string for file-insertion
  (let ((str (concat feedback-dir-path "\n" feedback-template-path "\n" oblig-number)))

    ;; Write to file
    (write-region str nil settings-file)
    (message "Updated data (settings-file)")))


;; Gets data from file
(defun read-data ()
  (with-temp-buffer
    (when (file-exists-p settings-file)
      (insert-file-contents settings-file)

      (let ((beg (point))) (end-of-line) (copy-region-as-kill beg (point)))
      (setq feedback-dir-path (car kill-ring-yank-pointer))
      (let ((beg (progn (goto-line 2) (point)))) (end-of-line) (copy-region-as-kill beg (point)))
      (setq feedback-template-path (car kill-ring-yank-pointer))
      (let ((beg (progn (goto-line 3) (point)))) (end-of-line) (copy-region-as-kill beg (point)))
      (setq oblig-number (car kill-ring-yank-pointer)))))


;; Initiates the system
(defun devilry-init ()
  (setq settings-file (concat devilry-mode-location "devilry-mode.settings"))
  (read-data)

  ;; Check if we need to write to file
  (let ((data-updated nil))
    ;; Check if not file exists
    (if (not (file-exists-p settings-file))
        (progn
          (message "Data file \"%s\" does not exist" settings-file)
          (setq oblig-number (read-string "Oblig number: "))
          (setq feedback-dir-path (read-string "Path to feedback directory: "))
          (setq feedback-template-path (read-string "Path to feedback template: "))
          (setq data-updated t))

      ;; Check if user wants to update data, i.e new template path
      (when (y-or-n-p (concat "Change oblig number? (is " oblig-number ") "))
        (setq oblig-number (read-string "Oblig number: "))
        (setq data-updated t))

      (when (y-or-n-p (concat "Change feedback directory? (is " feedback-dir-path ") "))
        (setq feedback-dir-path (read-string "Path to feedback directory: "))
        (setq data-updated t))

      (when (y-or-n-p (concat "Change feedback template path? (is " feedback-template-path ") "))
        (setq feedback-template-path (read-string "Path to feedback template: "))
        (setq data-updated t)))

    ;; Check if the paths are valid, if not, ask to create directories
    (while (not (file-exists-p feedback-dir-path))
      (if (yes-or-no-p (concat "Feedback directory does not exist (" feedback-dir-path "). Create it? "))
          (make-directory feedback-dir-path t)
        (setq feedback-dir-path (read-string "Path to feedback directory: "))))

    (while (not (file-exists-p feedback-template-path))
      (if (yes-or-no-p (concat "Feedback template does not exist (" feedback-template-path "). Create it? "))
          (progn
            ;; Creating feedback file, making sure the parent directories exist
            (let ((dir (file-name-directory feedback-template-path)))
              (unless (file-exists-p dir) (make-directory dir t)))
            (write-region (concat "# INF1000 - Assignment " oblig-number "\n"
				  "## Exercise 1\n## Exercise 2\n## Exercise 3\n\n"
				  "## Generally:\n\n\n### **Approved**")
			  nil feedback-template-path))
        (setq feedback-template-path (read-string "Path to feedback directory: "))))

    ;; If we have new varables or could not find data we have to write to file
    (when data-updated
      (write-data))))

(defun reverse-string (str)
  (interactive)
  (apply
   #'string
   (reverse (string-to-list str))))


;; Fetches username from the path of the current buffer file.
(defun devilry-get-username ()
  (interactive)
  (if devilry-easy-file-system
      (reverse-string
       (nth 1
            (split-string (reverse-string (buffer-file-name)) "/")))
    (car
     (split-string
      (reverse-string
       (nth 3
            (split-string
             (reverse-string (buffer-file-name))  "/"))) " "))))

;; The mode
(define-minor-mode devilry-mode
  nil
  :lighter " Devilry"
  :global t
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f5>") 'devilry-do-oblig)
            (define-key map (kbd "<f6>") 'desktop-hard-clear)
            (define-key map (kbd "C-, y") 'devilry-yank-java-block)
            map)
  ;; This will be run every time the mode is toggled on or off
  ;; If we toggled the mode on, run init function
  (when (and devilry-mode (boundp 'devilry-mode))
    (devilry-init)))

(provide 'devilry-mode)
