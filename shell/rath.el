;;; rath --- emacs <-> rath communication
;;; Code:

(defun send-string (string)
  (let* ((size (length string))
         (send (concat (byte-to-string size)
                       string)))
    (process-send-string process send)))

(defun region-bytes (start end)
  "Return the number of bytes used by the region."
  (interactive "r")
  (message "Region has %d bytes"
           (- (bufferpos-to-filepos end 'exact)
              (bufferpos-to-filepos start 'exact))))


(setq process (make-serial-process :port "/dev/ttyUSB2"
                                   :speed 115200
                                   :flowcontrol 'hw
                                   :bytesize 8
                                   ))

(dotimes (i 500)
  (send-string "The rain in spain falls mainly on the plain. The rain in spain falls mainly on the plain. The rain in spain falls mainly on the plain. The rain in spain falls mainly on the plain. The rain in spain falls mainly on the plain. ")
  (usleep 100))

(dotimes (i 2)
  (send-string "T"))
;  (usleep 1000))


(length "bla")

(setq cont 1)

(dolist (char (append "The rain in spain falls mainly on the plain " nil))
  (process-send-string process char))

(dotimes (i 500)
  (mapcar (lambda (c)
            (process-send-string process (char-to-string c)))
          "The rain in spain falls mainly on the plain. ")
  (process-send-string process
                       (concat "The rain in Spain falls mainly on the   end. "))

  (length "bla")

  (process-send-string process "The rain in spain falls mainly on the plain.\n")



(while 0
  (process-send-string process "A")
  (process-send-string process "B"))





;;; Commentary:
;; rath is good and sweet
(require 'term)

(defun replace-all (string to-find to-replace)
  "A replace-all fn."
  (let ((index  (cl-search to-find string))
        (pos    0)
        (result ""))
    (while index
      (setq result (concat result
                           (substring string pos index)
                           to-replace)
            pos    (+ index (length to-find))
            index  (cl-search to-find string :start2 pos)))
    (concat result (substring string pos))))

(defun rath-serial-process-filter (process output)
  "Replace LF in output string with CR+LF."
  (term-emulate-terminal process
                         (replace-all output
                                      (byte-to-string ?\n)
                                      (string ?\r ?\n))))

(defun rath-term (port)
  "Basically duplicate SERIAL-TERM from term.el but with process
  filtering to translate LF to CR+LF."
  (interactive (list (serial-read-name)))
  (serial-supported-or-barf)
  (let* ((process (make-serial-process
                   :port port
                   :speed 115200
;                   :bytesize 8
;                   :parity nil
;                   :stopbits 1
                   :flowcontrol 'hw
;                   :coding 'raw-text-unix
;                   :noquery t
                   :name (format "rath:%s" port)
;                   :filter 'rath-serial-process-filter
                   :sentinel 'term-sentinel
                   ))
         (buffer (process-buffer process)))
    (with-current-buffer buffer
      (term-mode)
      (term-line-mode)
      (goto-char (point-max))
      (set-marker (process-mark process) (point)))
    (switch-to-buffer buffer)
    buffer))



(provide 'rath)
;;; rath.el ends here


