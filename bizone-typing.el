(require 'cl)

(defun file-string (file)
  "Read the contents of a file and return as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun add-bracket (base-string)
  (concatenate 'string "(" base-string ")"))

(defun get-counter (max-count)
  (lexical-let ((count 0)
                (max-cnt max-count))
    (lambda ()
      (if (< count (- max-cnt 1))
          (progn
            (setq count (+ count 1))
            (- count 1))
        (progn (setq count 0)
               (- max-cnt 1))))))

;; TODO : text-type default value
(defun bizone/texts (&optional text-type)
  "Read the typing text by text-type parameter."
  (defun get-text-function (body default-call &optional call)
    (lexical-let ((bd body))
      (cond ((not call) (get-text-function body default-call default-call))
            ((eq call 'random)
             (lambda () (nth (random (list-length bd)) bd)))
             ((or (eq call 'seq) (eq call 'sequence))
              (lexical-let ((counter (get-counter (list-length body))))
                (lambda () (nth (funcall counter) bd))))
             (t
              (t (error "\"%s\" is not a valid call." call))))))
  
  (lexical-let* ((sentence-filename "bizone-typing-sentence.el")
                 (sentence-body (lambda () (car (read-from-string (add-bracket (file-string sentence-filename)))))))
    (cond ((not text-type)
           (bizone/texts 'sentence))
          ((eq text-type 'sentence)
           (lambda (&optional call)
             (get-text-function (funcall sentence-body) 'random call)))
          (t (error "\"%s\" is not a valid text-type." text-type)))))
           
(defun bizone/typing (&optional text-count text-type call)
  (interactive)
  (defun setup-buffer (buffer-name)
    (let* ((buffer (get-buffer-create buffer-name)))
      (other-window 1)
      (switch-to-buffer buffer)))
  (defun put-typing-text (text)
    (insert (format "Type this: \n\n%s\n" text)))
  (defun put-typing-speed-info (info)
    (if info
        (insert (format "speed : %s\n\n" info))
      nil))
  (defun put-average-typing-speed-info (info)
    (if info
        (insert (format "Average speed : %s\n" info))
      nil))
  (defun get-typing-speed-info ()
    "200 type/minute")
  (defun bizone/typing-iter (texts-function typing-speed-info text-count)
    (let* ((next-text (funcall texts-function)))
      (read-only-mode -1)
      (erase-buffer)
      (put-average-typing-speed-info typing-speed-info)
      (put-typing-speed-info typing-speed-info)
      (if (< text-count 0)
          (insert "Typing test is done.")
        (progn
          (put-typing-text next-text)
          ;; (let* ((typing-info (get-typing next-text))
          ;;        (typing-speed (car typing-info))
          ;;        (typing-etc (cdr typing-info)))
            (bizone/typing-iter texts-function (get-typing-speed-info) (- text-count 1)))
      (read-only-mode 1))))
  
  (let* ((typing-buffer-name "*bizone-typing*")
         (typing-buffer (setup-buffer typing-buffer-name))
         (texts-function (funcall (bizone/texts text-type) call)))
    (bizone/typing-iter texts-function nil text-count)))


;; (bizone/typing)
