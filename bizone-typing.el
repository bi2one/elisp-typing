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
(defun bizone/texts (text-type)
  "Read the typing text by text-type parameter."
  ;; function define area ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun get-text-function (body call)
    (lexical-let ((bd body))
      (cond ((eq call 'random)
             (lambda () (nth (random (list-length bd)) bd)))
             ((eq call 'seq)
              (lexical-let ((counter (get-counter (list-length body))))
                (lambda () (nth (funcall counter) bd)))))))
  ;; body area ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (lexical-let* ((sentence-filename "bizone-typing-sentence.el")
                 (sentence-body (lambda () (car (read-from-string (add-bracket (file-string sentence-filename)))))))
    (cond ((eq text-type 'sentence)
           (lambda (call) (get-text-function (funcall sentence-body) call))))))

(defun bizone/typing code
  (interactive)
  (defun init-buffer (buffer-name)
    (let* ((buffer (get-buffer-create buffer-name)))
      (other-window 1)
      (switch-to-buffer buffer)
      (erase-buffer)))
  (let* ((typing-buffer-name "*bizone-typing*")
         (typing-buffer (init-buffer typing-buffer-name)))
    typing-buffer))

(bizone/typing)
