;;; hbase-shell.el --- a plugin for leetcodeme
;;; Commentary:
;;; code:

(require 'ctable)
(require 'cl-lib)

(defvar hbase-shell-bin)

(setq hbase-shell--asyn-process-output "")
(make-local-variable 'hbase-shell--asyn-process-output)


(defun hbase-shell--output-to-local-buffer-variable (proc string)
  "Get Asyn process's output to hbase-shell--asyn-process-output variable"
  (when (buffer-live-p (process-buffer proc))
    (setq hbase-shell--asyn-process-output (concat hbase-shell--asyn-process-output (replace-regexp-in-string "\015" "" (ansi-color-apply string))))))

(defun hbase-shell--split-string-wtith-blank (str)
  "Split String with space, and remove all empty element"
  (remove "" (split-string str "\s")))

(defun hbase-shell--list-all-async (process signal)
  "Run hbase-shell Async and list resource"
  (when (memq (process-status process) '(exit))
    (let* ((output (remove nil 
                           (mapcar 'hbase-shell--split-string-wtith-blank
                                   (split-string (substring hbase-shell--asyn-process-output 
                                                            (string-match "hbase(main)" hbase-shell--asyn-process-output)
                                                            (string-match " seconds" hbase-shell--asyn-process-output)) "\n"))))
          (titles (nth 1 output))
          (data (cdr output))
          (the-buffer (process-buffer process)))
      (hbase-shell--show-ctable titles data the-buffer (process-command process) nil))))

(defun hbase-shell--list-all (buffer-name shell-command)
  "Run hbase-shell and list resource"
  (setq hbase-shell--asyn-process-output nil)
  (make-process :name buffer-name
                :buffer buffer-name
                :command shell-command
                :filter #'hbase-shell--output-to-local-buffer-variable
                :sentinel 'hbase-shell--list-all-async))


(defun hbase-shell--create-cmodel (title)
  "Create cmodel by title string"
  (make-ctbl:cmodel :title  title
                    :align 'left))



(defun hbase-shell--show-ctable (titles data buffer command keymap)
  "Create and show data in ctable"
  (with-current-buffer buffer 
    (setq buffer-read-only nil)
    (erase-buffer))
  (let* ((model ; data model
          (make-ctbl:model
           :column-model  (mapcar 'hbase-shell--create-cmodel titles)
           :data data))
         (component ; ctable component
          (ctbl:create-table-component-buffer
           :custom-map keymap
           :buffer buffer
           :model model)))
    (setq hbase-shell--asyn-process-output nil)
    (pop-to-buffer (ctbl:cp-get-buffer component))
    )
  (setq buffer-read-only t))


(defun run-hbase-shell (command)
  (let* ((process-name (concat "hbase-shell-" command))
         (process-buffer-name (concat "*hbase-shell-" command "*"))
         (process (start-process-shell-command process-name process-buffer-name (concat "echo \"" command "\" | " hbase-shell-bin " shell"))))
    (set-process-filter process 'hbase-shell--output-to-local-buffer-variable)
    (set-process-sentinel process 'hbase-shell--list-all-async)))


(defun hbase-shell-list-tables()
  "hbase-shell list tables"
  (interactive)
  (run-hbase-shell "list"))

(defun hbase-shell-scan-table (table-name)
  "hbase-shell list tables"
  (interactive "sHbase table name: ")
  (run-hbase-shell (concat "scan '" table-name "'")))


(defun combine-hbase-shell-command (command parameters)
  (let* ((quote-parameters (mapcar (lambda (parameter) (concat "'" parameter "'")) (remove "" (remove nil parameters))))
         (command (concat command " " (mapconcat 'identity quote-parameters ", "))))
    command))

(defun hbase-shell-put (table-name
                        rowkey
                        column
                        value
                        )
  "hbase-shell list tables"
  (interactive "sHbase table name: \nsHbase rowkey: \nsColumn: \nsColumn Value: ")
  (let* ((parameters (list table-name rowkey column value))
         (command (combine-hbase-shell-command "put" parameters)))
    (run-hbase-shell command)
    ))

(defun hbase-shell-delete (table-name
                        rowkey
                        column
                        &optional timestamp)
  "hbase-shell list tables"
  (interactive "sHbase table name: \nsHbase rowkey: \nsColumn: \ns Timestamp: ")
  (let* ((parameters (list table-name rowkey column timestamp))
         (command (combine-hbase-shell-command "delete" parameters)))
    (run-hbase-shell command)))

(provide 'hbase-shell)
;;; hbase-shell.el ends here
