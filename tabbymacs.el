;;; tabbymacs.el --- The Emacs Client for tabby-agent  -*- lexical-binding: t; -*-

(require 'json)
(require 'jsonrpc)
(require 'cl-lib)
(require 'url-util)

;; ------------------------------
;; Buffer-local caches & vars
;; ------------------------------

(defvar tabbymacs--connection nil
  "The JSONRPC connection to tabby-agent")

(defvar-local tabbymacs--TextDocumentIdentifier-cache nil
  "Cached LSP TextDocumentIdentifier for the current buffer.")

(defvar-local tabbymacs--current-buffer-version 0
  "The version number of document (it increases after each change).")

(defvar-local tabbymacs--buffer-languageId-cache nil
  "Cached LSP languageId for current buffer.")

(defvar-local tabbymacs--change-idle-timer nil
  "Idle timer for batching textDocument/didChange notifications.")

(defvar-local tabbymacs--recent-changes nil
  "List of pending buffer changes for textDocument/didChange or
the symbol `:emacs-messup' if Emacs gave inconsistent data.")

;; ------------------------------
;; User configuration
;; ------------------------------

(defcustom tabbymacs-language-id-configuration
  '((c++-ts-mode . "cpp")
	(c++-mode . "cpp"))
  "Mapping of major modes to LSP language IDs.
You can extend this mapping for custom major modes."
  :type '(alist :key-type symbol :value-type string)
  :group 'tabbymacs)

(defcustom tabbymacs-send-changes-idle-time 0.5
  "Don't send changes to tabby-agent before Emacs's been idle for this many seconds."
  :type 'number
  :group 'tabbymacs)

;; ------------------------------
;; Utility functions
;; ------------------------------

(defun tabbymacs--path-to-uri (path)
  "Convert PATH to a proper file:// URI with escaping."
  (concat "file://"
		  (url-hexify-string (expand-file-name (file-local-name path))
							 url-path-allowed-chars)))

(defun tabbymacs--basename (path)
  "Return the last component of PATH, without trailing slash."
  (file-name-nondirectory (directory-file-name path)))

(defun tabbymacs--buffer-languageId ()
  "Get languageId corresponding to current buffer."
  (or tabbymacs--buffer-languageId-cache
	  (setq tabbymacs--buffer-languageId-cache
			(or (cdr (assoc major-mode tabbymacs-language-id-configuration))
				(let ((name (symbol-name major-mode)))
				  (cond
				   ((string-suffix-p "-ts-mode" name)
					(string-remove-suffix "-ts-mode" name))
				   ((string-suffix-p "-mode" name)
					(string-remove-suffix "-mode" name))
				   (t nil))))))
  (unless tabbymacs--buffer-languageId-cache
	(display-warning
	 'tabbymacs
	 (format "Unable to extract languageId for buffer `%s'. Major mode: %s"
			 (buffer-name) major-mode)
	 :warning))
  tabbymacs--buffer-languageId-cache)

(defun tabbymacs--TextDocumentIdentifier ()
  "Return TextDocumentIdentifier object for the current buffer."
  (or tabbymacs--TextDocumentIdentifier-cache
	  (setq tabbymacs--TextDocumentIdentifier-cache
			(list :uri (tabbymacs--path-to-uri buffer-file-name)))))

(defun tabbymacs--VersionedTextDocumentIdentifier ()
  "Return VersionedTextDocumentIdentifier object for the current buffer."
  (append (tabbymacs--TextDocumentIdentifier)
		  (list :version tabbymacs--current-buffer-version)))

(defun tabbymacs--buffer-content ()
  "Return content of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun tabbymacs--TextDocumentItem ()
  "Return TextDocumentItem object for the current buffer."
  (append (tabbymacs--VersionedTextDocumentIdentifier)
		  (list :languageId (tabbymacs--buffer-languageId)
				:text (tabbymacs--buffer-content))))

(defun tabbymacs--pos-to-lsp-position (pos)
  "Convert buffer position POS to an LSP position."
  (save-excursion
	(goto-char pos)
	(list :line (1- (line-number-at-pos))
		  :character (- (point) (line-beginning-position)))))

(defun tabbymacs--reset-vars ()
  "Reset buffer local caches and vars."
  (setq tabbymacs--recent-changes nil
		tabbymacs--current-buffer-version 0
		tabbymacs--change-idle-timer nil
		tabbymacs--TextDocumentIdentifier-cache nil
		tabbymacs--buffer-languageId-cache nil))

;; ------------------------------
;; JSONRPC Connection
;; ------------------------------

(defun tabbymacs--make-connection()
  "Start a JSONRPC connection to tabby-agent."
  (setq tabbymacs--connection
		(make-instance
		 'jsonrpc-process-connection
		 :name "tabby-agent"
		 :process (lambda ()
					(make-process
					 :name "tabby-agent"
					 :command '("tabby-agent" "--stdio")
					 :connection-type 'pipe
					 :coding 'utf-8-emacs-unix
					 :stderr "*tabby-agent-stderr*"))
		 :request-dispatcher
		 (lambda (_conn method params)
		   (message "[tabby-agent] Request from server: %s %S" method params)
		   nil)
		 :notification-dispatcher
		 (lambda (_conn method params)
		   (message "[tabby-agent] Notification: %s %S" method params)))))

(defun tabbymacs-connect ()
  "Connect to tabby-agent if not already connected."
  (unless (and tabbymacs--connection
			   (jsonrpc-running-p tabbymacs--connection))
	(tabbymacs--make-connection)
	(let* ((root (expand-file-name default-directory))
		   (uri (tabbymacs--path-to-uri root))
		   (init-params
			`(:processId ,(emacs-pid)
						 :clientInfo (:name "emacs" :version ,(emacs-version))
						 :rootPath ,(file-local-name root)
						 :rootUri ,uri
						 :workspaceFolders
						 [(:uri ,uri
								:name ,(tabbymacs--basename root))]
						 :capabilities
						 (:textDocument
						  (:inlineCompletion
						   (:dynamicRegistration :json-false))))))
	  (jsonrpc-async-request
	   tabbymacs--connection
	   :initialize init-params
	   :success-fn (lambda (_result)
					 (jsonrpc-notify
					  tabbymacs--connection
					  :initialized nil)
					 (message "Tabby-agent initialized."))
	   :error-fn (lambda (err)
				   (message "Tabby-agent init failed: %S" err))))))

(defun tabbymacs-disconnect ()
  "Shutdown the connection to tabby-agent."
  (when (and tabbymacs--connection
			 (jsonrpc-running-p tabbymacs--connection))
	(jsonrpc-async-request
	 tabbymacs--connection
	 :shutdown nil
	 :success-fn (lambda (_result)
				   (jsonrpc-notify
					tabbymacs--connection
					:exit nil)
				   (jsonrpc-shutdown
					tabbymacs--connection)
				   (setq tabbymacs--connection nil)
				   (message "Stopped tabby-agent connection."))
	 :error-fn (lambda (err)
				 (message "tabby-agent shutdown failed: %S" err)))))

;; ------------------------------
;; Buffer notifications
;; ------------------------------

(defun tabbymacs--did-open ()
  "Send textDocument/didOpen notification for the current buffer."
  (when (and buffer-file-name tabbymacs--connection)
    (jsonrpc-notify
	 tabbymacs--connection
	 :textDocument/didOpen
	 `(:textDocument ,(tabbymacs--TextDocumentItem)))))

(defun tabbymacs--did-close ()
  "Send textDocument/didClose notification for the current buffer."
  (when (and buffer-file-name tabbymacs--connection)
	(jsonrpc-notify
	 tabbymacs--connection
	 :textDocument/didClose
	 `(:textDocument ,(tabbymacs--TextDocumentIdentifier)))))

(defun tabbymacs--did-change ()
  "Send textDocument/didChange notification, batching changes."
  (when (and tabbymacs--recent-changes tabbymacs--connection buffer-file-name)
	(let* ((full-sync-p (eq tabbymacs--recent-changes :emacs-messup))
		   (changes
			(if full-sync-p
				(vector `(:text ,(tabbymacs--buffer-content)))
			  (cl-loop for (beg end len text) in (reverse tabbymacs--recent-changes)
					   vconcat `[,(list :range `(:start ,beg :end ,end)
										:rangeLength len
										:text text)]))))
	  (jsonrpc-notify
	   tabbymacs--connection
	   :textDocument/didChange
	   (list :textDocument (tabbymacs--VersionedTextDocumentIdentifier)
			 :contentChanges changes)))
	  (setq tabbymacs--recent-changes nil
			tabbymacs--change-idle-timer nil)))

;; ------------------------------
;; Hooks
;; ------------------------------

(defun tabbymacs--before-change (beg end)
  "Record BEG and END before a buffer change."
  (when (listp tabbymacs--recent-changes)
	(push `(,(tabbymacs--pos-to-lsp-position beg)
			,(tabbymacs--pos-to-lsp-position end)
			(,beg . ,(copy-marker beg nil)) ;; non-inserting marker
			(,end . ,(copy-marker end t))) ;; inserting marker
		  tabbymacs--recent-changes)))

(defun tabbymacs--after-change (beg end pre-change-length)
  "Record buffer state after a change."
  (cl-incf tabbymacs--current-buffer-version)
  (pcase (car-safe tabbymacs--recent-changes)
	(`(,lsp-beg, lsp-end
				 (,b-beg . ,b-beg-marker)
				 (,b-end . ,b-end-marker))
	 (if (and (= b-end b-end-marker) (= b-beg b-beg-marker)
			  (or (/= beg b-beg) (/= end b-end)))
		 (setcar tabbymacs--recent-changes
				 `(,lsp-beg ,lsp-end ,(- b-end-marker b-beg-marker)
							,(buffer-substring-no-properties b-beg-marker
															 b-end-marker)))
	   (setcar tabbymacs--recent-changes
			   `(,lsp-beg ,lsp-end ,pre-change-length
						 ,(buffer-substring-no-properties beg end)))))
	(_ (setq tabbymacs--recent-changes :emacs-messup)))
  (when tabbymacs--change-idle-timer
	(cancel-timer tabbymacs--change-idle-timer))
  (let ((buf (current-buffer)))
	(setq tabbymacs--change-idle-timer
		  (run-with-idle-timer
		   tabbymacs-send-changes-idle-time
		   nil (lambda ()
				 (when (buffer-live-p buf)
				   (with-current-buffer buf
					 (tabbymacs--did-change))))))))

(defun tabbymacs--enable-change-hooks ()
  "Enable before and after change hooks for LSP didChange tracking."
  (add-hook 'before-change-functions #'tabbymacs--before-change nil t)
  (add-hook 'after-change-functions #'tabbymacs--after-change nil t))

(defun tabbymacs--disable-change-hooks ()
  "Disable before and after change hooks."
  (remove-hook 'before-change-functions #'tabbymacs--before-change t)
  (remove-hook 'after-change-functions #'tabbymacs--after-change t))

;; ------------------------------
;; Minor mode
;; ------------------------------

;;;###autoload
(define-minor-mode tabbymacs-mode
  "Minor mode for Tabby LSP inline completion."
  :lighter " Tabbymacs"
  :group 'tabbymacs
  :keymap nil
  (if tabbymacs-mode
	  (progn
		(tabbymacs-connect)
		(tabbymacs--reset-vars)
		(tabbymacs--did-open)
		(tabbymacs--enable-change-hooks))
	(progn
	  (when tabbymacs--change-idle-timer
		(cancel-timer tabbymacs--change-idle-timer))
	  (when tabbymacs--recent-changes
		(tabbymacs--did-change))
	  (tabbymacs--disable-change-hooks)
	  (tabbymacs--did-close)
	  (tabbymacs--reset-vars))
	(unless (cl-some (lambda (buf)
					   (buffer-local-value 'tabbymacs-mode buf))
					 (buffer-list))
	  (tabbymacs-disconnect))))

(provide 'tabbymacs)

