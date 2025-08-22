(require 'json)
(require 'jsonrpc)
(require 'cl-lib)
(require 'url-util)

(defvar tabbymacs--connection nil
  "The JSONRPC connection to tabby-agent")

(defun tabbymacs--path-to-uri (path)
  "Convert PATH to a proper file:// URI with escaping."
  (concat "file://"
		  (url-hexify-string (expand-file-name (file-local-name path))
							 url-path-allowed-chars)))

(defun tabbymacs--basename (path)
  "Return the last component of PATH, without trailing slash."
  (file-name-nondirectory (directory-file-name path)))

(defun tabbymacs--make-connection()
  "Start a connection to tabby-agent via LSP (stdio)."
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
  (interactive)
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
						 :workspaceFolders [(:uri ,uri
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
					 (message "Connection with tabby-agent initialized successfully."))
	   :error-fn (lambda (err)
				   (message "tabby-agent init failed: %S" err))))))

(defun tabbymacs-disconnect ()
  "Shutdown the connection to tabby-agent."
  (interactive)
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

(defcustom tabbymacs-language-id-configuration
  '((c++-ts-mode . "cpp")
	(c++-mode . "cpp"))
  "Mapping of major modes to LSP language IDs.
You can extend this mapping for custom major modes."
  :type '(alist :key-type symbol :value-type string)
  :group 'tabbymacs)

(defvar-local tabbymacs--buffer-language-cache nil
  "Cached languageId for current buffer.")

(defun tabbymacs--buffer-language ()
  "Get language corresponding to current buffer."
  (or tabbymacs--buffer-language-cache
	  (setq tabbymacs--buffer-language-cache
			(or
			 (cdr (assoc major-mode tabbymacs-language-id-configuration))
			 (let ((name (symbol-name major-mode)))
			   (cond
				((string-suffix-p "-ts-mode" name)
				 (string-remove-suffix "-ts-mode" name))
				((string-suffix-p "-mode" name)
				 (string-remove-suffix "-mode" name))
				(t nil))))))
  (unless tabbymacs--buffer-language-cache
	(display-warning
	 'tabbymacs
	 (format "Unable to extract languageId for buffer `%s'. Major mode: %s"
			 (buffer-name) major-mode)
	 :warning))
  tabbymacs--buffer-language-cache)

(defun tabbymacs--buffer-uri ()
  "Return URI of the current buffer."
  (tabbymacs--path-to-uri buffer-file-name))

(defun tabbymacs--buffer-content ()
  "Get content of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defvar-local tabbymacs--current-buffer-version 0
  "The version number of document (it increases after each change).")

(defun tabbymacs--did-open ()
  "Send textDocument/didOpen for the current buffer."
  (when (and buffer-file-name tabbymacs--connection)
	(let* ((uri (tabbymacs--path-to-uri buffer-file-name))
		   (langId ))
	  (jsonrpc-notify
	   tabbymacs--connection
	   :textDocument/didOpen
	   `(:textDocument (:uri ,(tabbymacs--buffer-uri)
							 :languageId ,(tabbymacs--buffer-language)
							 :version ,tabbymacs--current-buffer-version
							 :text ,(tabbymacs--buffer-content)))))))

(defun tabbymacs--did-change (beg end len)
  "Send textDocument/didChange after buffer edits. Simplified."
  (when (and buffer-file-name tabbymacs--connection)
	(let ((uri (tabbymacs--path-to-uri buffer-file-name)))
	  (jsonrpc-notify
	   tabbymacs--connection
	   :textDocument/didChange
	   `(:textDocument (:uri ,uri :version tabbymacs--current-buffer-version)
					   :contentChages [(:text ,(buffer-substring-no-properties (point-min) (point-max)))])))))

(defun tabbymacs-complete-at-point ()
  "Request completions from tabby-agent. Simplified."
  (interactive)
  (when (and buffer-file-name tabbymacs--connection)
	(let* ((uri (tabbymacs--path-to-uri buffer-file-name))
		   (pos (list :line (1- (line-number-at-pos))
					  :character (current-column)))
		   (params `(:textDocument (:uri ,uri)
								   :position ,pos)))
	  (jsonrpc-async-request
	   tabbymacs--connection
	   :textDocument/completion params
	   :success-fn (lambda (res)
					 (if (and res (plist-get res :items))
						 (let* ((first (aref (plist-get res :items) 0))
								(insert-text (or (plist-get first :insertText)
												 (plist-get first :label))))
						   (when insert-text
							 (insert insert-text)
							 (message "tabby completion inserted: %s" insert-text)))
					   (message "No completions.")))
	   :error-fn (lambda (err)
				   (message "Completion request failed: %S" err))))))

(defun tabbymacs-enable-sync ()
  "Enable LSP document sync for the current buffer."
  (interactive)
  (add-hook 'after-change-functions #'tabbymacs--did-change nil t)
  (tabbymacs--did-open))

(provide 'tabbymacs)



;; Need to add timeout, tabby-agent start command as confifurable
;; correct language id extraction
