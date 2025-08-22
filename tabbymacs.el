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

(defvar-local tabbymacs--buffer-languageId-cache nil
  "Cached languageId for current buffer.")

(defun tabbymacs--buffer-languageId ()
  "Get languageId corresponding to current buffer."
  (or tabbymacs--buffer-languageId-cache
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
  (unless tabbymacs--buffer-languageId-cache
	(display-warning
	 'tabbymacs
	 (format "Unable to extract languageId for buffer `%s'. Major mode: %s"
			 (buffer-name) major-mode)
	 :warning))
  tabbymacs--buffer-languageId-cache)

(defvar-local tabbymacs--TextDocumentIdentifier-cache nil
  "Cached LSP TextDocumentIdentifier for the current buffer.")

(defun tabbymacs--TextDocumentIdentifier ()
  "Return TextDocumentIdentifier object for the current buffer."
  (or tabbymacs--TextDocumentIdentifier-cache
	  (setq tabbymacs--TextDocumentIdentifier-cache
			(list :uri (tabbymacs--path-to-uri buffer-file-name)))))

(defvar-local tabbymacs--current-buffer-version 0
  "The version number of document (it increases after each change).")

(defun tabbymacs--VersionedTextDocumentIdentifier ()
  "Return VersionedTextDocumentIdentifier object for the current buffer.
Extends `tabbymacs-TextDocumentIdentifier` with :version."
  (append (tabbymacs--TextDocumentIdentifier)
		  (list :version tabbymacs--curent-buffer-version)))

(defun tabbymacs--buffer-content ()
  "Get content of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun tabbymacs--TextDocumentItem ()
  "Compute TextDocumentItem object for the current buffer.
This includes URI, version, languageId and current text."
  (append
   (tabbymacs--VersionedTextDocumentIdentifier)
   (list :languageId (tabbymacs--buffer-languageId)
		 :text (tabbymacs--buffer-content))))

(defun tabbymacs--did-open ()
  "Send textDocument/didOpen notification for the current buffer."
  (when (and buffer-file-name tabbymacs--connection)
	(setq tabbymacs--TextDocumentIdentifier-cache nil
		  tabbymacs--current-buffer-version 0)
	(jsonrpc-notify
	 tabbymacs--connection
	 :textDocument/didOpen
	 `(:textDocument ,(tabbymacs--TextDocumentItem)))
	(add-hook 'after-change-functions
			  #'tabbymacs--after-change-hook
			  nil t)))

(defun tabbymacs--after-change-hook (_beg _end _len)
  "Hook function to send didChange notifications after buffer edits."
  (when (and tabbymacs--connection buffer-file-name)
	(tabbymacs--did-change)))

(defun tabbymacs--did-change ()
  "Send textDocument/didChange after buffer edits. Simplified."
  (when (and buffer-file-name tabbymacs--connection)
	(cl-incf tabbymacs--current-buffer-version)
	(jsonrpc-notify
	 tabbymacs--connection
	 :textDocument/didChange
	 `(:textDocument ,(tabbymacs--VersionedTextDocumentIdentifier)
					 :contentChages [(:text ,(tabbymacs--buffer-content))]))))



(provide 'tabbymacs)

;; Need to add timeout, tabby-agent start command as confifurable
;; correct language id extraction
