(require 'json)
(require 'jsonrpc)
(require 'cl-lib)
(require 'url-util)

(defvar tabbymacs--proc nil)
(defvar tabbymacs--connection nil
  "The JSONRPC connection to tabby-agent")

(defun tabbymacs--path-to-uri (path)
  "Convert PATH to a proper file:// URI with escaping."
  (concat "file://"
		  (url-hexify-string (expand-file-name (file-local-name path))
							 url-path-allowed-chars)))

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
												  :name ,(file-name-nondirectory
														  (directory-file-name root)))]
						 :capabilities
						 (:textDocument
						  (:completion
						   (:completionItem
							(:snippetSupport t)))))))
	  (jsonrpc-request tabbymacs--connection
					   :initialize init-params))
	(jsonrpc-notify tabbymacs--connection :initialized '(:dummy t))
	(message "Started tabby-agent connection.")))

(defun tabbymacs-disconnect ()
  "Shutdown the connection to tabby-agent." ;
  (interactive)
  (when (and tabbymacs--connection
			 (jsonrpc-running-p tabbymacs--connection))
	(jsonrpc-request tabbymacs--connection :shutdown nil)
	(jsonrpc-notify tabbymacs--connection :exit nil)
	(jsonrpc-shutdown tabbymacs--connection)
	(setq tabbymacs--connection nil)
	(message "Stopped tabby-agent connection.")))

(provide 'tabbymacs)

