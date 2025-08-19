(require 'json)
(require 'jsonrpc)
(require 'cl-lib)

(defvar tabbymacs--proc nil)
(defvar tabbymacs--connection nil)
(defvar tabbymacs--request-id 0)

(defun tabbymacs--initialize()
  "Send the LSP initialize request to the tabby-agent."
  (jsonrpc-request
   tabbymacs--connection
   "initialize"
   `((processId . ,(emacs-pid))
	 (rootUri . ,(concat "file://" default-directory))
	 (capabilities . ((textDocument . ((synchronization . ((didSave . t)))))))
	 (clientInfo . ((name . "tabbymacs")
				   (version . "0.1"))))))

(defun tabbymacs--initialized ()
  (jsonrpc-notify tabbymacs--connection "initialized" '()))

(defun tabbymacs-start ()
  "Start Tabbymacs LSP client."
  (interactive)
  (setq tabbymacs--proc
		(make-process
		 :name "tabby-agent"
		 :command '("tabby-agent" "--lsp" "--stdio")
		 :connection-type 'pipe
		 :coding 'utf-8
		 :stderr "*tabby-agent stderr*"
		 :noquery t))
  (setq tabbymacs--connection
		(jsonrpc-process-connection
		 :name "tabby-agent"
		 :process tabbymacs--proc
		 :request-dispatcher #'jsonrpc-default-request-dispatcher
		 :notification-dispatcher #'jsonrpc-default-notification-dispatcher
		 :on-shutdown (lambda (_conn) (message "Tabby server shut down"))))
  ;; Send initialize request after starting
  (message "Sending initialize request...")
  (tabbymacs--initialize)
  (tabbymcas--initialized))


(provide 'tabbymacs)

