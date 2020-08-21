;; ssh agent interop
;; -----------------
(defvar ss8ch-agent-socket-var "SSH_AUTH_SOCK")
(defvar ss8ch-agent-process-id "SSH_AGENT_PID")
(defvar ss8ch-agent-search-end "; export")

(defun ss8ch-find-var-value-in-agent-response
    (var-name response)
  "Takes a var-name and the response of calling `ssh-agent` in a
  shell environment. Finds the value for the given var-name in
  the given agent response."
  (let* ((start-idx (+ 1
                       (length var-name)
                       (string-match var-name response)))
         (end-idx (string-match ss8ch-agent-search-end
                                response
                                start-idx)))
    (substring response start-idx end-idx)))

(defun ss8ch-ensure-agent ()
  (when (not (getenv ss8ch-agent-process-id))
    (message "ss8ch-... starting new ssh-agent")
    (let ((agent-response (shell-command-to-string "ssh-agent")))
      (setenv ss8ch-agent-socket-var
              (ss8ch-find-var-value-in-agent-response
               ss8ch-agent-socket-var
               agent-response))
      (setenv ss8ch-agent-process-id
              (ss8ch-find-var-value-in-agent-response
               ss8ch-agent-process-id
               agent-response))
      (message "ssh agent started"))))

(defun ss8ch-handle-passphrase-request (process process-message)
  "Helper to handle passphrase requests from running processes.
   To be registered with set-process-filter."
  (save-match-data
    (if (string-match "passphrase.*:\\s *\\'" process-message)
        (process-send-string process
                             (concat (read-passwd process-message) "\n"))
      (message process-message))))

(defun ss8ch-add (key-file)
  "Checks if an agent is registered in the environment. If not
   so, an agent is started and registered. Then runs ssh-add to
   add a key to the running SSH agent. Emacs prompts for a
   password as long as the process requires input."
  (interactive (list (read-file-name "select ssh key: " "~/.ssh/")))
  (ss8ch-ensure-agent)
  (let (process)
    (unwind-protect
        (progn
          (setq process (start-process  "ssh-add" nil
                                        "ssh-add" (expand-file-name key-file)))
          (set-process-filter process 'ss8ch-handle-passphrase-request)
          (while (accept-process-output process)))
      (if (eq (process-status process) 'run)
          (kill-process process)))))
