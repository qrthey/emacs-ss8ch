;; ssh agent interop
;; -----------------
(defconst ss8ch-agent-socket-var "SSH_AUTH_SOCK")
(defconst ss8ch-agent-process-id "SSH_AGENT_PID")
(defconst ss8ch-agent-search-end "; export")

(defun ss8ch-find-var-value-in-agent-response
    (var-name response)
  "Takes a var-name and the response of calling `ssh-agent` in a
   shell environment. Finds the value for the given var-name in
   the given agent response."
  (save-match-data
    (if (string-match (concat var-name "=\\([^;]+\\)" ss8ch-agent-search-end)
                      response)
        (match-string 1 response))))

(defun ss8ch-ensure-agent ()
  "Checks if the environment contains the pid var for an ssh
   agent. If not so, starts an ssh-agent process and captures its
   output the configure the environment."
  (when (not (getenv ss8ch-agent-process-id))
    (let ((agent-response (shell-command-to-string "ssh-agent")))
      (setenv ss8ch-agent-socket-var
              (ss8ch-find-var-value-in-agent-response
               ss8ch-agent-socket-var
               agent-response))
      (setenv ss8ch-agent-process-id
              (ss8ch-find-var-value-in-agent-response
               ss8ch-agent-process-id
               agent-response)))
    (message "ss8ch ~ agent started")))

(defun ss8ch-handle-passphrase-request (process process-message)
  "Helper function to handle passphrase requests from the ssh-add
   process."
  (save-match-data
    (if (string-match "passphrase.*:\\s *\\'" process-message)
        (process-send-string process
                             (concat (read-passwd process-message) "\n"))
      (if (not (string-match "^\n+$" process-message))
          (message (concat "ss8ch ~ " (string-trim process-message)))))))

(defun find-private-ssh-keys-in (directory)
  "Returns a list of file paths under directory for private ssh
   keys."
  (remove nil (mapcar (lambda (file-name)
                    (save-match-data
                      (message file-name)
                      (if (string-match "^\\([^.]+\\)\\.pub$" file-name)
                          (concat directory (match-string 1 file-name)))))
                  (directory-files directory))))

(defun ss8ch-add (key-file)
  "Checks if an agent is registered in the environment. If not
   so, an agent is started and registered. Then runs ssh-add to
   add a key to the running SSH agent, using the minibuffer to
   ask for the keys passphrase."
  (interactive
   (list (completing-read "Select ssh key to add: "
                          (find-private-ssh-keys-in "~/.ssh/"))))
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
