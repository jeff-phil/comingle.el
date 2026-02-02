;;; comingle.el --- (Co)mingling of Codeium for Emacs with Overlays   -*- lexical-binding: t; -*-

;; MIT License

;; (Co)mingling Overlay additions: Copyright (c) 2025 jeff-phil
;;
;; Original: Copyright (c) 2023 Exafunction

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; use M-x `comingle-install' to install binaries automatically
;; add `comingle-completion-at-point' to your `completion-at-point-functions'
;; use `comingle-diagnose' to see currently enabled apis and fields

;; anything defined by `comingle-def' a constant or a function that
;; takes 1, 2, or 3 arguments, which are (api state val)
;; api is a symbol such as 'GetCompletions, state is of type `comingle-state'
;; which keeps all the state (including a process, port, and some hash tables)
;; val is only relevant if the field is only sensible given a previous
;; value, such as `comingle/request_id' used in `'CancelRequest'

;; use M-x `customize' see a full list of settings.

;;; Code:

(defvar comingle-latest-local-server-version "1.46.3")

;; (require 'url-parse)
(autoload 'url-parse-make-urlobj "url-parse")

(eval-when-compile
  (require 'url-vars)
  (defvar url-http-end-of-headers)
  (defvar url-http-response-status))

(defgroup comingle nil
  "comingle.el customization -some-doc-str-here-"
  :group 'convenience)
(defvar comingle-log-waiting-text (propertize "waiting for response" 'face '(:weight ultra-bold)))

(defvar comingle-fullpath-alist nil)

(eval-and-compile
  (defun comingle-default-func-name (symbol-name)
    (if (string-prefix-p "comingle" symbol-name)
        (concat "comingle-default" (substring symbol-name (length "comingle")))
      (error "invalid name"))))

(eval-and-compile
  (defun comingle-def-handle-args (args)
    (let ((doc-str nil) (value nil) (arglist nil) (body nil))
      (ignore doc-str value arglist body)
      (pcase-let*
          (
           (
            (or
             `(,value)
             `(,value ,(and (pred stringp) doc-str))
             `(,arglist ,(and (pred stringp) doc-str) . ,body)
             `(,arglist . ,body))
            args))
        (list doc-str value arglist body)))))

(defmacro comingle-def (name &rest args)
  (declare (doc-string 3))
  (pcase-let*
      (
       (`(,doc-str ,value ,arglist ,body) (comingle-def-handle-args args))
       (funcsymbol (when body (intern (comingle-default-func-name (symbol-name name)))))
       (value (or value `',funcsymbol))

       (fullpath
        (when (string-prefix-p "comingle/" (symbol-name name))
          (mapcar #'intern (cdr (split-string (symbol-name name) "/")))))
       (funcdefform (when body `((defun ,funcsymbol ,arglist ,@body))))

       (doc-str (or doc-str ""))
       )
    `(progn
       (setf (alist-get ',name comingle-fullpath-alist) ',fullpath)
       (defcustom ,name ,value
         ;; i can probably process the doc-str
         ,doc-str
         :type 'sexp
         :group 'comingle)
       ,@funcdefform)))

(comingle-def
 comingle-delay
 0.100
 "General comingle timer delay, NOT related to `comingle-auto-complete-delay' or `comingle-auto-complete'.

More used internally for control flow.")

(comingle-def
 comingle-auto-complete
 t
 "Completion automatically shows after `comingle-auto-complete-delay' if t.")

(comingle-def
 comingle-auto-complete-delay
 0.500
 "Delay in seconds before the comingle auto completion is displayed, if
`comingle-auto-complete' is t.")

(comingle-def comingle-directory (_api state) (comingle-state-manager-directory state))
(comingle-def comingle-port (_api state) (comingle-state-port state))

(defun comingle-get-language-server-string ()
  (let ((arch
         (unless (eq system-type 'windows-nt)
           (if (string= (string-trim (shell-command-to-string "uname -m")) "x86_64")
               "x64" "arm"))))
    (pcase system-type
      ('windows-nt "language_server_windows_x64.exe")
      ('gnu/linux (concat "language_server_linux_" arch))
      ('darwin (concat "language_server_macos_" arch))
      (_ (error "unable to automatically determine your system, or your system is not supported yet. Please file an issue on github.")))))

(comingle-def comingle-local-server-version comingle-latest-local-server-version)

(comingle-def comingle-download-url
              (condition-case err;; don't signal error on loading
                  (concat "https://github.com/Exafunction/codeium/releases/download/language-server-v"
                          comingle-local-server-version "/" (comingle-get-language-server-string) ".gz")
                (error
                 (defvar comingle-download-url (lambda () (signal (car err) (cdr err))))
                 nil)))

(defconst comingle-apis
  '(GetCompletions AddTrackedWorkspace GetProcesses Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))

(comingle-def comingle-api-enabled () t)

(comingle-def comingle-fields-regexps
              `(
                (GetCompletions .
                                ,(rx bol "comingle/" (or "metadata" "document" "editor_options") "/" (* anychar) eol))
                (GetProcesses .
                              ,(rx bol "comingle/metadata/" (* anychar) eol))
                (Heartbeat .
                           ,(rx bol "comingle/metadata/" (* anychar) eol))
                (CancelRequest .
                               ,(rx bol "comingle/" (or (seq "metadata/" (* anychar)) "request_id")  eol))
                (GetAuthToken)
                (AddTrackedWorkspace)
                (RegisterUser .
                              ,(rx bol "comingle/firebase_id_token" eol))
                (AcceptCompletion .
                                  ,(rx bol "comingle/" (or (seq "metadata/" (* anychar)) "completion_id")  eol))
                ))
(comingle-def comingle-api-fields (api)
              (let ((regexp (alist-get api comingle-fields-regexps)))
                (if (stringp regexp)
                    (remq nil
                          (mapcar
                           (lambda (el)
                             (when (string-match regexp (symbol-name (car el)))
                               (car el)))
                           comingle-fullpath-alist))
                  nil)))

(defvar comingle-special-url-alist '((auth-redirect . "/auth")))
(comingle-def comingle-url (api state)
              (let ((endpoint
                     (or (alist-get api comingle-special-url-alist)
                         (concat "/exa.language_server_pb.LanguageServerService/" (symbol-name api)))))
                (url-parse-make-urlobj "http" nil nil "127.0.0.1" (comingle-state-port state)
                                       endpoint nil nil t)))


(comingle-def comingle/metadata/ide_name "emacs")
(comingle-def comingle/metadata/extension_version comingle-local-server-version)
(comingle-def comingle/metadata/ide_version emacs-version)
;; (comingle-def comingle/metadata/request_id (api)
;; 	(when (eq api 'GetCompletions)
;; 		(random most-positive-fixnum)))

(defvar comingle-global-requestid-counter 0)
(comingle-def comingle/metadata/request_id (api)
              (when (eq api 'GetCompletions)
                (cl-incf comingle-global-requestid-counter)))

;; for CancelRequest
(comingle-def comingle/request_id (_api _state val) val)

;; for AcceptCompletion
(comingle-def comingle/completion_id (_api _state val) val)

(defun comingle-get-saved-api-key ()
  "Retrieve the saved API key for codeium.com from auth-source."
  ;; Ensure the auth-source library is loaded
  (require 'auth-source)
  (auth-source-pick-first-password :host "codeium.com" :user "apikey"))

(comingle-def comingle/metadata/api_key (_api state)
              (if-let ((api-key (let ((key (or (comingle-state-last-api-key state)
                                               (comingle-get-saved-api-key))))
                                  (when (and key (> (length key) 0)) key))))
                  (setq comingle/metadata/api_key api-key)
                (setq comingle/metadata/api_key
                      (lambda (_api state)
                        (when-let* ((api-key (comingle-state-last-api-key state)))
                          (setq comingle/metadata/api_key api-key))))
                nil))

(comingle-def comingle/document/text ()
              (buffer-string))
(comingle-def comingle/document/cursor_offset ()
              (comingle-utf8-byte-length (buffer-substring-no-properties (point-min) (point))))

(comingle-def comingle/document/editor_language () (symbol-name major-mode))

(defvar comingle-language-alist
  '(
    (nil . 0)
    (c-mode . 1)
    (c-ts-mode . 1)
    (clojure-mode . 2)
    (clojurec-mode . 2)
    (clojurescript-mode . 2)
    (coffee-mode . 3)
    (cc-mode . 4)
    (c++-mode . 4)
    (c++-ts-mode . 4)
    (csharp-mode . 5)
    (csharp-tree-sitter-mode . 5)
    (csharp-ts-mode . 5)
    (css-mode . 6)
    (css-ts-mode . 6)
    (cuda-mode . 7)
    (dockerfile-mode . 8)
    (dockerfile-ts-mode . 8)
    (go-dot-mod-mode . 9)
    (go-mod-ts-mode . 9)
    (go-mode . 9)
    (go-ts-mode . 9)
    (groovy-mode . 10)
    (haskell-mode . 12)
    (terraform-mode . 13)
    (html-mode . 14)
    (sgml-mode . 14)
    (mhtml-mode . 14)
    (java-mode . 16)
    (java-ts-mode . 16)
    (jdee-mode . 16)
    (ecmascript-mode . 17)
    (javascript-mode . 17)
    (js-mode . 17)
    (js2-mode . 17)
    (js-ts-mode . 17)
    (rjsx-mode . 17)
    (json-mode . 18)
    (json-ts-mode . 18)
    (julia-mode . 19)
    (ess-julia-mode . 19)
    (kotlin-mode . 20)
    (kotlin-ts-mode . 20)
    (latex-mode . 21)
    (less-mode . 22)
    (less-css-mode . 22)
    (lua-mode . 23)
    (lsp--render-markdown . 25)
    (markdown-mode . 25)
    (gfm-mode . 25)
    (objc-mode . 26)
    (perl-mode . 28)
    (cperl-mode . 28)
    (php-mode . 29)
    (php-ts-mode . 29)
    (text-mode . 30)
    (python-mode . 33)
    (python-ts-mode . 33)
    (cython-mode . 33)
    (ess-r-mode . 34)
    (ruby-mode . 35)
    (enh-ruby-mode . 35)
    (ruby-ts-mode . 35)
    (rust-mode . 36)
    (rust-ts-mode . 36)
    (rustic-mode . 36)
    (sass-mode . 37)
    (ssass-mode . 37)
    (scala-mode . 38)
    (scss-mode . 39)
    (sh-mode . 40)
    (ebuild-mode . 40)
    (pkgbuild-mode . 40)
    (sql-mode . 41)
    (swift-mode . 43)
    (tsx-mode . 44)
    (tsx-ts-mode . 44)
    (ts-mode . 45)
    (typescript-mode . 45)
    (typescript-ts-mode . 45)
    (nxml-mode . 48)
    (xml-mode . 48)
    (yaml-mode . 50)
    (yaml-ts-mode . 50)
    (svelte-mode . 51)
    (svelte-ts-mode . 51)
    (conf-toml-mode . 52)
    (toml-ts-mode . 52)
    (dart-mode . 53)
    (caml-mode . 55)
    (tuareg-mode . 55)
    (cmake-mode . 56)
    (cmake-ts-mode . 56)
    (pascal-mode . 57)
    (elixir-mode . 58)
    (elixir-ts-mode . 58)
    (heex-ts-mode . 58)
    (fsharp-mode . 59)
    (lisp-mode . 60)
    (lisp-data-mode . 60)
    (emacs-lisp-mode . 71)))

(comingle-def comingle/document/language ()
              (let ((mode major-mode))
                (while (not (alist-get mode comingle-language-alist))
                  (setq mode (get mode 'derived-mode-parent)))
                (alist-get mode comingle-language-alist)))

(comingle-def comingle/document/line_ending "\n"
              "according to https://www.reddit.com/r/emacs/comments/5b7o9r/elisp_how_to_concat_newline_into_string_regarding/
    this can be always \\n")

(comingle-def comingle/document/absolute_path_migrate_me_to_uri ()
              (or buffer-file-name (expand-file-name (buffer-name))))

(comingle-def comingle/editor_options/tab_size ()
              tab-width)
(comingle-def comingle/editor_options/insert_spaces ()
              (if indent-tabs-mode :false t))

(comingle-def comingle/firebase_id_token (_api state) (comingle-state-last-auth-token state))

;;;###autoload
(cl-defstruct
    (comingle-state
     (:constructor comingle-state-make)
     (:copier nil))
  (name "")
  (config nil
          :documentation "state-wise config, access it with `comingle-config'")
  (proc nil
        :documentation "created on a `comingle-init', not created if one specifies `comingle-port'")
  (manager-directory nil
                     :documentation "directory which comingle local language server places temp files; created by `comingle-default-command'")
  (port nil
        :documentation "port used by comingle local language server; by default a random port is used.
If you set `comingle-port', it will be used instead and no process will be created")
  (port-ready-hook nil
                   :documentation "hook called when the server is ready; use `comingle-on-port-ready' to add to it")

  (alive-tracker nil
                 :documentation "a symbol, set to nil on a comingle-reset which ensures that requests on timers made before the request are dropped")

  last-auth-token
  last-api-key

  chat-client-port
  chat-webserver-port

  (last-request-id 0)

  ;; hash tables for comingle-request-synchronously
  ;; these has distinct elements
  (results-table (make-hash-table :test 'eql :weakness nil)) ; results that are ready
  (pending-table (make-hash-table :test 'eql :weakness nil)) ; requestid that we are waiting for
  (completion-timer nil :documentation "Holds the active debounce timer"))

(comingle-def comingle-command-executable
              (expand-file-name
               (pcase system-type
                 ('windows-nt "codeium_language_server.exe")
                 (_ "codeium_language_server"))
               (expand-file-name "codeium" user-emacs-directory)))

(comingle-def comingle-chat t)
(comingle-def comingle-enterprise nil)
(comingle-def comingle-portal-url "https://www.codeium.com")
(comingle-def comingle-api-url "https://server.codeium.com")
(comingle-def comingle-register-user-url ()
              (if comingle-enterprise
                  (concat comingle-api-url "/exa.seat_management_pb.SeatManagementService/RegisterUser")
                "https://api.codeium.com/register_user/"))

(comingle-def comingle-command (api state)
              (unless (comingle-state-manager-directory state)
                (setf (comingle-state-manager-directory state) (make-temp-file "comingle_" t)))
              `(,(comingle-get-config 'comingle-command-executable api state)
                "--api_server_url" ,(comingle-get-config 'comingle-api-url api state)
                "--manager_dir" ,(comingle-state-manager-directory state)
                "--register_user_url" ,(comingle-get-config 'comingle-register-user-url api state)
                ,@(if (comingle-get-config 'comingle-enterprise api state) '("--enterprise_mode"))
                ,@(if (comingle-get-config 'comingle-chat api state)
                      '("--enable_local_search" "--enable_index_service"
                        "--search_max_workspace_file_count" "5000"
                        "--enable_chat_web_server" "--enable_chat_client"))
                "--portal_url" ,(comingle-get-config 'comingle-portal-url api state)))

(defvar comingle-state (comingle-state-make :name "default"))

;;;###autoload
(defun comingle-config (field &optional state)
  (setq state (or state comingle-state))
  (if (eq (alist-get field (comingle-state-config state) 'noexist) 'noexist)
      (symbol-value field)
    (alist-get field (comingle-state-config state))))
(defun comingle--set-config (val field &optional state)
  (setq state (or state comingle-state))
  (setf (alist-get field (comingle-state-config state)) val))

;;;###autoload
(gv-define-setter comingle-config (val field &optional state)
  `(comingle--set-config ,val ,field ,state))


(defun comingle-get-config (field api state &optional given-val)
  (let ((val (comingle-config field state)))
    (if (functionp val)
        (cl-case (cdr (func-arity val))
          (0 (funcall val))
          (1 (funcall val api))
          (2 (funcall val api state))
          (t (funcall val api state given-val)))
      val)))

(defun comingle-nested-alist-get-multi (body top &rest rest)
  (if rest
      (apply #'comingle-nested-alist-get-multi (alist-get top body) rest)
    (alist-get top body)))
(defun comingle-nested-alist-set-multi (body val top &rest rest)
  (let ((cur-alist body))
    (setf (alist-get top cur-alist)
          (if rest
              (apply #'comingle-nested-alist-set-multi (alist-get top cur-alist) val rest)
            val))
    cur-alist))
(defun comingle-nested-alist-get (body field)
  (let ((fullpath (alist-get field comingle-fullpath-alist)))
    (unless fullpath (error "field %s is set to path %s which is not valid" field fullpath))
    (apply #'comingle-nested-alist-get-multi body fullpath)))
(defun comingle--nested-alist-set (body field val)
  (let ((fullpath (alist-get field comingle-fullpath-alist)))
    (unless fullpath (error "field %s is set to path %s which is not valid" field fullpath))
    (apply #'comingle-nested-alist-set-multi body val fullpath)))
(gv-define-expander comingle-nested-alist-get
  (lambda (do body field)
    (gv-letplace (getter setter) body
      (macroexp-let2 nil field field
        (funcall do `(comingle-nested-alist-get ,getter ,field)
                 (lambda (v)
                   (macroexp-let2 nil v v
                     `(progn
                        ,(funcall setter`(comingle--nested-alist-set ,getter ,field ,v))
                        ,v))))))))


(defun comingle-compute-configs (api state vals-alist)
  (let (ans)
    (mapc
     (lambda (field)
       (setf (alist-get field ans) (comingle-get-config field api state (alist-get field vals-alist))))
     (comingle-get-config 'comingle-api-fields api state))
    ans))

(defun comingle-diagnose (&optional state)
  (interactive)
  (setq state (or state comingle-state))
  (with-output-to-temp-buffer "*comingle-diagnose*"

    (with-current-buffer standard-output
      (insert "comingle state: ")
      (insert (propertize (comingle-state-name state) 'face '(:weight ultra-bold)))
      (terpri)
      (insert "command: ")
      (let ((command
             (if (comingle-state-proc state)
                 (process-command (comingle-state-proc state))
               (insert "[will be]")
               (comingle-get-config 'comingle-command nil state))))
        (terpri)
        (insert
         (propertize (mapconcat #'shell-quote-argument command " ")
                     'face '(:weight ultra-bold)))
        (terpri)))
    (terpri)
    (mapc
     (lambda (api)
       (if (not (comingle-get-config 'comingle-api-enabled api state))
           (progn
             (with-current-buffer standard-output
               (insert (propertize (symbol-name api) 'face '(:weight ultra-bold :strike-through t))))
             (terpri)
             (terpri))
         (with-current-buffer standard-output
           (insert (propertize (symbol-name api) 'face '(:weight ultra-bold))))
         (terpri)
         (princ (url-recreate-url (comingle-get-config 'comingle-url api state)))
         (terpri)
         (mapc
          (lambda (item)
            ;; help-insert-xref-button
            (with-current-buffer standard-output
              (help-insert-xref-button (symbol-name (car item)) 'help-variable-def (car item))
              (insert (propertize "\t" 'display '(space :align-to 40))))
            (let*
                (
                 (print-escape-newlines t) (print-length 100)
                 (obj (cdr item))
                 (obj (if (stringp obj)
                          (substring-no-properties obj 0 (length obj)) obj)))
              (cl-prin1 obj))
            (terpri))
          (comingle-compute-configs api state nil))
         (terpri)))
     comingle-apis)))


(defun comingle-make-body-for-api (api state vals-alist)
  (let (body tmp)
    (mapc
     (lambda (field)
       (setq tmp (comingle-get-config field api state (alist-get field vals-alist)))
       (when tmp
         (setf (comingle-nested-alist-get body field) tmp)))
     (comingle-get-config 'comingle-api-fields api state))
    body))

(comingle-def comingle-log-buffer ()
              (let ((buf (get-buffer "*comingle-log*")))
                (if buf buf
                  (setq buf (generate-new-buffer "*comingle-log*"))
                  (with-current-buffer buf
                    (special-mode)
                    (buffer-disable-undo))
                  buf)))

(comingle-def comingle-mode-line-enable nil)
(comingle-def comingle-mode-line-keep-time 3)


;; https://nullprogram.com/blog/2010/05/11/
;; ID: 90aebf38-b33a-314b-1198-c9bffea2f2a2
(defun comingle-uuid-create ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (recent-keys)))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defvar comingle-last-auth-url nil)

(defvar comingle-overlay nil
  "The active comingle completion overlay.")
(defvar comingle-keymap-overlay nil
  "Keymap completions for comingle overlay.")
(defvar comingle-current-completions nil
  "A list of the current completion strings.")
(defvar comingle-current-completion-metadata nil
  "A list of metadata for the current completions.")
(defvar comingle-current-completion-index 0
  "The index of the currently displayed completion in `comingle-current-completions`.")
(defvar comingle-completion-info nil
  "Info about the current completion context (range, etc.).")

(defface comingle-ghost-text-face
  '((t :foreground "gray50"))
  "Face for comingle ghost text.")

(defun comingle-make-auth-url (state &optional uuid manual)
  (let*
      (
       (uuid (or uuid (comingle-uuid-create)))
       (query-params
        (url-build-query-string
         `(
           ("response_type" "token")
           ("state" ,uuid)
           ("scope" "openid profile email")
           ("redirect_uri"
            ,(if (eq manual 'manual) "vim-show-auth-token"
               (url-recreate-url (comingle-get-config 'comingle-url 'auth-redirect state))))
           ("redirect_parameters_type" "query"))))
       (url
        (concat (comingle-get-config 'comingle-portal-url 'auth-redirect state) "/profile?" query-params)))
    (setq comingle-last-auth-url url)))

(defun comingle-kill-last-auth-url ()
  (interactive)
  (when comingle-last-auth-url
    (message "%s sent to kill-ring" comingle-last-auth-url)
    (kill-new comingle-last-auth-url)))

(defun comingle-defer-until-no-input (state tracker func &optional args)
  (when (eq tracker (comingle-state-alive-tracker state))
    (if (input-pending-p)
        (run-with-idle-timer 0.005 nil #'comingle-defer-until-no-input state tracker func args)
      (with-local-quit
        (apply func args)))))

(defun comingle-run-with-timer (state secs func &rest args)
  (unless (comingle-state-alive-tracker state)
    (error "comingle-state is not alive! %s" state))
  (unless (numberp secs)
    (if (eq secs 'default)
        (setq secs (comingle-get-config 'comingle-delay nil state))))
  (let ((old-timer (comingle-state-completion-timer state)))
    (when (timerp old-timer)
      (cancel-timer old-timer)))
  (let ((new-timer
         (run-with-timer secs nil #'comingle-defer-until-no-input
                         state (comingle-state-alive-tracker state) func args)))
    (setf (comingle-state-completion-timer state) new-timer)))

(defun comingle-time-from (start-time)
  (float-time (time-subtract (current-time) start-time)))


;;;###autoload
(defun comingle-install (&optional state noconfirm)
  (interactive)
  (setq state (or state comingle-state))
  (when (comingle-state-alive-tracker state)
    (unless (yes-or-no-p "comingle is already running! are you sure to comingle-install? ") (user-error "aborted")))
  (setf (comingle-state-alive-tracker state)
        (gensym (comingle-state-name comingle-state)))
  (let*
      (
       (filename (comingle-get-config 'comingle-command-executable nil state))
       (url (comingle-get-config 'comingle-download-url nil state)))
    (when (file-exists-p filename)
      (unless (yes-or-no-p (format "%s already exists; overwrite? " filename)) (user-error "aborted")))
    (unless
        (or noconfirm
            (yes-or-no-p
             (format "you are about to download %s to %s. Proceed? " url filename)))
      (user-error "aborted"))
    (let ((log-callback (comingle-log-request state url)))
      (url-retrieve url
                    (lambda (status)
                      (when log-callback
                        (funcall log-callback
                                 (let ((inhibit-read-only t) (print-escape-newlines t))
                                   (format " status: %s"
                                           (prin1-to-string
                                            (or
                                             (if url-http-response-status
                                                 url-http-response-status status)
                                             "no status available"))))))
                      (if (and url-http-response-status (<= 200 url-http-response-status) (<= url-http-response-status 299))
                          (let ((url-buf (current-buffer)))
                            (comingle-run-with-timer state 'default
                                                     (lambda ()
                                                       (comingle-install-process-url-res state url url-buf filename))))
                        (message "comingle cannot fetch local language server: %s %s"
                                 status url-http-response-status)))
                    nil 'silent 'inhibit-cookies))))

(defun comingle-install-process-url-res (state url url-buf filename)
  (make-directory (file-name-directory filename) t)
  (with-temp-file filename
    (set-buffer-multibyte nil)
    (url-insert-buffer-contents url-buf url)
    (unless (zlib-decompress-region (point-min) (point-max))
      (user-error "zlib is unable to decompress")))
  (chmod filename #o744)
  (kill-buffer url-buf)
  (message "successfully installed comingle local language server")
  (comingle-background-process-start state))


(defun comingle-request-callback (status state tracker callback log-callback)
  (let ((buf (current-buffer)))
    (when (eq tracker (comingle-state-alive-tracker state))
      (comingle-run-with-timer state 'default
                               (lambda ()
                                 (when (buffer-live-p buf)
                                   (with-current-buffer buf
                                     (comingle-request-callback-process-res
                                      status callback log-callback))))))))
;; should be local to the url retrieve buffer
(defvar-local comingle-kill-url-retrieve-buffer t)
(defun comingle-request-callback-process-res (status callback log-callback)
  (when log-callback
    (let*
        ((print-escape-newlines t)
         (status-str
          (format " status: %s"
                  (prin1-to-string
                   (or
                    (if url-http-response-status
                        url-http-response-status status)
                    "no status available")))))
      (funcall log-callback status-str
               (if (and url-http-response-status (= url-http-response-status 200))
                   "" status-str))))
  (funcall callback
           (let ((parsed 'error))
             (when url-http-end-of-headers
               (goto-char url-http-end-of-headers)
               (ignore-error json-parse-error
                 (setq parsed (json-parse-buffer :object-type 'alist))))
             (when comingle-kill-url-retrieve-buffer
               (kill-buffer))
             (when (and parsed (not (eq parsed 'error)) log-callback)
               (funcall log-callback
                        (let* ((print-escape-newlines t))
                          (format " %s"
                                  (prin1-to-string
                                   (if (listp parsed)
                                       (or
                                        (alist-get 'state parsed)
                                        (alist-get 'message parsed)
                                        parsed)
                                     parsed))))
                        (when-let
                            ((message-str
                              (and (listp parsed)
                                   (or
                                    (alist-get 'message (alist-get 'state parsed))
                                    (alist-get 'message parsed)))))
                          (when (stringp message-str)
                            (concat " " message-str)))))
             parsed)))

(defun comingle-log-request (state str &optional mode-line-str mode-line-ttl)
  "print str on its own line in *comingle-log*, returns a callback function
that can add to that line."
  (let ((modeline-callback (when mode-line-str (comingle-log-mode-line state mode-line-str mode-line-ttl))))
    (when-let ((buf (comingle-get-config 'comingle-log-buffer nil state)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              time-beg-marker time-end-marker insert-marker
              (start-time (current-time)))
          (save-excursion
            (goto-char (point-max))
            (beginning-of-line)
            (insert-before-markers "\n")
            (goto-char (1- (point)))
            (insert str)
            (insert " ")
            (setq time-beg-marker (point-marker))
            (insert comingle-log-waiting-text)
            (setq time-end-marker (point-marker))
            (set-marker-insertion-type time-end-marker t)
            (setq insert-marker (point-marker))
            (set-marker-insertion-type insert-marker t)
            (lambda (newstr &optional newstr-modeline modeline-append)
              (when (and newstr-modeline modeline-callback)
                (funcall modeline-callback newstr-modeline modeline-append))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (cl--set-buffer-substring time-beg-marker time-end-marker
                                              (format "%.2f secs" (comingle-time-from start-time)))
                    (set-marker-insertion-type time-end-marker nil)
                    (cl--set-buffer-substring insert-marker insert-marker
                                              newstr)
                    (set-marker-insertion-type time-end-marker t)))))))))))

(defvar-local comingle-mode-line nil)
;; requirement for modeline
(put 'comingle-mode-line 'risky-local-variable t)


;; run user code in timers, for efficiency and infinite loop guard
(defvar-local comingle-modeline-refresh-scheduled nil)
(defun comingle-schedule-refresh-modeline-currentbuffer ()
  (unless comingle-modeline-refresh-scheduled
    (run-with-timer 0.005 nil #'comingle-refresh-modeline (current-buffer))
    (setq comingle-modeline-refresh-scheduled t)))

(defun comingle-refresh-modeline (buffer)
  (if (input-pending-p)
      (run-with-idle-timer 0.005 nil #'comingle-refresh-modeline buffer)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unwind-protect
            (force-mode-line-update)
          (setq comingle-modeline-refresh-scheduled nil))))))

(defun comingle-remove-modeline-segment (segment buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq comingle-mode-line (delq segment comingle-mode-line))
      (comingle-schedule-refresh-modeline-currentbuffer))))

(defun comingle-log-mode-line (_state str ttl)
  (let*
      (
       (segment `("[" nil (-30 ,str) "]"))
       (buffer (current-buffer))
       (start-time (current-time))
       (timer (run-with-timer ttl nil #'comingle-remove-modeline-segment segment buffer)))
    (push segment comingle-mode-line)
    (comingle-schedule-refresh-modeline-currentbuffer)
    (lambda (newstr &optional append)
      (cancel-timer timer)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (unless (memq segment comingle-mode-line) (push segment comingle-mode-line))
          (setq timer (run-with-timer ttl nil #'comingle-remove-modeline-segment segment buffer))
          (setf (nth 1 segment)
                (format "%.2fs" (comingle-time-from start-time)))
          (setf (nth 1 (nth 2 segment))
                (if append
                    (concat (nth 1 (nth 2 segment)) newstr)
                  newstr))
          (comingle-schedule-refresh-modeline-currentbuffer))))))

(defun comingle-request-with-body (api state body tracker callback)
  (when (eq tracker (comingle-state-alive-tracker state))
    (if (comingle-get-config 'comingle-port nil state)
        (let*
            (
             (url (comingle-get-config 'comingle-url api state))
             (url-request-method "POST")
             (url-request-extra-headers `(("Content-Type" . "application/json")))
             (url-request-data (encode-coding-string (json-serialize body) 'utf-8))
             (log-callback
              (comingle-log-request state (url-recreate-url url)
                                    (when (comingle-get-config 'comingle-mode-line-enable api state)
                                      (symbol-name api))
                                    (comingle-get-config 'comingle-mode-line-keep-time api state))))
          (when-let
              (
               (url-buf
                (url-retrieve url #'comingle-request-callback
                              (list state (comingle-state-alive-tracker state) callback log-callback)
                              'silent 'inhibit-cookies))
               (url-proc (get-buffer-process url-buf)))
            (set-process-query-on-exit-flag url-proc nil)))
      (comingle-on-port-ready state (lambda () (comingle-request-with-body api state body tracker callback))))))

(defun comingle-request (api state vals-alist callback)
  "make an async request to api, calls callback when done.
callback is called with a single argument, the return of
(json-parse-buffer :object-type \\='alist)

returns the body as returned by comingle-make-body-for-api
If `comingle-api-enabled' returns nil, does nothing.

"
  (unless (comingle-state-alive-tracker state)
    (error "comingle-state is not alive! %s" state))
  (when (comingle-get-config 'comingle-api-enabled api state)
    (let ((body (comingle-make-body-for-api api state vals-alist)))
      (comingle-request-with-body api state body (comingle-state-alive-tracker state) callback)
      body)))


(defun comingle-background-process-schedule (state)
  (comingle-run-with-timer state 'default #'comingle-background-process-start state))

(defun comingle-create-process (state)
  (let (buf (executable (car (comingle-get-config 'comingle-command nil state))))
    (unless (executable-find executable)
      (if (and (file-name-absolute-p executable) (not (file-exists-p executable)))
          (user-error "%s does not exist. use M-x comingle-install to install one"
                      executable)
        (user-error "%s is not a valid executable. use M-x comingle-install to install one"
                    executable)))
    (setq buf (comingle-get-config 'comingle-log-buffer nil state))
    (setf (comingle-state-proc state)
          (make-process
           :name "comingle"
           :connection-type 'pipe
           :buffer buf
           :coding 'no-conversion
           :command (comingle-get-config 'comingle-command nil state)
           :noquery t))))

(defun comingle-background-process-start (state)
  ;; entrypoint
  ;; since user calls start, cancel previous stuff
  (unless (comingle-state-alive-tracker state)
    (error "comingle-state is not alive! %s" state))
  (cond
   (;; we created the process but that is now dead for some reason
    (and (comingle-state-proc state)
         (not (process-live-p (comingle-state-proc state))))
    (comingle-reset state))
   ((and
     (not (comingle-get-config 'comingle-port nil state))
     (not (comingle-get-config 'comingle-directory nil state))
     (not (comingle-state-proc state)))
    (setf (comingle-state-port state) nil)
    (when-let ((dir (comingle-state-manager-directory state)))
      (delete-directory dir t)
      (setf (comingle-state-manager-directory state) nil))
    (comingle-create-process state)
    (comingle-background-process-schedule state))
   ((not (comingle-get-config 'comingle-port nil state))
    (unless (comingle-get-config 'comingle-directory nil state)
      (error "no manager directory defined"))
    (let ((files
           (directory-files (comingle-get-config 'comingle-directory nil state)
                            nil (rx bol (* num) eol))))
      (when files
        (setf (comingle-state-port state) (string-to-number (car files)))
        (mapc (lambda (func) (comingle-run-with-timer state 'default func))
              (comingle-state-port-ready-hook state)))
      (comingle-background-process-schedule state)))
   ((and
     (not (comingle-state-last-auth-token state))
     (not (comingle-state-last-api-key state))
     (not (comingle-get-config 'comingle/metadata/api_key 'GetCompletions state)))
    (let* ((login-method (car (read-multiple-choice
                               "No comingle API key found. Authenticate manually or automatically via a browser: "
                               `((manual "manual")
                                 (auto "auto"))
                               nil nil t)))
           (authurl (comingle-make-auth-url comingle-state nil login-method)))
      (if (eq login-method 'auto)
          (progn
            (browse-url authurl)
            (comingle-request 'GetAuthToken state nil
                              (lambda (res)
                                (if-let ((token (and (listp res) (alist-get 'authToken res))))
                                    (setf (comingle-state-last-auth-token state) token)
                                  (error "Cannot get auth_token from res"))
                                (comingle-background-process-schedule state)))
            (message "you can also use M-x comingle-kill-last-auth-url to copy the comingle login url"))
        (kill-new authurl)
        (setf (comingle-state-last-auth-token state)
              (read-string (format "%s has been copied to clipboard.\nAfter you login, paste the token here:\n" authurl)))
        (comingle-background-process-schedule state))))

   ((and
     (not (comingle-state-last-api-key state))
     (not (comingle-get-config 'comingle/metadata/api_key 'GetCompletions state)))
    (comingle-request 'RegisterUser state nil
                      (lambda (res)
                        (if-let ((key (and (listp res) (alist-get 'api_key res))))
                            (progn
                              (when (y-or-n-p "save comingle/metadata/api_key using customize?")
                                (customize-save-variable 'comingle/metadata/api_key key))
                              (setf (comingle-state-last-api-key state) key))
                          (error "cannot get api_key from res"))
                        (comingle-background-process-schedule state))))
   ((and
     (comingle-get-config 'comingle-chat nil state)
     (not (comingle-state-chat-client-port state))
     (not (comingle-state-chat-webserver-port state)))
    (comingle-request 'GetProcesses state nil
                      (lambda (res)
                        (if (listp res)
                            (setf (comingle-state-chat-client-port state) (alist-get 'chatClientPort res)
                                  (comingle-state-chat-webserver-port state) (alist-get 'chatWebServerPort res))
                          (error "cannot get chat ports from res")))))
   (t
    (comingle-request 'Heartbeat state nil
                      (lambda (_res)
                        (comingle-run-with-timer state 5 #'comingle-background-process-start state))))))

(defun comingle-chat-open (&optional state)
  (interactive)
  (setq state (or state comingle-state))
  (setq comingle-chat-url
        (concat
         "http://127.0.0.1:"
         (number-to-string (or (comingle-state-chat-client-port state)
                               (error "chat client port is not set yet, please wait")))
         "?api_key=" comingle/metadata/api_key
         "&has_enterprise_extension=" (if comingle-enterprise "true" "false")
         "&web_server_url=ws://127.0.0.1:"
         (number-to-string (or (comingle-state-chat-webserver-port state)
                               (error "chat client port is not set yet, please wait")))
         "&ide_name=" comingle/metadata/ide_name
         "&ide_version=" comingle/metadata/ide_version
         "&app_name=Emacs"
         "&extension_name=comingle.el"
         "&extension_version=" comingle/metadata/extension_version
         "&ide_telemetry_enabled=true"
         "&has_index_service=true"
         "&spen_file_pointer_enabled=true"
         "&diff_view_enabled=true"
         "&locale=en_US"))
  (browse-url comingle-chat-url)
  ;; Add current project directory to chat when started
  ;;  TODO: projectile support?
  (comingle-request-with-body 'AddTrackedWorkspace
                              state
                              `((workspace . ,(project-root (project-current))))
                              (comingle-state-alive-tracker state)
                              (lambda (res)
                                (when res
                                  (error "AddTrackedWorkspace failed")))))

(defun comingle-reset (&optional state)
  (interactive)
  (setq state (or state comingle-state))
  (setf (comingle-state-alive-tracker state) nil)
  (when-let ((proc (comingle-state-proc state)))
    (delete-process proc)
    (setf (comingle-state-proc state) nil))
  (when-let ((dir (comingle-state-manager-directory state)))
    (delete-directory dir t)
    (setf (comingle-state-manager-directory state) nil))
  (setf (comingle-state-port state) nil)
  (setf (comingle-state-port-ready-hook state) nil)
  (setf (comingle-state-last-api-key state) nil)
  (setf (comingle-state-last-auth-token state) nil)
  (setf (comingle-state-chat-client-port state) nil)
  (setf (comingle-state-chat-webserver-port state) nil)
  (setf (comingle-state-results-table state) (make-hash-table :test 'eql :weakness nil))
  (setf (comingle-state-pending-table state) (make-hash-table :test 'eql :weakness nil)))


(defun comingle-on-port-ready (state callback)
  (if (comingle-state-port state)
      (funcall callback)
    (push callback (comingle-state-port-ready-hook state))))

(defun comingle-request-cancelrequest (state requestid)
  (comingle-request 'CancelRequest state
                    `((comingle/request_id . ,requestid))
                    #'ignore))

(defun comingle-utf8-byte-length (str)
  (length (encode-coding-string str 'utf-8)))
(defun comingle-make-utf8-offset-table (str offsets)
  (let*
      (
       (str-cur 0)
       (str-len (length str))
       (offset-cur 0)
       (offset-max (apply #'max 0 offsets))
       (table (make-hash-table :test 'eql :weakness nil :size (* 2 (length offsets)))))
    (mapc
     (lambda (offset)
       (puthash offset nil table))
     offsets)
    (while (and (< str-cur str-len) (<= offset-cur offset-max))
      (dotimes (_ (comingle-utf8-byte-length (substring-no-properties str str-cur (1+ str-cur))))
        (unless (eq (gethash offset-cur table 'noexist) 'noexist)
          (puthash offset-cur str-cur table))
        (cl-incf offset-cur))
      (cl-incf str-cur))
    (while (<= offset-cur offset-max)
      (puthash offset-cur str-len table)
      (cl-incf offset-cur))
    table))

(defmacro comingle-gv-map-table (gv table)
  `(setf ,gv (gethash (comingle-string-to-number-safe ,gv) ,table)))

(defmacro comingle-mapcar-mutate (func seq-gv)
  `(setf ,seq-gv (mapcar ,func ,seq-gv)))

(defun comingle-make-completion-string (completion-item document beg end)
  (let ((cur beg))
    (mapconcat
     (lambda (part)
       (when-let*
           (
            ;; should be int since its been processed by comingle-parse-getcompletions-res-process-offsets
            (offset (alist-get 'offset part))
            (type (alist-get 'type part))
            (text (alist-get 'text part)))
         (when (or (string= type "COMPLETION_PART_TYPE_INLINE") (string= type "COMPLETION_PART_TYPE_BLOCK"))
           (prog1
               (concat
                (substring document cur (min offset (length document)))
                ;; (substring document (min cur offset (length document)) (min offset (length document)))
                (when (string= type "COMPLETION_PART_TYPE_BLOCK") "\n")
                text)
             (setq cur offset)))))
     (append (alist-get 'completionParts completion-item) `(((offset . ,end))))
     "")))

(defun comingle-string-to-number-safe (str)
  (if (stringp str) (string-to-number str) str))
(defun comingle-parse-getcompletions-res-process-offsets (document cursor res)
  (let*
      (
       (items (alist-get 'completionItems res))
       (offsets-full-list
        (mapcar #'comingle-string-to-number-safe
                (remove nil
                        (mapcan
                         (lambda (item)
                           (append
                            (list
                             (alist-get 'startOffset (alist-get 'range item))
                             (alist-get 'endOffset (alist-get 'range item)))
                            (mapcar
                             (lambda (part) (alist-get 'offset part))
                             (alist-get 'completionParts item))))
                         items))))
       (offsets-table (comingle-make-utf8-offset-table document (push cursor offsets-full-list)))
       (_
        (comingle-mapcar-mutate
         (lambda (item)
           (comingle-gv-map-table (alist-get 'startOffset (alist-get 'range item)) offsets-table)
           (comingle-gv-map-table (alist-get 'endOffset (alist-get 'range item)) offsets-table)
           (comingle-mapcar-mutate
            (lambda (part)
              (comingle-gv-map-table (alist-get 'offset part) offsets-table)
              part)
            (alist-get 'completionParts item))
           item)
         items)))
    offsets-table))

;; WARNING: this mutates res
(defun comingle-parse-getcompletions-res (req res)
  "takes req and res"

  ;; (setq res (cdr (comingle-request-synchronously 'GetCompletions comingle-state nil)))
  ;; (mapcar 'car res)
  ;; (state completionItems requestInfo)
  ;; (alist-get 'state res)
  ;; (alist-get 'requestInfo res)

  ;; (setq items (alist-get 'completionItems res))
  ;; (setq item (elt items 0))
  ;; (mapcar 'car item)
  ;; (completion range source completionParts)
  ;; (alist-get 'completion item)
  ;; (alist-get 'range item)
  ;; (alist-get 'source item)
  ;; (alist-get 'completionParts item)
  ;; (alist-get 'endOffset (alist-get 'range item))

  ;; (print (alist-get 'state res))
  ;; (alist-get 'completionId (alist-get 'completion item))
  (when (alist-get 'completionItems res)
    (let*
        (
         (document (comingle-nested-alist-get req 'comingle/document/text))
         (cursor (comingle-nested-alist-get req 'comingle/document/cursor_offset))
         (items (alist-get 'completionItems res))
         (offset-hashtable (comingle-parse-getcompletions-res-process-offsets document cursor res))
         (cursor (gethash cursor offset-hashtable))
         offset-list
         (_
          (maphash (lambda (_ offset) (if offset (push offset offset-list))) offset-hashtable))
         (range-min (apply #'min cursor offset-list))
         (range-max (apply #'max cursor offset-list))
         (strings-list
          (mapcar
           (lambda (item) (comingle-make-completion-string item document range-min range-max))
           items))
         (completionids
          (mapcar
           (lambda (item)
             (alist-get 'completionId (alist-get 'completion item)))
           items)))
      ;; (print (elt items 0))
      ;; (print (alist-get 'completionParts (elt items 0)))
      ;; (print strings-list)
      ;; (print (alist-get 'completion (elt items 0)))
      ;; ;; (print (alist-get 'range (elt items 0)))
      ;; ;; (print (alist-get 'source (elt items 0)))
      ;; (print (list (- range-min cursor) (- range-max cursor) (nth 0 strings-list)))
      ;; (print (list range-min cursor range-max))
      (list (- range-min cursor) (- range-max cursor) strings-list completionids))))

(defun comingle-clear-completion ()
  "Clear any active comingle completion."
  (interactive)
  (when comingle-overlay
    (delete-overlay comingle-overlay)
    (delete-overlay comingle-keymap-overlay)
    (setq comingle-overlay nil))
  (setq comingle-current-completions nil
        comingle-current-completion-metadata nil
        comingle-current-completion-index 0
        comingle-completion-info nil))

(defun comingle-get-keymap-overlay ()
  (unless (overlayp comingle-keymap-overlay)
    (setq comingle-keymap-overlay (make-overlay 1 1 nil nil t))
    (overlay-put comingle-keymap-overlay 'keymap comingle-keymap)
    (overlay-put comingle-keymap-overlay 'priority 101))
  comingle-keymap-overlay)

(defun comingle-show-completion (&optional index)
  "Show the comingle completion at INDEX."
  (when comingle-current-completions
    (when comingle-overlay
      (delete-overlay comingle-overlay)
      (setq comingle-overlay nil))
    (setq comingle-current-completion-index
          (mod (or index comingle-current-completion-index)
               (length comingle-current-completions)))
    (let* ((completion-text (nth comingle-current-completion-index comingle-current-completions))
           (info comingle-completion-info)
           (start (plist-get info :start))
           (end (plist-get info :end))
           (prefix (buffer-substring-no-properties start end)))
      (when (string-prefix-p prefix completion-text)
        (let ((suffix (substring completion-text (length prefix))))
          (when (> (length suffix) 0)
            (setq comingle-overlay (make-overlay end end))
            (overlay-put comingle-overlay 'keymap (comingle-get-keymap-overlay))
            (overlay-put comingle-overlay 'after-string
                         (propertize suffix 'face 'comingle-ghost-text-face))
            (overlay-put comingle-overlay 'comingle t)))))))

(defun comingle-handle-completion-response (req res buffer point-before-request)
  "Callback for handling completion responses from comingle server."
  (when (and (not (input-pending-p))
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((parsed (comingle-parse-getcompletions-res req res)))
        (when parsed
          (cl-destructuring-bind (dmin dmax strings completion-ids) parsed
            (when (and strings (> (length strings) 0))
              (let* ((start (+ dmin point-before-request))
                     (end (+ dmax point-before-request)))
                (when (and (<= (point-min) start) (<= end (point-max)))
                  (comingle-clear-completion)
                  (setq comingle-current-completions strings)
                  (setq comingle-current-completion-metadata completion-ids)
                  (setq comingle-completion-info (list :start start :end end :point point-before-request))
                  (comingle-show-completion 0))))))))))

(defun comingle-get-completion ()
  "Request a completion from comingle."
  (interactive)
  (when (and (comingle-state-alive-tracker comingle-state)
             (comingle-get-config 'comingle-api-enabled 'GetCompletions comingle-state))
    (let* ((req-body (comingle-make-body-for-api 'GetCompletions comingle-state nil))
           (buffer (current-buffer))
           (point (point)))
      (comingle-request-with-body 'GetCompletions
                                  comingle-state
                                  req-body
                                  (comingle-state-alive-tracker comingle-state)
                                  (lambda (res)
                                    (comingle-handle-completion-response req-body res buffer point))))))

(defun comingle-accept-completion ()
  "Accept the current comingle completion."
  (interactive)
  (when comingle-overlay
    (let* ((info comingle-completion-info)
           (start (plist-get info :start))
           (end (plist-get info :end))
           (completion-text (nth comingle-current-completion-index comingle-current-completions))
           (completion-id (nth comingle-current-completion-index comingle-current-completion-metadata)))
      (comingle-clear-completion)
      (delete-region start end)
      (goto-char start)
      (insert completion-text)
      (when completion-id
        (comingle-request 'AcceptCompletion comingle-state
                          `((comingle/completion_id . ,completion-id))
                          #'ignore)))))

(defun comingle--accept-partial (split-fn)
  "Accept a part of a completion, given a splitting function (word, or line).
   `SPLIT-FN' takes the displayed suffix text and returns a cons cell
   of `(PART-TO-INSERT . REST-OF-SUFFIX)'."
  (when comingle-overlay
    (let* ((overlay-start (overlay-start comingle-overlay))
           (suffix-text (substring-no-properties (overlay-get comingle-overlay
                                                              'after-string)))
           (parts (funcall split-fn suffix-text)))
      (when parts
        (let ((to-insert (car parts))
              (remaining-suffix (cdr parts)))

          (goto-char overlay-start)
          (insert to-insert)
          (setf (plist-get comingle-completion-info :end) (point))

          (if (or (null remaining-suffix) (string-empty-p remaining-suffix))
              (comingle-clear-completion)
            ;; Part of the completion was accepted. The full completion text in
            ;; `comingle-current-completions` is still good, and `comingle-completion-info`
            ;; now has an updated :end. `comingle-show-completion` will re-calc the new
            ;; prefix and suffix display the remaining part.
            (comingle-show-completion comingle-current-completion-index)))))))

(defun comingle-accept-completion-word ()
  "Accept the next word of the comingle completion."
  (interactive)
  (comingle--accept-partial
   (lambda (text)
     (if (string-match "\\`\\s-*\\S-+" text)
         (cons (match-string 0 text) (substring text (match-end 0)))
       ;; If no word found (e.g., only whitespace), accept that whitespace.
       (if (string-match "\\`\\s-+\\'" text)
           (cons text "")
         nil)))))

(defun comingle-accept-completion-character ()
  "Accept the next character of the comingle completion."
  (interactive)
  (comingle--accept-partial
   (lambda (text)
     (if (string-match "\\`\\s-*\\S+" text)
         (cons (match-string 0 text) (substring text (match-end 0)))
       ;; If no character found (e.g., only whitespace), accept that whitespace.
       (if (string-match "\\`\\s-+\\'" text)
           (cons text "")
         nil)))))

(defun comingle-accept-completion-line ()
  "Accept the first line of the comingle completion."
  (interactive)
  (comingle--accept-partial
   (lambda (text)
     (let ((newline-pos (string-search "\n" text)))
       (if newline-pos
           (cons (substring text 0 (1+ newline-pos))
                 (substring text (1+ newline-pos)))
         ;; No newline, so whole text is one line.
         (cons text ""))))))

(defun comingle-next-completion ()
  "Cycle to the next completion."
  (interactive)
  (when comingle-current-completions
    (comingle-show-completion (1+ comingle-current-completion-index))))

(defun comingle-prev-completion ()
  "Cycle to the previous completion."
  (interactive)
  (when comingle-current-completions
    (comingle-show-completion (1- comingle-current-completion-index))))

(defconst comingle-keymap (make-sparse-keymap)
  "Keymap for Comingle completion overlay.")

(defun comingle-post-command-hook ()
  "Hook for `post-command-hook` to trigger completions."
  (when (and comingle-auto-complete
             (not (minibufferp))
             (member this-command '(self-insert-command newline)))
    (unless (and comingle-overlay
                 (overlay-buffer comingle-overlay))
      (comingle-run-with-timer
       comingle-state
       (comingle-get-config 'comingle-auto-complete-delay nil comingle-state)
       #'comingle-get-completion))))

(defun comingle-pre-command-hook ()
  "Hook for `pre-command-hook` to clear completions."
  (when (and comingle-overlay (overlay-buffer comingle-overlay))
    (unless (memq this-command '(comingle-accept-completion
                                 comingle-accept-completion-word
                                 comingle-accept-completion-line
                                 comingle-next-completion
                                 comingle-prev-completion
                                 scroll-up-command
                                 scroll-down-command))
      (comingle-clear-completion))))

;;;###autoload
(define-minor-mode comingle-mode
  "Toggle comingle's in-buffer completion UI."
  :init-value nil
  :lighter " comingle"
  :keymap comingle-keymap
  (if comingle-mode
      (progn
        (add-hook 'post-command-hook #'comingle-post-command-hook nil t)
        (add-hook 'pre-command-hook #'comingle-pre-command-hook nil t)
        (unless (comingle-state-alive-tracker comingle-state)
          (comingle-init)))
    (remove-hook 'post-command-hook #'comingle-post-command-hook t)
    (remove-hook 'pre-command-hook #'comingle-pre-command-hook t)
    (comingle-clear-completion)))

;;;###autoload
(defun comingle-init (&optional state)
  (interactive)
  (setq state (or state comingle-state))
  (setf (comingle-state-alive-tracker state)
        (gensym (comingle-state-name comingle-state)))
  (condition-case err
      (comingle-background-process-start state)
    (error (comingle-reset state)
           (signal (car err) (cdr err)))))

(provide 'comingle)
;;; comingle.el ends here
