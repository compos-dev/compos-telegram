;;; telegram-app.scm — your own Telegram account in compos, as an app.
;;;
;;; M-x telegram lists your chats, newest first, with their unread counts;
;;; moving the point shows the conversation beside the list, one page per
;;; chat. r replies, R marks the chat read, s searches every chat, g reads
;;; again. New messages arrive live: the list and an open page follow them,
;;; a direct message shows a notice, and each one is published as an event
;;; on telegram-account:CHAT-ID for agents to listen to.
;;;
;;; The account is reached through the bridge (bridge/bridge.py in this
;;; package: a Telethon client logged in as the account, serving JSON on
;;; telegram-bridge-url with a bearer token). M-x telegram-login signs the
;;; bridge in. telegram.scm is the separate bot gateway.

(require 'events)

(package! "telegram-app")

(domain! 'chat)
(effects! '(write))

(defcustom 'telegram-bridge-url ""
  "The Telegram account bridge, such as http://HOST:8792 (bridge/bridge.py of this package)."
  'group 'telegram 'type 'string)

(defcustom 'telegram-bridge-token #f
  "The bridge's bearer token; also read from TELEGRAM_BRIDGE_TOKEN. The bridge reads the same token."
  'group 'telegram 'type 'secret 'env "TELEGRAM_BRIDGE_TOKEN")

(defcustom 'telegram-chat-limit 100
  "How many chats the listing reads."
  'group 'telegram 'type 'integer)

(defcustom 'telegram-message-limit 60
  "How many recent messages a conversation page reads."
  'group 'telegram 'type 'integer)

(defcustom 'telegram-notify #t
  "Show a notice for each direct message that arrives on the account."
  'group 'telegram 'type 'boolean)

(define *telegram-buffer* "*telegram*")
(define *telegram-dialogs* (if (boundp '*telegram-dialogs*) *telegram-dialogs* '()))
(define *telegram-state* (if (boundp '*telegram-state*) *telegram-state* "not read yet: g reads the chats"))
(define *telegram-feed-gen* (if (boundp '*telegram-feed-gen*) *telegram-feed-gen* 0))
(define *telegram-feed-seq* (if (boundp '*telegram-feed-seq*) *telegram-feed-seq* 0))
(define *telegram-feed-on* (if (boundp '*telegram-feed-on*) *telegram-feed-on* #f))
(define *telegram-me* (if (boundp '*telegram-me*) *telegram-me* #f))

;;; ---------------------------------------------------------------- text

(domain! 'chat)
(effects! '(pure))

(define (telegram--str x fallback)
  (if (and (string? x) (not (equal? x ""))) x fallback))

(define (telegram--one-line s)
  (string-replace (telegram--str s "") "\n" " "))

(define (telegram--time ts)
  (cond ((not (number? ts)) "")
        ((equal? (format-time ts "%Y-%m-%d") (format-time (current-time) "%Y-%m-%d"))
         (format-time ts "%H:%M"))
        (else (format-time ts "%d %b"))))

(define telegram--events
  '(("ContactSignUp" "joined Telegram") ("ChatAddUser" "added to the group")
    ("ChatJoinedByLink" "joined by link") ("ChatDeleteUser" "left the group")
    ("PinMessage" "pinned a message") ("PhoneCall" "call") ("ChatCreate" "created the group")
    ("ChatEditTitle" "renamed the group") ("ChatEditPhoto" "changed the group photo")))

(define (telegram--media-label m)
  (let ((media (plist-get m 'media)))
    (if (and (pair? media) (equal? (plist-get media 'type) "event"))
        (let ((hit (assoc (telegram--str (plist-get media 'name) "") telegram--events)))
          (string-append "[" (if hit (cadr hit) (telegram--str (plist-get media 'name) "event")) "]"))
    (if (pair? media)
        (let ((type (telegram--str (plist-get media 'type) "media")))
          (string-append "[" type
                         (if (plist-get media 'name) (string-append ": " (plist-get media 'name)) "")
                         (if (plist-get media 'emoji) (string-append " " (plist-get media 'emoji)) "")
                         "]"))
        ""))))

(define (telegram--message-text m)
  (let ((media (telegram--media-label m)) (text (telegram--str (plist-get m 'text) "")))
    (cond ((equal? media "") text)
          ((equal? text "") media)
          (else (string-append media " " text)))))

(define (telegram--preview d)
  (let ((m (plist-get d 'last)))
    (if (pair? m)
        (string-append (if (plist-get m 'out) "you: " "") (telegram--one-line (telegram--message-text m)))
        "")))

(define (telegram--name d)
  (telegram--str (plist-get d 'name) (string-append "chat " (number->string (plist-get d 'id)))))

(define (telegram-detail-buffer d)
  (string-append "*telegram: " (telegram--name d) "*"))

(define (telegram--dialog id)
  (find (lambda (d) (equal? (plist-get d 'id) id)) *telegram-dialogs*))

(define (telegram--plist-put p k v)
  (cond ((null? p) (list k v))
        ((eq? (car p) k) (cons k (cons v (cddr p))))
        (else (cons (car p) (cons (cadr p) (telegram--plist-put (cddr p) k v))))))

;;; ---------------------------------------------------------------- the bridge

(domain! 'chat)
(effects! '(write external))

(define (telegram-account--call method path body k &optional timeout)
  "METHOD PATH on the bridge with plist BODY as JSON; K gets the error text or #f, and the parsed answer"
  (let ((token (secret-value 'telegram-bridge-token)))
    (cond
      ((equal? telegram-bridge-url "")
       (k "Telegram: set telegram-bridge-url to the bridge" #f))
      ((not token)
       (k "Telegram: no bridge token (TELEGRAM_BRIDGE_TOKEN)" #f))
      (else
        (http-request (string-append telegram-bridge-url path)
          (append (list 'method method 'timeout (or timeout 30000)
                        'headers (list 'authorization (string-append "Bearer " token)
                                       'content-type "application/json"))
                  (if body (list 'body (json-encode body)) '()))
          (lambda (reply)
            (let ((j (http-json reply)))
              (if (and j (plist-get j 'ok))
                  (k #f j)
                  (k (string-append "Telegram: "
                                    (or (and j (plist-get j 'error)) (plist-get reply 'error)
                                        (string-append "HTTP " (number->string (or (plist-get reply 'status) 0)))))
                     j)))))))))

(define (telegram-account-status k)
  (telegram-account--call "GET" "/status" #f
    (lambda (err j) (k err (and j (plist-get j 'result))))))
(public! 'telegram-account-status "(telegram-account-status K) — whether the bridge is signed in, and as whom; K gets ERR and (authorized BOOL me (id name username phone))")

(define (telegram-login-code! phone k)
  (telegram-account--call "POST" "/login/code" (list 'phone phone) (lambda (err j) (k err))))
(public! 'telegram-login-code! "(telegram-login-code! PHONE K) — Telegram sends a login code to the account of PHONE (+country code); K gets ERR")

(define (telegram-login-sign-in! code k)
  (telegram-account--call "POST" "/login/sign_in" (list 'code code)
    (lambda (err j) (k err (and j (plist-get j 'result))))))
(public! 'telegram-login-sign-in! "(telegram-login-sign-in! CODE K) — sign the bridge in with the login code; K gets ERR and (authorized BOOL [needs password])")

(define (telegram-login-password! password k)
  (telegram-account--call "POST" "/login/sign_in" (list 'password password)
    (lambda (err j) (k err (and j (plist-get j 'result))))))
(public! 'telegram-login-password! "(telegram-login-password! PASSWORD K) — finish a sign-in with the two-step password; K gets ERR and (authorized BOOL)")

(define (telegram-dialogs k)
  (telegram-account--call "GET" (string-append "/dialogs?limit=" (number->string telegram-chat-limit)) #f
    (lambda (err j) (k err (and j (plist-get j 'result))))))
(public! 'telegram-dialogs "(telegram-dialogs K) — the account's chats, newest first; K gets ERR and rows of id name kind unread mentions pinned ts last")

(define (telegram-messages chat k &optional limit before)
  (telegram-account--call "GET" (string-append "/messages?chat=" (number->string chat)
                                       "&limit=" (number->string (or limit telegram-message-limit))
                                       (if before (string-append "&before=" (number->string before)) ""))
    #f (lambda (err j) (k err (and j (plist-get j 'result))))))
(public! 'telegram-messages "(telegram-messages CHAT K [LIMIT BEFORE]) — CHAT's messages, newest first; K gets ERR and rows of id ts out sender text reply_to media")

(define (telegram-search-messages query k &optional chat)
  (telegram-account--call "GET" (string-append "/search?q=" (url-encode query)
                                       (if chat (string-append "&chat=" (number->string chat)) ""))
    #f (lambda (err j) (k err (and j (plist-get j 'result))))))
(public! 'telegram-search-messages "(telegram-search-messages QUERY K [CHAT]) — messages matching QUERY in CHAT or in every chat; K gets ERR and rows that also carry chat_id and chat")

(define (telegram-say! chat text k &optional reply-to)
  (telegram-account--call "POST" "/send"
    (append (list 'chat chat 'text text) (if reply-to (list 'reply_to reply-to) '()))
    (lambda (err j) (k err (and j (plist-get j 'result))))))
(public! 'telegram-say! "(telegram-say! CHAT TEXT K [REPLY-TO]) — send TEXT to CHAT as the account itself; K gets ERR and the sent message")

(define (telegram-mark-read! chat k)
  (telegram-account--call "POST" "/read" (list 'chat chat) (lambda (err j) (k err))))
(public! 'telegram-mark-read! "(telegram-mark-read! CHAT K) — mark CHAT read on the account; K gets ERR")

;;; ---------------------------------------------------------------- pages

(define telegram--detail-actions
  '(("telegram:reply" "Reply" "r")
    ("telegram:read" "Mark read" "R")
    ("telegram:refresh" "Refresh" "g")
    ("telegram:search" "Search all chats" "s")))

(define (telegram--message-block m)
  (component 'ui/row
    (list 'segs (list (list "c-dim" (telegram--time (plist-get m 'ts)))
                      (list (if (plist-get m 'out) "c-accent" "")
                            (string-append "  " (if (plist-get m 'out) "you" (telegram--str (plist-get m 'sender) "?"))))
                      (list "" (string-append "  " (telegram--message-text m)))))))

(define (telegram-detail-blocks d msgs notice)
  (append
    (list
      (component 'ui/section (list 'title (telegram--name d)))
      (component 'ui/badge
        (list 'text (string-append (telegram--str (plist-get d 'kind) "chat")
                                   (let ((u (or (plist-get d 'unread) 0)))
                                     (if (> u 0) (string-append " · " (number->string u) " unread") "")))
              'class (if (> (or (plist-get d 'unread) 0) 0) "warn" "good")))
      (component 'ui/actions (list 'actions telegram--detail-actions))
      (component 'ui/section (list 'title "Messages" 'count (length msgs))))
    (if (pair? msgs)
        (map telegram--message-block msgs)
        (list (component 'ui/empty (list 'text (or notice "No messages.")))))
    (list (list 'tag "div" 'text "— end —"))))

(domain! 'chat)
(effects! '(write external display))

(define (telegram--join! buf)
  (let ((g (frame-group)))
    (when (and g (not (buffer-in-group? buf g))) (buffer-add-group! buf g)))
  buf)

(define (telegram--draw! buf)
  (let ((d (telegram--dialog (buffer-local buf 'telegram-chat))))
    (when d
      (anchor-page! buf (telegram-detail-blocks d (or (buffer-local buf 'telegram-messages) '())
                                                (buffer-local buf 'telegram-notice))))))

(define (telegram--read-page! buf)
  (let ((id (buffer-local buf 'telegram-chat)))
    (telegram-messages id
      (lambda (err rows)
        (when (buffer-exists? buf)
          (if err
              (buffer-set-local! buf 'telegram-notice err)
              (begin (buffer-set-local! buf 'telegram-messages (reverse rows))
                     (buffer-set-local! buf 'telegram-read-at (current-time))))
          (telegram--draw! buf))))))

(define (telegram--page! d)
  (let ((buf (telegram-detail-buffer d)))
    (unless (buffer-exists? buf) (buffer-create buf))
    (telegram--join! buf)
    (unless (buffer-derived-mode? buf "telegram-detail-mode")
      (with-current-buffer buf (lambda () (set-mode! "telegram-detail-mode"))))
    (buffer-set-local! buf 'telegram-chat (plist-get d 'id))
    (telegram--draw! buf)
    ;; what the page read stands for a minute; an older read reads again
    (let ((at (buffer-local buf 'telegram-read-at)))
      (when (or (not at) (> (- (current-time) at) 60))
        (telegram--read-page! buf)))
    buf))

(define (telegram-show-detail! listing d)
  (when d
    (let ((buf (telegram--page! d)))
      (when (buffer-exists? listing) (display-buffer-detail! buf listing))
      buf)))

(define (telegram--current)
  "the chat at hand: the listing's row, or the page's own"
  (let ((buf (current-buffer)))
    (if (buffer-derived-mode? buf "telegram-mode")
        (list-current buf)
        (let ((id (buffer-local buf 'telegram-chat))) (and id (telegram--dialog id))))))

(define (telegram--redraw-list!)
  (when (buffer-exists? *telegram-buffer*) (list-refresh! *telegram-buffer*)))

(define (telegram-refresh!)
  "read the chats again and redraw the listing and its open pages"
  (set! *telegram-state* "reading…")
  (telegram--redraw-list!)
  (telegram-dialogs
    (lambda (err rows)
      (if err
          (begin (set! *telegram-state* err) (message err))
          (begin (set! *telegram-dialogs* rows) (set! *telegram-state* "live")))
      (telegram--redraw-list!)
      (for-each (lambda (d)
                  (let ((b (telegram-detail-buffer d)))
                    (when (buffer-exists? b) (telegram--read-page! b))))
                *telegram-dialogs*))))

(define (telegram-reply-interactive! d)
  (when d
    (read-string (string-append "Telegram to " (telegram--name d) ": ")
      (lambda (text)
        (if (equal? (string-trim text) "")
            (message "Telegram: nothing sent")
            (begin
              (message (string-append "Telegram: sending to " (telegram--name d) "…"))
              (telegram-say! (plist-get d 'id) text
                (lambda (err m)
                  (if err (message err)
                      (begin (message (string-append "Telegram: sent to " (telegram--name d)))
                             (telegram--incoming! (list 'chat_id (plist-get d 'id) 'chat (plist-get d 'name)
                                                        'kind (plist-get d 'kind) 'message m))))))))))))

(define (telegram-mark-read-interactive! d)
  (when d
    (telegram-mark-read! (plist-get d 'id)
      (lambda (err)
        (if err (message err)
            (begin
              (set! *telegram-dialogs*
                (map (lambda (x) (if (equal? (plist-get x 'id) (plist-get d 'id)) (telegram--plist-put x 'unread 0) x))
                     *telegram-dialogs*))
              (telegram--redraw-list!)
              (let ((b (telegram-detail-buffer d))) (when (buffer-exists? b) (telegram--draw! b)))
              (message (string-append "Telegram: " (telegram--name d) " marked read"))))))))

(define *telegram-search-buffer* "*telegram search*")

(define (telegram-search-interactive!)
  (read-string "Search Telegram: "
    (lambda (q)
      (unless (equal? (string-trim q) "")
        (message (string-append "Telegram: searching for " q "…"))
        (telegram-search-messages q
          (lambda (err rows)
            (if err (message err)
                (let ((buf *telegram-search-buffer*))
                  (unless (buffer-exists? buf) (buffer-create buf))
                  (telegram--join! buf)
                  (anchor-page! buf
                    (append
                      (list (component 'ui/section (list 'title (string-append "Telegram: " q) 'count (length rows))))
                      (if (pair? rows)
                          (map (lambda (m)
                                 (component 'ui/row
                                   (list 'segs (list (list "c-dim" (telegram--time (plist-get m 'ts)))
                                                     (list "c-accent" (string-append "  " (telegram--str (plist-get m 'chat) "?")))
                                                     (list "" (string-append "  " (if (plist-get m 'out) "you" (telegram--str (plist-get m 'sender) "?")) ":"))
                                                     (list "" (string-append "  " (telegram--message-text m)))))))
                               rows)
                          (list (component 'ui/empty (list 'text "No messages match."))))
                      (list (list 'tag "div" 'text "— end —"))))
                  (if (buffer-exists? *telegram-buffer*)
                      (display-buffer-detail! buf *telegram-buffer*)
                      (with-frame-windows (lambda () (display-buffer-other-window! buf))))))))))))

;;; ---------------------------------------------------------------- live

(define (telegram--incoming! e)
  (let* ((id (plist-get e 'chat_id))
         (m (plist-get e 'message))
         (old (telegram--dialog id))
         (out? (plist-get m 'out))
         (d (if old
                (telegram--plist-put
                  (telegram--plist-put
                    (telegram--plist-put old 'last m)
                    'ts (plist-get m 'ts))
                  'unread (if out? 0 (+ 1 (or (plist-get old 'unread) 0))))
                (list 'id id 'name (plist-get e 'chat) 'kind (plist-get e 'kind)
                      'unread (if out? 0 1) 'mentions 0 'pinned #f 'ts (plist-get m 'ts) 'last m))))
    (set! *telegram-dialogs* (cons d (filter (lambda (x) (not (equal? (plist-get x 'id) id))) *telegram-dialogs*)))
    (telegram--redraw-list!)
    (let ((b (telegram-detail-buffer d)))
      (when (buffer-exists? b)
        (let ((msgs (or (buffer-local b 'telegram-messages) '())))
          (unless (find (lambda (x) (equal? (plist-get x 'id) (plist-get m 'id))) msgs)
            (buffer-set-local! b 'telegram-messages (append msgs (list m)))))
        (telegram--draw! b)))
    (event-publish! (string-append "telegram-account:" (number->string id)) (if out? 'sent 'received)
                    (list 'chat (telegram--name d) 'from (telegram--str (plist-get m 'sender) "")
                          'text (telegram--message-text m)))
    (when (and telegram-notify (not out?) (equal? (plist-get d 'kind) "user"))
      (message (string-append "Telegram · " (telegram--name d) ": " (telegram--one-line (telegram--message-text m)))))))

(define (telegram--feed! gen)
  (when (= gen *telegram-feed-gen*)
    (telegram-account--call "GET" (string-append "/updates?timeout=25&after=" (number->string *telegram-feed-seq*)) #f
      (lambda (err j)
        (when (= gen *telegram-feed-gen*)
          (if err
              (debounce! "telegram-feed-retry" 15000 telegram--feed! gen)
              (let ((seq (or (plist-get j 'seq) 0)))
                ;; the bridge restarted and counts from zero again
                (when (< seq *telegram-feed-seq*) (set! *telegram-feed-seq* 0))
                (for-each (lambda (e)
                            (set! *telegram-feed-seq* (max *telegram-feed-seq* (or (plist-get e 'seq) 0)))
                            (report-errors "a Telegram message failed" (lambda () (telegram--incoming! e))))
                          (or (plist-get j 'result) '()))
                (when (> seq *telegram-feed-seq*) (set! *telegram-feed-seq* seq))
                (telegram--feed! gen)))))
      40000)))

(define (telegram-feed-start!)
  (set! *telegram-feed-gen* (+ 1 *telegram-feed-gen*))
  (set! *telegram-feed-on* #t)
  (telegram--feed! *telegram-feed-gen*)
  #t)
(public! 'telegram-feed-start! "(telegram-feed-start!) — follow the account's new messages live")

(define (telegram-feed-stop!)
  (set! *telegram-feed-gen* (+ 1 *telegram-feed-gen*))
  (set! *telegram-feed-on* #f)
  (debounce-cancel! "telegram-feed-retry")
  #t)
(public! 'telegram-feed-stop! "(telegram-feed-stop!) — stop following the account")

;;; ---------------------------------------------------------------- commands

(define-command "telegram-open" "Show the Telegram chat on this row beside the list"
  (lambda () (telegram-show-detail! *telegram-buffer* (telegram--current))))
(define-command "telegram-reply" "Send a message to this Telegram chat as yourself"
  (lambda () (telegram-reply-interactive! (telegram--current))))
(define-command "telegram-mark-read" "Mark this Telegram chat read"
  (lambda () (telegram-mark-read-interactive! (telegram--current))))
(define-command "telegram-search" "Search every Telegram chat"
  (lambda () (telegram-search-interactive!)))
(define-command "telegram-refresh" "Read the Telegram chats again"
  (lambda () (telegram-refresh!)))

(define (telegram--signed-in! result)
  (if (plist-get result 'authorized)
      (begin (message "Telegram: signed in")
             (unless *telegram-feed-on* (telegram-feed-start!))
             (telegram-refresh!))
      (message "Telegram: not signed in")))

(define-command "telegram-login" "Sign the Telegram bridge in to your account: phone, then the code Telegram sends"
  (lambda ()
    (read-string "Telegram phone (+country code): "
      (lambda (phone)
        (telegram-login-code! (string-trim phone)
          (lambda (err)
            (if err (message err)
                (read-string "Code from Telegram: "
                  (lambda (code)
                    (telegram-login-sign-in! (string-trim code)
                      (lambda (err r)
                        (cond
                          (err (message err))
                          ((equal? (plist-get r 'needs) "password")
                           (read-string "Two-step password: "
                             (lambda (pw)
                               (telegram-login-password! pw
                                 (lambda (err r) (if err (message err) (telegram--signed-in! r)))))))
                          (else (telegram--signed-in! r))))))))))))))

(add-hook! (list 'block-click 'telegram)
  (lambda (buf id)
    (let ((chat (buffer-local buf 'telegram-chat)))
      (and chat
           (let ((d (telegram--dialog chat)))
             (cond ((equal? id "telegram:reply") (telegram-reply-interactive! d) #t)
                   ((equal? id "telegram:read") (telegram-mark-read-interactive! d) #t)
                   ((equal? id "telegram:refresh") (telegram--read-page! buf) #t)
                   ((equal? id "telegram:search") (telegram-search-interactive!) #t)
                   (else #f)))))))

(domain! 'chat)
(effects! '(read))

(define (telegram-rows buf) *telegram-dialogs*)

(define (telegram-columns buf)
  (list (list "chat" 26) (list "new" 5) (list "when" 7) (list "last message" #f)))

(define (telegram-cells buf d)
  (let ((u (or (plist-get d 'unread) 0)))
    (list (list (telegram--name d) (if (> u 0) "accent" ""))
          (if (> u 0) (list (number->string u) "warn") (list "" "dim"))
          (list (telegram--time (plist-get d 'ts)) "dim")
          (list (telegram--preview d) (if (> u 0) "" "dim")))))

(define (telegram-meta buf)
  (let ((unread (length (filter (lambda (d) (> (or (plist-get d 'unread) 0) 0)) *telegram-dialogs*))))
    (string-append (if *telegram-me* (string-append (plist-get *telegram-me* 'name) " · ") "")
                   (number->string (length *telegram-dialogs*)) " chats · "
                   (number->string unread) " unread · " *telegram-state*)))

(domain! 'chat)
(effects! '(write external display))

(define-list-mode! "telegram-mode"
  (list 'doc "Your Telegram chats, newest first, with how many messages are unread. Moving or RET shows the conversation beside the list. r sends a message as yourself, R marks the chat read, s searches every chat, g reads Telegram again. New messages arrive by themselves. / narrows. q quits."
        'transient #f
        'noun "chat"
        'rows telegram-rows
        'key (lambda (buf d) (number->string (plist-get d 'id)))
        'columns telegram-columns
        'cells telegram-cells
        'title (lambda (buf) "Telegram")
        'meta telegram-meta
        'preview telegram-show-detail!
        'debounce #t
        'footer (lambda (b) (list (list "RET" "read") (list "r" "reply") (list "R" "mark read")
                                  (list "s" "search") (list "g" "refresh") (list "q" "quit")))
        'keys (list (list "RET" "telegram-open")
                    (list "r" "telegram-reply")
                    (list "R" "telegram-mark-read")
                    (list "s" "telegram-search")
                    (list "g" "telegram-refresh")
                    (list "q" "quit-window"))))

(define-mode "telegram-detail-mode" (lambda () #f))

(mode-keys! "telegram-detail-mode"
  '(("r" "telegram-reply")
    ("R" "telegram-mark-read")
    ("s" "telegram-search")
    ("g" "telegram-refresh")
    ("q" "quit-window")))

(mode-doc! "telegram-detail-mode"
  "One Telegram conversation beside the list, oldest message first. r sends a message as yourself, R marks the chat read, s searches every chat, g reads Telegram again. New messages appear by themselves.")

(detail-name! "telegram-detail-mode"
  (lambda (buf) (string-append "*telegram kept: " (let ((d (telegram--dialog (buffer-local buf 'telegram-chat)))) (if d (telegram--name d) buf)) "*")))

(mode-icon! "telegram-mode" "T")
(mode-icon! "telegram-detail-mode" "T")

(define (telegram-listing!)
  (unless (buffer-exists? *telegram-buffer*) (buffer-create *telegram-buffer*))
  (telegram--join! *telegram-buffer*)
  (unless (buffer-derived-mode? *telegram-buffer* "telegram-mode")
    (with-current-buffer *telegram-buffer* (lambda () (set-mode! "telegram-mode"))))
  (list-refresh! *telegram-buffer*)
  *telegram-buffer*)

(define-command "telegram" "Open the Telegram app: your chats, a page for each"
  (lambda ()
    (let ((buf (telegram-listing!)))
      (app-open! buf)
      (unless *telegram-feed-on* (telegram-feed-start!))
      (telegram-refresh!))))

;; follow the account from boot, so notices and events come without the app open
(after-init
  (lambda ()
    (when (and (not *telegram-feed-on*) (secret-value 'telegram-bridge-token))
      (telegram-account--call "GET" "/status" #f
        (lambda (err j)
          (unless err
            (set! *telegram-me* (plist-get (plist-get j 'result) 'me))
            (telegram-feed-start!)))))))
