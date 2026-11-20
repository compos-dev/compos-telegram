;;; telegram.scm — a Telegram bot gateway to compos agents, in the manner
;;; of the openclaw and hermes gateways. The telegram package; its account
;;; app is telegram-app.scm, loaded at the end of this file.
;;;
;;; The bot long-polls Telegram. A direct message from an approved person,
;;; or a message in a group that @mentions the bot or replies to it, goes
;;; to that conversation's own agent chat (group "Telegram: NAME"). While
;;; the agent works, Telegram shows typing and a progress message of the
;;; tools it runs; the final reply arrives formatted, as a reply to the
;;; message that asked. A stranger gets a pairing code, and M-x
;;; telegram-allow approves it. Photos, files and voice notes are saved
;;; under telegram-files-directory and the agent is told where.
;;; Commands: /new /stop /status /help.

(require 'events)
(require 'groups)

(package! "telegram")

(domain! 'chat)
(effects! '(write))

(defgroup 'telegram "A Telegram bot that lets approved people message a compos agent.")

(defcustom 'telegram-bot-token ""
  "The bot token @BotFather gave; also read from TELEGRAM_BOT_TOKEN."
  'group 'telegram 'type 'secret 'env "TELEGRAM_BOT_TOKEN")

(defcustom 'telegram-allowed-users '()
  "Who may talk to the agent: a list of (ID NAME [ROLE]), ID the Telegram user id, ROLE owner or staff (the default)."
  'group 'telegram 'type 'sexp)

(defcustom 'telegram-chat-routes '()
  "Which compos chat answers whom: a list of (WHO GROUP). WHO is a Telegram user id, a group chat id, a name as approved or a group title, or default; GROUP is a compos group name, whose main chat answers. Unrouted, each person and each group gets its own group \"Telegram: NAME\"."
  'group 'telegram 'type 'sexp)

(defcustom 'telegram-enabled #f
  "Run the Telegram bot. It starts at boot, so it stays on after a restart."
  'group 'telegram 'type 'boolean)

(defcustom 'telegram-progress #t
  "Show a progress message in Telegram of the tools the agent runs."
  'group 'telegram 'type 'boolean)

(defcustom 'telegram-files-directory "~/.compos/telegram/files/"
  "Where photos, files and voice notes sent to the bot are saved."
  'group 'telegram 'type 'string)

(defcustom 'telegram-chat-brief
  "You are talking with a staff member over Telegram. Each message below comes from them. Your final reply text is sent back to them as a Telegram message: keep it short, use plain Markdown (bold, code, links, lists) and no tables. They cannot see this editor and cannot answer permission prompts. A photo, file or voice note they send is saved on disk and its path is given to you."
  "What a new Telegram chat with a staff member is told before the first message."
  'group 'telegram 'type 'string)

(defcustom 'telegram-group-brief
  "You are in a Telegram group with the owner of this editor and their staff. You see only the messages that @mention you; each one starts with the sender's name. Your final reply text is posted to the group as a reply to that message: keep it short, use plain Markdown (bold, code, links, lists) and no tables. Do what the owner asks; for staff, help with their work, and leave anything sensitive or destructive for the owner to confirm. Nobody here can answer permission prompts."
  "What a new Telegram group chat is told before the first message."
  'group 'telegram 'type 'string)

(defcustom 'telegram-group-replies #f
  "In a group, also answer a reply to one of the bot's own messages. Off, only an @mention reaches the agent."
  'group 'telegram 'type 'boolean)

(defcustom 'telegram-owner-brief
  "You are talking with the owner of this editor over Telegram. Each message below comes from them. Your final reply text is sent back to them as a Telegram message: keep it short, use plain Markdown (bold, code, links, lists) and no tables. They are away from the editor, so they cannot answer permission prompts here. A photo, file or voice note they send is saved on disk and its path is given to you."
  "What a new Telegram chat with the owner is told before the first message."
  'group 'telegram 'type 'string)

;;; ---------------------------------------------------------------- text

(effects! '(pure))

(define (telegram--api method)
  (string-append "https://api.telegram.org/bot" (secret-value 'telegram-bot-token) "/" method))

(define (telegram--allowed id)
  (assoc id telegram-allowed-users))

(define (telegram--brief id)
  (let ((entry (telegram--allowed id)))
    (if (and entry (> (length entry) 2) (eq? (nth 2 entry) 'owner))
        telegram-owner-brief
        telegram-chat-brief)))

(define (telegram--sender-name from)
  (string-trim (string-append (or (plist-get from 'first_name) "") " "
                              (or (plist-get from 'last_name) ""))))

(define (telegram--escape s)
  (string-replace (string-replace (string-replace s "&" "&amp;") "<" "&lt;") ">" "&gt;"))

;; prose outside code: headings, bullets, bold, italic, links
(define (telegram--prose s)
  (let* ((s (telegram--escape s))
         (s (re-replace-all "(?m)^#+ +(.+)$" s "<b>\\1</b>"))
         (s (re-replace-all "(?m)^[*-] +" s "• "))
         (s (re-replace-all "[*][*]([^*]+)[*][*]" s "<b>\\1</b>"))
         (s (re-replace-all "(^|[^*])[*]([^* ][^*]*)[*]([^*]|$)" s "\\1<i>\\2</i>\\3"))
         (s (re-replace-all "[[]([^]]+)[]][(](https?://[^) ]+)[)]" s "<a href=\"\\2\">\\1</a>")))
    s))

;; inline code: the odd pieces between backticks
(define (telegram--inline s)
  (let loop ((parts (string-split s "`")) (code? #f) (acc '()))
    (if (null? parts)
        (apply string-append (reverse acc))
        (loop (cdr parts) (not code?)
              (cons (if code?
                        (string-append "<code>" (telegram--escape (car parts)) "</code>")
                        (telegram--prose (car parts)))
                    acc)))))

;; fenced blocks: the odd pieces between ``` fences; the first line names the language
(define (telegram--html md)
  (let loop ((parts (string-split md "```")) (code? #f) (acc '()))
    (if (null? parts)
        (apply string-append (reverse acc))
        (loop (cdr parts) (not code?)
              (cons (if code?
                        (let* ((p (car parts))
                               (nl (string-index p #\newline))
                               (body (if (and nl (not (string-search " " (substring p 0 nl))))
                                         (substring p (+ nl 1) (string-length p))
                                         p)))
                          (string-append "<pre>" (telegram--escape (string-trim body)) "</pre>"))
                        (telegram--inline (car parts)))
                    acc)))))

;; pieces of at most 3500 characters, cut at paragraphs where it can
(define (telegram--chunks text)
  (if (<= (string-length text) 3500)
      (list text)
      (let* ((head (substring text 0 3500))
             (cut (let back ((i 3499))
                    (cond ((< i 1000) 3500)
                          ((and (char=? (string-ref head i) #\newline)
                                (char=? (string-ref head (- i 1)) #\newline)) i)
                          (else (back (- i 1)))))))
        (cons (substring text 0 cut)
              (telegram--chunks (string-trim (substring text cut (string-length text))))))))

(define (telegram--strip-mention text)
  (if *telegram-username*
      (string-trim (string-replace text (string-append "@" *telegram-username*) ""))
      text))

;;; ---------------------------------------------------------------- state

(effects! '(write external))

;; kept across a reload: a reload must not forget who waits, or start a
;; second poll beside the one that runs
(define *telegram-offset* (if (boundp '*telegram-offset*) *telegram-offset* 0))
(define *telegram-generation* (if (boundp '*telegram-generation*) *telegram-generation* 0))
(define *telegram-username* (if (boundp '*telegram-username*) *telegram-username* #f))  ; the bot's @name, from getMe
(define *telegram-bot-id* (if (boundp '*telegram-bot-id*) *telegram-bot-id* #f))
(define *telegram-pending* (if (boundp '*telegram-pending*) *telegram-pending* '()))  ; ((ID NAME USERNAME CODE CHAT-ID) ...)
(define *telegram-turns* (if (boundp '*telegram-turns*) *telegram-turns* '()))  ; ((SLUG chat-id N reply-to N status-id N steps (...)) ...)

(define (telegram--turn slug) (let ((e (assoc slug *telegram-turns*))) (and e (cdr e))))
(define (telegram--turn-set! slug plist)
  (set! *telegram-turns*
    (cons (cons slug plist) (filter (lambda (e) (not (equal? (car e) slug))) *telegram-turns*))))
(define (telegram--turn-drop! slug)
  (set! *telegram-turns* (filter (lambda (e) (not (equal? (car e) slug))) *telegram-turns*)))
(define (telegram--plist-put p k v)
  (cond ((null? p) (list k v))
        ((eq? (car p) k) (cons k (cons v (cddr p))))
        (else (cons (car p) (cons (cadr p) (telegram--plist-put (cddr p) k v))))))

;;; ---------------------------------------------------------------- the API

(define (telegram--call method body &optional k)
  (http-post (telegram--api method) body '()
             (lambda (reply) (when k (k (plist-get reply 'json))))))

(define (telegram-send! chat-id text &optional reply-to)
  (let loop ((parts (telegram--chunks (if (equal? (string-trim text) "") "(no reply)" text)))
             (first? #t))
    (when (pair? parts)
      (let* ((part (car parts))
             (base (append (list 'chat_id chat-id)
                           (if (and first? reply-to)
                               (list 'reply_parameters
                                     (list 'message_id reply-to 'allow_sending_without_reply #t))
                               '()))))
        (telegram--call "sendMessage"
          (append base (list 'text (telegram--html part) 'parse_mode "HTML"))
          (lambda (json)
            ;; Telegram refused the markup: the same text goes plain
            (unless (and json (plist-get json 'ok))
              (telegram--call "sendMessage" (append base (list 'text part))))
            (loop (cdr parts) #f)))))))
(public! 'telegram-send! "(telegram-send! CHAT-ID TEXT [REPLY-TO]) — send Markdown TEXT to a Telegram chat as the bot, formatted, as a reply to message REPLY-TO")

(define (telegram--typing! chat-id)
  (telegram--call "sendChatAction" (list 'chat_id chat-id 'action "typing")))

(define (telegram--react! chat-id message-id emoji)
  (telegram--call "setMessageReaction"
    (list 'chat_id chat-id 'message_id message-id
          'reaction (list (list 'type "emoji" 'emoji emoji)))))

;;; ---------------------------------------------------------------- chats

(define (telegram--chat-name key-name) (string-append "Telegram: " key-name))

;; the compos group that answers WHO: a route by id, then by name, then
;; the default route, else a group of its own
(define (telegram--route id name)
  (let ((hit (or (assoc id telegram-chat-routes)
                 (assoc name telegram-chat-routes)
                 (assoc 'default telegram-chat-routes))))
    (if hit (cadr hit) (telegram--chat-name name))))

(define (telegram-chat-for id name)
  (group-chat (telegram--route id name)))
(public! 'telegram-chat-for "(telegram-chat-for ID NAME) — the chat that answers Telegram user or group ID called NAME, by telegram-chat-routes")

(define (telegram-user-chat id)
  (let ((entry (telegram--allowed id)))
    (and entry (telegram-chat-for id (cadr entry)))))

;; After /new a Telegram conversation talks to a chat of its own in the
;; routed group, until the next /new or /main. A session holds the chat's
;; buffer until its first send, then its slug, which outlives the rename a
;; chat makes after its first prompt. Sessions are kept in the content
;; cache, so they outlive a restart.
(define *telegram-sessions*
  (if (boundp '*telegram-sessions*)
      *telegram-sessions*
      (let ((saved (cache-get 'telegram "sessions")))
        (if (string? saved)
            (map (lambda (p) (cons (plist-get p 'chat) (plist-get p 'key))) (or (json-parse saved) '()))
            '()))))

(define (telegram--sessions-save!)
  (cache-put! 'telegram "sessions"
    (json-encode (map (lambda (e) (list 'chat (car e) 'key (cdr e))) *telegram-sessions*))))

(define (telegram--session-set! chat-id key)
  (set! *telegram-sessions*
    (append (if key (list (cons chat-id key)) '())
            (filter (lambda (e) (not (equal? (car e) chat-id))) *telegram-sessions*)))
  (telegram--sessions-save!))

(define (telegram--session chat-id)
  "the live chat buffer of CHAT-ID's session, or #f"
  (let ((e (assoc chat-id *telegram-sessions*)))
    (and e (or (subagent-live-buffer (cdr e))
               (and (buffer-exists? (cdr e)) (cdr e))))))

(define (telegram--session-promote! chat-id slug)
  (let ((e (assoc chat-id *telegram-sessions*)))
    (when (and e slug (not (equal? (cdr e) slug)))
      (telegram--session-set! chat-id slug))))

;; a new chat in GROUP, made quietly: nothing shows it
(define (telegram--new-chat! group)
  ;; group-chat makes the group when the route names a new one
  (let ((id (begin (group-chat group) (group-resolve-id group))))
    (and id
         (let ((buf (group-chat-new-name id)))
           (buffer-create buf)
           (group-chat-init! buf id)
           (when (boundp 'llm-default-bundle-apply!) (llm-default-bundle-apply! buf id))
           (chat-set-group! buf id)
           (when (boundp 'workspace-chat-inherit!) (workspace-chat-inherit! buf (group-name id)))
           buf))))
(public! 'telegram-user-chat "(telegram-user-chat ID) — the agent chat of approved Telegram user ID, made when it has none")

(define (telegram--busy? buf)
  (and buf (buffer-local-value 'chat-turn-active buf) #t))

;;; ---------------------------------------------------------------- progress

;; every chat's activity line passes here; a chat with a Telegram turn
;; open shows its tool calls in Telegram too
(define *telegram-activity-base*
  (if (boundp '*telegram-activity-base*) *telegram-activity-base* chat-activity!))

(define (telegram--progress! buf label)
  (when (and (string? label) (string-prefix? "tool · " label))
    (let* ((slug (subagent-slug buf))
           (turn (and slug (telegram--turn slug))))
      (when turn
        (telegram--typing! (plist-get turn 'chat-id))
        (when telegram-progress
          (let ((steps (append (or (plist-get turn 'steps) '())
                               (list (substring label 7 (string-length label))))))
            (telegram--turn-set! slug (telegram--plist-put turn 'steps steps))
            (debounce! (string-append "telegram-progress:" slug) 1200
                       telegram--show-progress! slug)))))))

(define (telegram--progress-text steps)
  (let ((last (if (> (length steps) 5) (list-tail steps (- (length steps) 5)) steps)))
    (string-append "⏳ working…\n"
                   (string-join (map (lambda (s) (string-append "▸ " s)) last) "\n"))))

(define (telegram--show-progress! slug)
  (let ((turn (telegram--turn slug)))
    (when turn
      (let ((chat-id (plist-get turn 'chat-id))
            (text (telegram--progress-text (plist-get turn 'steps)))
            (sid (plist-get turn 'status-id)))
        (cond
          ((number? sid)
           (telegram--call "editMessageText" (list 'chat_id chat-id 'message_id sid 'text text)))
          ((eq? sid 'sending) #f)
          (else
           (telegram--turn-set! slug (telegram--plist-put turn 'status-id 'sending))
           (telegram--call "sendMessage"
             (list 'chat_id chat-id 'text text 'disable_notification #t)
             (lambda (json)
               (let ((id (and json (plist-get (plist-get json 'result) 'message_id)))
                     (now (telegram--turn slug)))
                 (cond ((not id) #f)
                       ;; the reply beat the progress message: it goes again
                       ((not now) (telegram--call "deleteMessage" (list 'chat_id chat-id 'message_id id)))
                       (else (telegram--turn-set! slug (telegram--plist-put now 'status-id id)))))))))))))

(set! chat-activity!
  (lambda (buf label)
    (*telegram-activity-base* buf label)
    (when (pair? *telegram-turns*)
      (report-errors "Telegram progress" (lambda () (telegram--progress! buf label))))))

;;; ---------------------------------------------------------------- turns

(define (telegram--finish! slug)
  (let ((turn (telegram--turn slug)))
    (when turn
      (telegram--turn-drop! slug)
      (debounce-cancel! (string-append "telegram-progress:" slug))
      (let ((chat-id (plist-get turn 'chat-id))
            (sid (plist-get turn 'status-id)))
        (when (number? sid)
          (telegram--call "deleteMessage" (list 'chat_id chat-id 'message_id sid)))
        (telegram-send! chat-id (or (subagent-last-assistant slug) "(no reply)")
                        (plist-get turn 'reply-to))
        ;; a message queued during the turn gets its own reply
        (when (telegram--busy? (subagent-live-buffer slug))
          (telegram--await! slug chat-id (plist-get turn 'reply-to)))))))

;; SLUG is the chat's stable id: a chat renames its buffer after its
;; first prompt, so everything here goes by slug, never by buffer name.
(define (telegram--await! slug chat-id reply-to)
  (let ((turn (telegram--turn slug)))
    (if turn
        (telegram--turn-set! slug (telegram--plist-put turn 'reply-to reply-to))
        (begin
          (telegram--turn-set! slug (list 'chat-id chat-id 'reply-to reply-to 'status-id #f 'steps '()))
          (set! *subagent-waiters*
            (cons (list (list slug) (list slug) (lambda (_) (telegram--finish! slug)))
                  *subagent-waiters*))))))

(define (telegram--to-agent! buf chat-id message-id prompt &optional user-id)
  (let ((prompt (if (null? (chat-record buf))
                    (string-append (if user-id (telegram--brief user-id) telegram-group-brief) "\n\n" prompt)
                    prompt)))
    (telegram--typing! chat-id)
    (telegram--react! chat-id message-id "👀")
    (with-current-buffer buf
      (lambda ()
        (chat-replace-input! buf prompt)
        (run-command "agent-send")))
    ;; the send starts the agent, and only then does the chat have its slug
    (let ((slug (subagent-slug buf)))
      (telegram--session-promote! chat-id slug)
      (telegram--await! slug chat-id message-id))))

;;; ---------------------------------------------------------------- media

(define (telegram--attachment msg)
  (let ((photo (plist-get msg 'photo)))
    (cond
      ((pair? photo)
       (list (plist-get (car (last-pair photo)) 'file_id) "photo" ".jpg"))
      ((plist-get msg 'document)
       => (lambda (d) (list (plist-get d 'file_id) "file"
                            (string-append "-" (or (plist-get d 'file_name) "file")))))
      ((plist-get msg 'voice) => (lambda (v) (list (plist-get v 'file_id) "voice note" ".ogg")))
      ((plist-get msg 'audio) => (lambda (v) (list (plist-get v 'file_id) "audio"
                                                    (string-append "-" (or (plist-get v 'file_name) "audio.mp3")))))
      ((plist-get msg 'video) => (lambda (v) (list (plist-get v 'file_id) "video" ".mp4")))
      (else #f))))

;; K gets the saved path, or #f
(define (telegram--fetch-file! file-id suffix k)
  (http-get (string-append (telegram--api "getFile") "?file_id=" file-id) '()
    (lambda (reply)
      (let* ((json (plist-get reply 'json))
             (path (and json (plist-get (plist-get json 'result) 'file_path))))
        (if (not path)
            (k #f)
            (let* ((dir (expand-file-name telegram-files-directory))
                   (dest (string-append dir (number->string (current-time)) suffix)))
              (make-directory! dir)
              (task-run!
                (lambda ()
                  (http-download! (string-append "https://api.telegram.org/file/bot"
                                                 (secret-value 'telegram-bot-token) "/" path)
                                  dest))
                (lambda (ok? v) (k (and ok? v dest))))))))))

;;; ---------------------------------------------------------------- commands

(define (telegram--command! cmd buf chat-id message-id group)
  (cond
    ((equal? cmd "/new")
     ;; the old chat stays as it is, for whoever else talks to it
     (let ((fresh (telegram--new-chat! group)))
       (if (not fresh)
           (telegram-send! chat-id "I could not start a new chat." message-id)
           (begin
             (telegram--session-set! chat-id fresh)
             (telegram-send! chat-id (string-append "Started a new chat in **" group "**. Your next message goes to it; /main goes back to the group's main chat.") message-id)))))
    ((equal? cmd "/main")
     (telegram--session-set! chat-id #f)
     (telegram-send! chat-id (string-append "Back to the main chat of **" group "**.") message-id))
    ((equal? cmd "/stop")
     (if (telegram--busy? buf)
         (begin (with-current-buffer buf (lambda () (run-command "chat-abort")))
                (telegram-send! chat-id "Stopped." message-id))
         (telegram-send! chat-id "Nothing is running." message-id)))
    ((equal? cmd "/status")
     (telegram-send! chat-id
       (string-append (if (telegram--busy? buf)
                          (string-append "Working: " (or (buffer-local-value 'chat-activity buf) "…"))
                          "Idle.")
                      "\nModel: " (or (buffer-local-value 'agent-model buf) "default"))
       message-id))
    (else
     (telegram-send! chat-id
       "Send a message and the agent answers. Photos, files and voice notes work too.\n\n/new — start a new chat\n/main — go back to the main chat\n/stop — stop the current answer\n/status — what the agent is doing"
       message-id))))

;;; ---------------------------------------------------------------- incoming

(define (telegram--pair! from chat-id)
  (let ((id (plist-get from 'id)))
    (let ((known (assoc id *telegram-pending*)))
      (if known
          (nth 3 known)
          (let ((code (number->string (+ 100000 (random 900000)))))
            (set! *telegram-pending*
              (cons (list id (telegram--sender-name from) (or (plist-get from 'username) "") code chat-id)
                    *telegram-pending*))
            (message (string-append "Telegram: " (telegram--sender-name from)
                                    " asks to pair, code " code " — M-x telegram-allow"))
            (event-publish! "telegram:pairing" 'received
                            (list 'from (telegram--sender-name from) 'id id 'code code))
            code)))))

(define (telegram--addressed? msg private?)
  (or private?
      (let ((text (or (plist-get msg 'text) (plist-get msg 'caption) ""))
            (reply (plist-get msg 'reply_to_message)))
        (or (and *telegram-username* (string-search (string-append "@" *telegram-username*) text))
            (and telegram-group-replies reply *telegram-bot-id*
                 (equal? (plist-get (plist-get reply 'from) 'id) *telegram-bot-id*))))))

(define (telegram--handle! update)
  (let* ((msg (plist-get update 'message))
         (from (and msg (plist-get msg 'from)))
         (chat (and msg (plist-get msg 'chat)))
         (chat-id (and chat (plist-get chat 'id)))
         (private? (and chat (equal? (plist-get chat 'type) "private"))))
    (when (and from chat-id (telegram--addressed? msg private?))
      (let* ((id (plist-get from 'id))
             (entry (telegram--allowed id))
             (message-id (plist-get msg 'message_id))
             (text (telegram--strip-mention (or (plist-get msg 'text) (plist-get msg 'caption) "")))
             (cmd (and (string-prefix? "/" text)
                       (car (string-split (car (string-split text " ")) "@")))))
        (cond
          ((not entry)
           (if private?
               (telegram-send! chat-id
                 (string-append "You're not paired yet. Your pairing code is **"
                                (telegram--pair! from chat-id)
                                "**. Ask to have it approved, and you'll get a message here."))
               (telegram-send! chat-id "Message me directly first to pair." message-id)))
          (else
           (let* ((group (if private?
                             (telegram--route id (cadr entry))
                             (telegram--route chat-id (or (plist-get chat 'title) "group"))))
                  (buf (or (telegram--session chat-id) (group-chat group)))
                  (who (if private? "" (string-append (cadr entry) ": ")))
                  (head (string-append "[Telegram, " (if private? (cadr entry) (or (plist-get chat 'title) "group")) "] ")))
             (cond
               ((equal? cmd "/start")
                (telegram-send! chat-id (string-append "Hi " (cadr entry) ", you're connected. Send me a message. /help shows what I can do.")))
               ((member cmd '("/new" "/reset" "/main" "/stop" "/status" "/help"))
                (telegram--command! (if (equal? cmd "/reset") "/new" cmd) buf chat-id message-id group))
               ((telegram--attachment msg)
                => (lambda (att)
                     (telegram--typing! chat-id)
                     (telegram--fetch-file! (car att) (nth 2 att)
                       (lambda (path)
                         (telegram--to-agent! buf chat-id message-id
                           (string-append head who
                             (if path
                                 (string-append "(sent a " (nth 1 att) ", saved at " path ")")
                                 (string-append "(sent a " (nth 1 att) " that could not be downloaded)"))
                             (if (equal? text "") "" (string-append " " text)))
                           (and private? id))))))
               ((not (equal? text ""))
                (telegram--to-agent! buf chat-id message-id (string-append head who text) (and private? id)))
               (else #f))
             (event-publish! (string-append "telegram:" (number->string chat-id)) 'received
                             (list 'from (cadr entry) 'text text)))))))))

(define (telegram--poll! gen)
  (when (and (= gen *telegram-generation*) (secret-set? 'telegram-bot-token))
    (http-get (string-append (telegram--api "getUpdates") "?timeout=30&offset="
                             (number->string *telegram-offset*))
              '(timeout 45000)
              (lambda (reply)
                (when (= gen *telegram-generation*)
                  (let ((json (plist-get reply 'json)))
                    (if (and json (plist-get json 'ok))
                        (begin
                          (for-each
                            (lambda (u)
                              (set! *telegram-offset* (+ 1 (plist-get u 'update_id)))
                              (report-errors "a Telegram message failed"
                                (lambda () (telegram--handle! u))))
                            (plist-get json 'result))
                          (telegram--poll! gen))
                        ;; the network or Telegram failed: try again in a while
                        (debounce! "telegram-retry" 10000 telegram--poll! gen))))))))

;;; ---------------------------------------------------------------- control

(define (telegram-start!)
  (set! *telegram-generation* (+ 1 *telegram-generation*))
  (if (not (secret-set? 'telegram-bot-token))
      (begin (message "Telegram: set telegram-bot-token first") #f)
      (let ((gen *telegram-generation*))
        (http-get (telegram--api "getMe") '()
          (lambda (reply)
            (let ((me (and (plist-get reply 'json) (plist-get (plist-get reply 'json) 'result))))
              (if (not me)
                  (message "Telegram: the bot token was refused")
                  (begin
                    (set! *telegram-username* (plist-get me 'username))
                    (set! *telegram-bot-id* (plist-get me 'id))
                    (telegram--call "setMyCommands"
                      (list 'commands (list (list 'command "new" 'description "Start a new chat")
                                            (list 'command "main" 'description "Back to the main chat")
                                            (list 'command "stop" 'description "Stop the current answer")
                                            (list 'command "status" 'description "What the agent is doing")
                                            (list 'command "help" 'description "What I can do"))))
                    (message (string-append "Telegram: @" *telegram-username* " is listening"))
                    (telegram--poll! gen))))))
        #t)))
(public! 'telegram-start! "(telegram-start!) — start the bot; #f when it has no token")

(define (telegram-stop!)
  (set! *telegram-generation* (+ 1 *telegram-generation*))
  (debounce-cancel! "telegram-retry")
  #t)
(public! 'telegram-stop! "(telegram-stop!) — stop the bot")

(define (telegram-allow! id name &optional role)
  (let ((pending (assoc id *telegram-pending*)))
    (customize-save! 'telegram-allowed-users
      (cons (if (eq? role 'owner) (list id name 'owner) (list id name))
            (filter (lambda (e) (not (equal? (car e) id))) telegram-allowed-users)))
    (set! *telegram-pending* (filter (lambda (p) (not (equal? (car p) id))) *telegram-pending*))
    (telegram-send! (if pending (nth 4 pending) id)
                    (string-append "Hi " name ", you're approved. Send me a message and the agent will answer. /help shows what I can do."))
    (telegram--allowed id)))
(public! 'telegram-allow! "(telegram-allow! ID NAME [ROLE]) — let Telegram user ID talk to the agent as NAME; ROLE 'owner gives the owner's brief")

(define (telegram-approve! code &optional role)
  (let ((p (find (lambda (p) (equal? (nth 3 p) code)) *telegram-pending*)))
    (and p (telegram-allow! (car p) (cadr p) role))))
(public! 'telegram-approve! "(telegram-approve! CODE [ROLE]) — approve the person whose pairing code is CODE, as ROLE ('owner or staff); #f when no one has it")

(define (telegram-revoke! id)
  (customize-save! 'telegram-allowed-users
    (filter (lambda (e) (not (equal? (car e) id))) telegram-allowed-users))
  #t)
(public! 'telegram-revoke! "(telegram-revoke! ID) — stop Telegram user ID from talking to the agent")

(effects! '(read))

(define (telegram-pending) *telegram-pending*)
(public! 'telegram-pending "(telegram-pending) — who waits for approval, as ((ID NAME USERNAME CODE CHAT-ID) ...)")

(define (telegram-running?) (and *telegram-username* #t))
(public! 'telegram-running? "(telegram-running?) — #t once the bot has started and knows its name")

(effects! '(write external))

(define-command "telegram-start" "Start the Telegram bot, now and at every boot"
  (lambda () (customize-save! 'telegram-enabled #t) (telegram-start!)))

(define-command "telegram-stop" "Stop the Telegram bot"
  (lambda () (customize-save! 'telegram-enabled #f) (telegram-stop!)))

(define-command "telegram-allow" "Approve a person who asked to pair with the bot"
  (lambda ()
    (if (null? *telegram-pending*)
        (message "Telegram: nobody is waiting")
        (let ((labels (map (lambda (p)
                             (string-append (cadr p)
                                            (if (equal? (nth 2 p) "") "" (string-append " @" (nth 2 p)))
                                            " · code " (nth 3 p)))
                           *telegram-pending*)))
          (completing-read "Approve: " labels
            (lambda (choice)
              (let loop ((ps *telegram-pending*) (ls labels))
                (when (pair? ps)
                  (if (equal? (car ls) choice)
                      (telegram-allow! (car (car ps)) (cadr (car ps)))
                      (loop (cdr ps) (cdr ls)))))))))))

;; at boot only: a reload keeps the poll that runs
(after-init (lambda () (when (and telegram-enabled (not *telegram-username*)) (telegram-start!))))

(load "telegram-app")
