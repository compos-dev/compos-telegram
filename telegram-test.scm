;;; telegram-test.scm --- the Telegram bot's text, routing and gating, and the app's rows.

(package! "telegram-test")

(domain! 'diagnostics)
(effects! '(read))

(deftest 'telegram-markdown-becomes-telegram-html
  "A reply's Markdown goes out as the HTML Telegram reads."
  (lambda ()
    (check-equal! (telegram--html "**bold** and *it* & <x>")
                  "<b>bold</b> and <i>it</i> &amp; &lt;x&gt;"
                  "bold, italic, and the characters HTML reserves")
    (check-equal! (telegram--html "## Plan\n- one")
                  "<b>Plan</b>\n• one"
                  "a heading turns bold and a dash a bullet")
    (check-equal! (telegram--html "see `a<b` [site](https://x.com/a?b=1)")
                  "see <code>a&lt;b</code> <a href=\"https://x.com/a?b=1\">site</a>"
                  "inline code is escaped, a link keeps its address")
    (check-equal! (telegram--html "```scheme\n(+ 1 2)\n```")
                  "<pre>(+ 1 2)</pre>"
                  "a fence drops its language line")))

(deftest 'telegram-long-replies-split-at-paragraphs
  "A reply over Telegram's limit goes in pieces, cut between paragraphs."
  (lambda ()
    (let ((parts (telegram--chunks (string-append (make-string 3000 #\a) "\n\n" (make-string 2000 #\b)))))
      (check-equal! (length parts) 2 "two pieces")
      (check-equal! (string-ref (cadr parts) 0) #\b "the second piece starts at the second paragraph"))
    (check-equal! (telegram--chunks "short") (list "short") "a short reply is one piece")))

(deftest 'telegram-routes-pick-the-answering-group
  "A route by id wins over one by name, then the default, then a group of its own."
  (lambda ()
    (let ((saved telegram-chat-routes))
      (set! telegram-chat-routes '((42 "by-id") ("Ana" "by-name") (default "fallback")))
      (let ((r (list (telegram--route 42 "Ana") (telegram--route 7 "Ana") (telegram--route 7 "Bo"))))
        (set! telegram-chat-routes '())
        (let ((own (telegram--route 7 "Bo")))
          (set! telegram-chat-routes saved)
          (check-equal! r (list "by-id" "by-name" "fallback") "id, then name, then default")
          (check-equal! own "Telegram: Bo" "unrouted, a group of its own"))))))

(deftest 'telegram-groups-answer-only-mentions
  "In a group only an @mention reaches the agent; a direct message always does."
  (lambda ()
    (let ((name *telegram-username*) (id *telegram-bot-id*) (replies telegram-group-replies))
      (set! *telegram-username* "TestBot")
      (set! *telegram-bot-id* 1)
      (set! telegram-group-replies #f)
      (let ((r (list (and (telegram--addressed? '(text "hi @TestBot") #f) #t)
                     (and (telegram--addressed? '(text "chatter") #f) #t)
                     (and (telegram--addressed? '(text "ok" reply_to_message (from (id 1))) #f) #t)
                     (and (telegram--addressed? '(text "hello") #t) #t)
                     (telegram--strip-mention "@TestBot do x"))))
        (set! *telegram-username* name)
        (set! *telegram-bot-id* id)
        (set! telegram-group-replies replies)
        (check-equal! r (list #t #f #f #t "do x")
                      "a mention passes, chatter and a reply do not, a direct message does; the mention is cut")))))

(deftest 'telegram-app-labels-media-and-events
  "A message without text shows what it holds."
  (lambda ()
    (check-equal! (telegram--media-label '(media (type "event" name "ContactSignUp"))) "[joined Telegram]" "a service message")
    (check-equal! (telegram--media-label '(media (type "photo"))) "[photo]" "a photo")
    (check-equal! (telegram--message-text '(text "look" media (type "photo"))) "[photo] look" "a caption follows its media")
    (check-equal! (telegram--preview '(last (out #t text "a\nb"))) "you: a b" "the preview is one line and marks your own")))
