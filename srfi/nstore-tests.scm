(import (scheme base)
        (scheme generator)
        (cffi wiredtiger okvs)
        (srfi :64)
        (arew)
        (prefix (arew filename) filename:)
        (scheme process-context)
        (scheme generator)
        (scheme comparator)
        (scheme mapping hash)
        (cffi wiredtiger nstore))


(define-syntax-rule (with-directory name body ...)
  (begin
    (when (filename:exists? name)
      (filename:delete name))
    (filename:make name)
    (let ((out (begin body ...)))
      (filename:delete name)
      out)))

(define-syntax-rule (test-check name expected computed)
  (test-equal name expected (with-directory "wt" computed)))

(test-begin "wiredtiger-nstore")

(define (triplestore)
  (let ((engine (nstore-engine okvs-ref okvs-set! okvs-rm! okvs-prefix)))
    (nstore engine (list 42) '(uid key value))))

(test-check "ask empty triplestore"
  #f
  (let ((okvs (okvs '((home . "wt") (create? . #t))))
        (triplestore (triplestore)))
    ;; ask
    (let ((out (nstore-ask? okvs triplestore '("P4X432" blog/title "hyper.dev"))))
      (okvs-close okvs)
      out)))

(test-check "add and ask triplestore"
  #t
  (let ((okvs (okvs '((home . "wt") (create? . #t))))
        (triplestore (triplestore)))
    ;; add
    (nstore-add! okvs triplestore '("P4X432" blog/title "hyper.dev"))
    ;; ask
    (let ((out (nstore-ask? okvs triplestore '("P4X432" blog/title "hyper.dev"))))
      (okvs-close okvs)
      out)))

(test-check "add, rm and ask triplestore"
  #f
  (let ((okvs (okvs '((home . "wt") (create? . #t))))
        (triplestore (triplestore)))
    ;; add!
    (nstore-add! okvs triplestore '("P4X432" blog/title "hyper.dev"))
    ;; remove!
    (nstore-rm! okvs triplestore '("P4X432" blog/title "hyper.dev"))
    ;; ask
    (let ((out (nstore-ask? okvs triplestore '("P4X432" blog/title "hyper.dev"))))
      (okvs-close okvs)
      out)))

(test-check "blog query post titles"
  '("DIY a database" "DIY a full-text search engine")

  (let ((okvs (okvs '((home . "wt") (create? . #t))))
        (triplestore (triplestore)))
    ;; add hyper.dev blog posts
    (nstore-add! okvs triplestore '("P4X432" blog/title "hyper.dev"))
    (nstore-add! okvs triplestore '("123456" post/title "DIY a database"))
    (nstore-add! okvs triplestore '("123456" post/blog "P4X432"))
    (nstore-add! okvs triplestore '("654321" post/title "DIY a full-text search engine"))
    (nstore-add! okvs triplestore '("654321" post/blog "P4X432"))
    ;; add dthompson.us blog posts
    (nstore-add! okvs triplestore '("1" blog/title "dthompson.us"))
    (nstore-add! okvs triplestore '("2" post/title "Haunt 0.2.4 released"))
    (nstore-add! okvs triplestore '("2" post/blog "1"))
    (nstore-add! okvs triplestore '("3" post/title "Haunt 0.2.3 released"))
    (nstore-add! okvs triplestore '("3" post/blog "1"))
    ;; query
    (let ()
      (define query
        (okvs-transactional
         (lambda (transaction blog/title)
           (generator->list (nstore-select
                             (nstore-from transaction triplestore
                                          (list (nstore-var 'blog/uid)
                                                'blog/title
                                                blog/title))
                             (nstore-where transaction triplestore
                                           (list (nstore-var 'post/uid)
                                                 'post/blog
                                                 (nstore-var 'blog/uid)))
                             (nstore-where transaction triplestore
                                           (list (nstore-var 'post/uid)
                                                 'post/title
                                                 (nstore-var 'post/title))))))))
      (let* ((out (query okvs "hyper.dev"))
             (out (map (lambda (x) (hashmap-ref x 'post/title)) out)))
        (okvs-close okvs)
        out))))

(test-check "nstore-from limit and offset"
  '("hyperdev.fr")
  (let ((okvs (okvs '((home . "wt") (create? . #t))))
        (triplestore (triplestore)))
    ;; add!
    (nstore-add! okvs triplestore '("P4X432" blog/title "hyper.dev"))
    (nstore-add! okvs triplestore '("P4X433" blog/title "hyperdev.fr"))
    (nstore-add! okvs triplestore '("P4X434" blog/title "hypermove.net"))
    ((okvs-transactional
     (lambda (transaction)
       (generator-map->list
        (lambda (item) (hashmap-ref item 'title))
        (nstore-from transaction triplestore (list (nstore-var 'uid)
                                            'blog/title
                                            (nstore-var 'title))
                     `((limit . 1) (offset . 1))))))
     okvs)))

(test-end)


(define xpass (test-runner-xpass-count (test-runner-current)))
(define fail (test-runner-fail-count (test-runner-current)))
(if (and (= xpass 0) (= fail 0))
    (exit 0)
    (exit 1))
