(define-library (nstore)
  (export make close transactional everything ask add! rm! var var? var-name from where engine)

  (import (scheme base))
  (import (scheme list))
  (import (scheme mapping hash))
  (import (scheme generator))
  (import (only (okvstore) pack unpack))

  (begin

    ;; helper

    (define (assert v msg)
      (unless v
        (error 'nstore msg)))

    ;; make-indices will compute the minimum number of indices/tables
    ;; required to bind any pattern in one hop. The math behind this
    ;; computation is explained at:
    ;;
    ;;   https://math.stackexchange.com/q/3146568/23663
    ;;
    ;; make-indices will return the minimum list of permutations in
    ;; lexicographic order of the base index ie. (iota n) where n is
    ;; the length of ITEMS ie. the n in nstore.

    (define (permutation-prefix? c o)
      (any (lambda (p) (prefix? p o)) (permutations c)))

    (define (ok? combinations candidate)
      (every (lambda (c) (any (lambda (p) (permutation-prefix? c p)) candidate)) combinations))

    (define (findij L)
      (let loop3 ((x L)
                  (y '()))
        (if (or (null? x) (null? (cdr x)))
            (values #f (append (reverse! y) x) #f #f)
            (if (and (not (cdr (list-ref x 0))) (cdr (list-ref x 1)))
                (values #t
                        (append (cddr x) (reverse! y))
                        (car (list-ref x 0))
                        (car (list-ref x 1)))
                (loop3 (cdr x) (cons (car x) y))))))

    (define (bool v)
      (not (not v)))

    (define (lex< a b)
      (let loop ((a a)
                 (b b))
        (if (null? a)
            #t
            (if (not (= (car a) (car b)))
                (< (car a) (car b))
                (loop (cdr a) (cdr b))))))

    (define (make-indices n)
      ;; This is based on:
      ;;
      ;;   https://math.stackexchange.com/a/3146793/23663
      ;;
      (let* ((tab (iota n))
             (cx (combination (floor (/ n 2)) tab)))
        (let loop1 ((cx cx)
                    (out '()))
          (if (null? cx)
              (begin (assert (ok? (combinations tab) out))
                     (sort! lex< out))
              (let loop2 ((L (map (lambda (i) (cons i (bool (memv i (car cx))))) tab))
                          (a '())
                          (b '()))
                (call-with-values (lambda () (findij L))
                  (lambda (continue? L i j)
                    (if continue?
                        (loop2 L (cons j a) (cons i b))
                        (loop1 (cdr cx)
                               (cons (append (reverse! a) (map car L) (reverse! b))
                                     out))))))))))

    (define-record-type <engine>
      (engine database close begin! commit! rollback! ref set! rm! range)
      engine?
      (database engine-database)
      (close engine-close)
      (begin! engine-begin!)
      (commit! engine-commit!)
      (rollback! engine-rollback!)
      (ref engine-ref)
      (set! engine-set!)
      (rm! engine-rm!)
      (range engine-range))

    (define-record-type <nstore>
      (make-nstore engine indices n)
      nstore?
      (engine nstore-engine)
      (indices nstore-indices)
      (n nstore-n))

    (define (make engine . items)
      (make-nstore engine (make-indices (iota (length items))) (length items)))

    (define (close nstore)
      (let ((engine (nstore-engine nstore)))
        ((engine-close engine) (engine-database engine))))

    ;; XXX: I think that is the best course of action would be to
    ;; extend somehow the okvstore's transaction. For the time being
    ;; it is (only) wrapped by one more record type.

    ;; XXX: Maybe generics are the solution, so one might propose an
    ;; implementation that is forward compatible with them
    (define-record-type <transaction>
      (make-transaction transaction nstore)
      transaction?
      (transaction transaction-transaction)
      (nstore transaction-nstore))

    ;; transaction helpers

    (define (transaction-indices transaction)
      (nstore-indices (transaction-nstore transaction)))

    (define (transaction-n transaction)
      (nstore-n (transaction-nstore transaction)))

    (define (transaction-engine transaction)
      (nstore-engine (transaction-nstore transaction-nstore)))

    (define (transactional proc)
      (lambda (some . args)
        (cond
         ((nstore? some)
          (let* ((nstore some)
                 (engine (nstore-engine nstore))
                 (transaction ((engine-begin! engine) (engine-database engine))))
            ;; TODO: handle exceptions and rollback as necessary
            (call-with-values (lambda () (apply proc (make-transaction transaction nstore) args))
              (lambda out
                ((engine-commit!) transaction)
                (apply values out)))))
         ((transaction? some) (apply proc some args))
         (else (error 'nstore "the first argument must be nstore or transaction")))))

    (define everything
      (transactional
       (lambda (transaction)
         (let ((engine (nstore-engine nstore)))
           ;; Retrieve all tuples from the subspace 0 associated with
           ;; the base index (iota n) which index number is 0
           (gmap (lambda (pair) (cdr (unpack (car pair))))
                 ((engine-range nstore) (transaction-transaction transaction) (pack 0)))))))

    (define ask?
      (transactional
       (lambda (transaction . items)
         (assert (= (length items) (transaction-n transaction)))
         ;; indices are sorted in lexicographic order, that is the
         ;; first index is always (iota n) also known as the base
         ;; index. So that there is no need to permute ITEMS.
         (let ((key (apply pack (cons 0 items)))
               (engine (transaction-engine transaction)))
           (bool ((engine-ref engine) (transaction-transaction transaction) key))))))

    (define true (pack #t))

    (define (make-tuple list permutation)
      ;; Construct a permutation of LIST based on PERMUTATION
      (let ((tuple (make-vector (length index))))
        (for-each (lambda (x y) (vector-set! tuple x y)) permutation list)
        (vector->list tuple)))

    (define add!
      (transactional
       (lambda (transaction . items)
         (assert (= (length items) (transaction-n transaction)))
         (let ((engine (transaction-engine transaction)))
           ;; add ITEMS into the okvstore and prefix each of the
           ;; permutation of ITEMS with the index of the permutation
           ;; inside the list INDICES called SUBSPACE.
           (let loop ((indices (transaction-indices transaction))
                      (subspace 0))
             (let ((key (apply pack (cons subspace (make-tuple items (car indices))))))
               ((engine-add! engine) (transaction-transaction transaction) key true)
               (loop (cdr indices (+ subspace 1)))))))))

    (define rm!
      (transactional
       (lambda (transaction . items)
         (assert (= (length items) (transaction-n transaction)))
         (let ((engine (transaction-engine transaction)))
           ;; Similar as the above but remove ITEMS
           (let loop ((indices (transaction-indices transaction))
                      (subspace 0))
             (let ((key (apply pack (cons subspace (make-tuple items (car indices))))))
               ((engine-rm! engine) (transaction-transaction transaction) key)))))
               (loop (cdr indices (+ subspace 1)))))

    (define-record-type <var>
      (var name)
      var?
      (name var-name))

    (define (bind pattern tuple seed)
      ;; associate variables of PATTERN to value of TUPLE with SEED.
      (let loop ((tuple tuple)
                 (pattern pattern)
                 (out seed))
        (if (null? tuple)
            out
            (if (var? (car pattern)) ;; only bind variables
                (loop (cdr tuple)
                      (cdr pattern)
                      (hashmap-set out (var-name (car pattern)) (car tuple)))
                (loop (cdr tuple) (cdr pattern) out)))))

    (define (pattern->index pattern indices)
      ;; Retrieve the index that will allow to bind pattern in one
      ;; hop. This is done by getting all non-variable items of the
      ;; pattern and looking up the first index that is
      ;; permutation-prefix
      (let ((combination (filter (lambda (item) (not (var? item))) pattern)))
        (let loop ((indices indices)
                   (subspace 0))
          (if (null? indices)
              (error 'nstore "oops!")
              (if (permutation-prefix? combination (car indices))
                  (values subspace (car indices))
                  (loop (+ subspace 1) (cdr indices)))))))

    (define (pattern->prefix pattern index)
      ;; Return the list that correspond to INDEX, that is the items
      ;; of PATTERN that are not variables. This is used as the prefix
      ;; for the range query done later.
      (let loop ((index index)
                 (out '()))

        ;; TODO: not sure about the comment below

        ;; pattern has at least one variable, otherwise the query
        ;; would be calling ask? The way the index is constructed with
        ;; pattern->index, makes it so that when the code reach a
        ;; variable what remains is the empty list or more variables
        (let ((v (list-ref pattern (car index))))
          (if (var? v)
              (reverse! out)
              (loop (cdr index) (cons v out))))))

    (define (%from transaction pattern seed)
      (call-with-values (lambda () (pattern->index pattern (transaction-indices transaction)))
        (lambda (subspace index)
          (let ((prefix (cons subspace (pattern->prefix pattern index)))
                (engine (transaction-engine transaction)))
            (gmap (lambda (pair) (bind pattern
                                       (make-tuple (cdr (unpack (car pair))) index)
                                       seed))
                  ((engine-range nstore) transaction (apply pack prefix)))))))

    (define from
      (transactional
       (lambda (transaction . pattern)
         (assert (= (length items) (transaction-n transaction)))
         (%from transaction pattern (hashmap)))))

    (define (and=> v proc)
      (if v (proc v) #f))

    (define (pattern-bind pattern seed)
      ;; Return a pattern where variables that have a binding in seed
      ;; are replaced with the associated value. In pratice, most of
      ;; the time, it is the same pattern with less variables.
      (map (lambda (item) (or (and (var? item)
                                   (and=> (hashmap-ref/default seed (var-name item) #f) cdr))
                              item))
           pattern))

    (define (gscatter generator)
      ;; Returns a generator that yields the elements of the
      ;; generators produced by the given generator. Same as gflatten
      ;; but the generator contains other generators instead of lists.
      (error 'fixme "not implemented, yet"))

    (define where
      (transactional
       (lambda (transaction . pattern)
         (assert (= (length items) (transaction-n transaction)))
         (lambda (from)
           (gscatter
            (gmap (lambda (bindings) (%from transaction (pattern-bind pattern bindings) bindings))
                  from))))))

    ))
