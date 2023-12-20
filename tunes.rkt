;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tunes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)
(require 2htdp/batch-io)


; data definitions

; An LTracks is one of:
; – '()
; – (cons Track LTracks)
(define (is-ltracks? ls)
  (or
   (empty? ls)
   (and
    (is-track? (first ls))
    (is-ltracks? (rest ls)))))
; checks
(check-expect (is-ltracks? '()) #t)
(check-expect (is-ltracks? TUNES) #t)
(check-expect (is-ltracks? (list IAM
                                 (create-date 1976 12 19 12 35 33))) #f)
#;
(define (fn-on-ltracks ltr)
  (cond
    [(not (is-ltracks? ltr)) (error "not a ListOfTracks")]
    [(empty? ltr) ...]
    [else (fn-on-track (first ltr)) ... (fn-on-ltracks (rest ltr))]))


; A Track is a structure:
;   (make-track String String String Natural Natural Date Natural Date)
; interpretation An instance records in order: the Track's  title,its producing
; artist, to which album it belongs, its playing time in milliseconds, its
; position within the album, the date it was added, how often it has been
; played, and the date when it was last played
(define (is-track? tr)
  (and
   (track? tr)
   (string? (track-name tr))
   (string? (track-artist tr))
   (string? (track-album tr))
   (is-natural? (track-time tr))
   (is-natural? (track-track# tr))
   (is-date? (track-added tr))
   (is-natural? (track-play# tr))
   (is-date? (track-played tr))))
; checks
(check-expect (is-track? IAM) #t)
(check-expect (is-track? (create-track 77 "Budd" "Take It!" 34567 9
                                       MILLENNIUM 567 TODAY)) #f)
(check-expect (is-track? (create-track  "I am" "Budd" "Take It!" 34567 9
                                        19761222 567 TODAY)) #f)
#;
(define (fn-on-track tr)
  (cond
    [(not (is-track? tr)) (error "not a Track")]
    [else ... (fn-on-string (track-name tr))
          ... (fn-on-string (track-artist tr))
          ... (fn-on-string (track-album tr))
          ... (fn-on-natural (track-time tr))
          ... (fn-on-natural (track-track# tr))
          ... (fn-on-date (track-added tr))
          ... (fn-on-natural (track-play# tr))
          ... (fn-on-date (track-played tr))]))


; A Date is a structure:
;   (create-date Natural Natural Natural Natural Natural Natural)
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive),
; day (between 1 and 31), hour (between 0 and 23), minute (between 0 and 59),
; and second (also between 0 and 59).
(define (is-date? d)
  (and
   (date? d)
   (is-natural? (date-year d))
   (is-natural? (date-month d))
   (is-natural? (date-day d))
   (is-natural? (date-hour d))
   (is-natural? (date-minute d))
   (is-natural? (date-second d))))
; checks
(check-expect (is-date?  TODAY) #t)
(check-expect (is-date? (create-date 2023 12 19 -12 35 33)) #f)
(check-expect (is-date? (create-date 2023.0 12 19 12 35 33)) #t)
(check-expect (is-date? (create-date 2023 12 19 12 35 "now")) #f)
(check-expect (is-date? 2023) #f)
#;
(define (fn-on-date d)
  (cond
    [(not (is-date? d)) (error "not a Date")]
    [else ... (fn-on-natural (date-year d))
          ... (fn-on-natural (date-month d))
          ... (fn-on-natural (date-day d))
          ... (fn-on-natural (date-hour d))
          ... (fn-on-natural (date-minute d))
          ... (fn-on-natural (date-second d))]))


; An Natural is one of:
; – 0
; – (add1 Natural)
; interpretation represents the counting numbers
(define (is-natural? n)
  (and
   (real? n)
   (or
    (= n 0)
    (and 
     (>= n 0)
     (is-natural? (sub1 n))))))
; checks
(check-expect (is-natural? 0) #t)
(check-expect (is-natural? 2) #t)
(check-expect (is-natural? pi) #f)
(check-expect (is-natural? -2) #f)
(check-expect (is-natural? 0+2i) #f)
#;
(define (fn-on-natural n)
  (cond
    [(not (is-natural? n)) (error "not a natural number")]
    [(= n 0) ...]
    [else ... (fn-on-natural (sub1 n))]))


; functions on structure-based representations


(define (total-time ltr)
  ; ListOfTracks -> Natural
  ; computes the total amount of time spent listening to a music library.
  ; Multiply number of plays by track length for each Track in the
  ; ListOfTracks and suns the total
  (cond
    [(empty? ltr) 0]
    [else (+ (total-track-time (first ltr)) (total-time (rest ltr)))]))
; checks
(check-expect (total-time TUNES)
              (+ (* (track-time IAM) (track-play# IAM))
                 (* (track-time ZUZZAH) (track-play# ZUZZAH))
                 (* (track-time THANKYE) (track-play# THANKYE))
                 (* (track-time MYRULES) (track-play# MYRULES))))


(define (select-all-album-titles ltr)
  ; ListOfTracks -> ListOfStrings
  ; generates a list of all album titles in the ListOfTracks
  (cond
    [(empty? ltr) '()]
    [else (push-to-set (track-album (first ltr))
                       (select-all-album-titles (rest ltr)))]))
; checks
(check-satisfied (list (select-all-album-titles TUNES)
                       (list "Take It!" "Rizz Monsters")) same-set?)


(define (select-album album ltr)
  ; String ListOfTracks -> ListOfTracks
  ; given an album title, retreive all that album's tracks from ListOfTracks
  (cond
    [(empty? ltr) '()]
    [(equal? (track-album (first ltr)) album)
     (cons (first ltr) (select-album album (rest ltr)))]
    [else  (select-album album (rest ltr))]))
; checks
(check-satisfied (list (select-album "Take It!" TUNES) TAKEIT) same-set?)


(define (select-album-date album date ltr)
  ; String Date ListOfTracks -> ListOfTracks
  ; given an album title and a date retreive all that album's tracks
  ; from ListOfTracks that have been played since Date
  (cond
    [(empty? ltr) '()]
    [(and (equal? (track-album (first ltr)) album)
          (since? (track-played (first ltr)) date))
     (cons (first ltr) (select-album-date album date (rest ltr)))]
    [else  (select-album-date album date (rest ltr))]))
; checks
(check-satisfied (list (select-album-date "Take It!" RANDOMDATE TUNES)
                       (list IAM THANKYE)) same-set?)


(define (select-albums ltr)
  ; ListOfTracks -> ListOfListOfTracks
  ; assembles a ListOfListOfTracks associated with a given music collection,
  ;      one per album
  (construct-album-track-list (select-all-album-titles ltr) ltr))
; checks
(check-satisfied (list (select-albums TUNES)
                       (list TAKEIT RIZZMONSTERS)) same-set?)


(define (total-track-time tr)
  ; Track -> Natural
  ; computes the total amount of time spent listening to a given track.
  ; Multiply number of plays by track length
  (*  (track-time tr) (track-play# tr)))
;checks
(check-expect (total-track-time IAM) (* (track-time IAM) (track-play# IAM)))


(define (push-to-set ele lst)
  ; Any ListOfAny -> ListOfAny
  ; adds an element to a set; a list in which no items repeat.
  ; assumes the list provided qualifies as a set
  (cond
    [(empty? lst) (list ele)]
    [(equal? ele (first lst)) lst]
    [else (cons (first lst) (push-to-set ele (rest lst)))]))
;checks
(check-expect (push-to-set "c" (list "a" "b")) (list "a" "b" "c"))
(check-expect (push-to-set "c" (list "a" "b" "c")) (list "a" "b" "c"))


(define (create-set lst1 lst2)
  ; Any ListOfAny -> ListOfAny
  ; merges two lists into a set; a list in which no items repeat.
  ; assumes the both lists provided qualify as sets
  (cond
    [(empty? lst1) lst2]
    [else (push-to-set (first lst1) (create-set (rest lst1) lst2))]))
;checks
(check-satisfied (list (create-set (list "a" "b" "c") (list "a" "b" "c"))
                       (list "a" "b" "c")) same-set?)
(check-satisfied (list (create-set (list "a" "b" "c") (list "d" "e" "f"))
                       (list "a" "b" "c" "d" "e" "f")) same-set?)
(check-satisfied (list (create-set (list "a" "b" "e" "c") (list "d" "e" "f"))
                       (list "a" "b" "c" "d" "e" "f")) same-set?)


(define (equivalent? s1 s2)
  ; ListOfAny ListOfAny -> Boolean
  ; returns #true if two lists are setwise equivalent, else #false
  (and (= (length s1) (length s2))
       (equal? s2 (create-set s1 s2))
       (equal? s1 (create-set s2 s1))))
;checks
(check-expect (equivalent? (list "a" "b" "c") (list "a" "b" "c")) #t)
(check-expect (equivalent? (list "a" "c" "b") (list "a" "b" "c")) #t)
(check-expect (equivalent? (list "a" "b") (list "a" "b" "c")) #f)
(check-expect (equivalent? (list "a" "b" "d") (list "a" "b" "c")) #f)
(check-expect (equivalent? (list "c" "b" "d") (list "a" "d" "b")) #f)


(define (same-set? los)
  ; ListOfSets -> Boolean
  ; variant of equivalent? for use in check-satisfied
  (equivalent? (first los) (second los)))


(define (since? date1 date2)
  ; Date Date -> Boolean
  ; returns #true if date1 occurs strictly after date2, else #false
  (or
   (> (date-year date1) (date-year date2))
   (and
    (= (date-year date1) (date-year date2))
    (or
     (> (date-month date1) (date-month date2))
     (and
      (= (date-month date1) (date-month date2))
      (or
       (> (date-day date1) (date-day date2))
       (and
        (= (date-day date1) (date-day date2))
        (or
         (> (date-hour date1) (date-hour date2))
         (and
          (= (date-hour date1) (date-hour date2))
          (or
           (> (date-minute date1) (date-minute date2))
           (and
            (= (date-minute date1) (date-minute date2))
            (> (date-second date1) (date-second date2)))))))))))))
(check-expect (since? TODAY MILLENNIUM) #t)
(check-expect (since? TODAY EARLIERTODAY) #t)
(check-expect (since? MILLENNIUM TODAY) #f)
(check-expect (since? EARLIERTODAY TODAY) #f)

  
(define (construct-album-track-list loati ltr)
  ; ListOfStrings ListOfTracks -> ListOfListOfTracks
  ; using a list of album titles, construct a ListOfListOfTracks,
  ; with a ListOfTracks for each album
  (cond
    [(empty? loati) '()]
    [else (cons (select-album (first loati) ltr)
                (construct-album-track-list (rest loati) ltr))]))
; checks
(check-satisfied (list (construct-album-track-list
                        (list "Rizz Monsters" "Take It!") TUNES)
                       (list TAKEIT RIZZMONSTERS)) same-set?)


; functions on list-based representations


(define (total-time/lists ll)
  ; ListOfLists -> Natural
  ; computes the total amount of time spent listening to a music library.
  ; Multiply number of plays by track length for each track in the
  ; ListOfLists and sums the total
  (cond
    [(empty? ll) 0]
    [else (+ (total-track-time/lists (first ll)) (total-time/lists (rest ll)))]))


(define (boolean-attributes ll)
  ; ListOfLists -> ListOfStrings
  ; constructs a list of all the association headers in the
  ; list-representation that are associated with boolean values.
  ; Output is effectively a set
  (cond
    [(empty? ll) '()]
    [else (create-set (parse-list (first ll))
                      (boolean-attributes (rest ll)))]))
;checks
(check-satisfied (list (boolean-attributes LISTOFLASSOC)
                       (list "tweed" "diamondite")) same-set?)


(define (track-as-struct lassoc)
  ; ListOfAssociations -> Track
  ; takes a track in ListOfAssociations format and converts it into
  ; a Track structure, if possible
  (create-track (find-association "Name" lassoc #f)
                (find-association "Artist" lassoc #f)
                (find-association "Album" lassoc #f)
                (find-association "Total Time" lassoc #f)
                (find-association "Track Number" lassoc #f)
                (find-association "Date Added" lassoc #f)
                (find-association "Play Count" lassoc #f)
                (find-association "Date Modified" lassoc #f)))
; checks
(check-expect (track-as-struct MYRULESLASSOC) MYRULES)
(check-expect (track-as-struct SALLYSUELASSOC) #f)


(define (llassoc-to-ltr llassoc)
  ; ListOfListOfAssociations -> ListOfTracks
  ; takes a track library in ListOfListOfAssociations structure
  ; and converts it into a ListOfTracks structure
  (cond
    [(empty? llassoc) '()]
    [(boolean? (track-as-struct (first llassoc)))
     (llassoc-to-ltr (rest llassoc))]
    [else (cons (track-as-struct (first llassoc))
                (llassoc-to-ltr (rest llassoc)))]))
; checks
(check-satisfied (list (llassoc-to-ltr LLASSOC) (list MYRULES)) same-set?)


(define (total-track-time/lists l)
  ; List -> Natural
  ; computes the total amount of time spent listening to a given track.
  ; Multiply number of plays by track length
  (*  (find-association "Total Time" l 0) (find-association "Play Count" l 0)))


(define (find-association key lassoc default)
  ; String ListOfAssociations Any -> Association/Any
  ; returns an association if the key matches an
  ; association header, else default
  (cond
    [(empty? lassoc) default]
    [(equal? (first (first lassoc)) key) (second (first lassoc))]
    [else (find-association key (rest lassoc) default)]))
; checks
(check-expect (find-association "tweed" LASSOC #f) #f)
(check-expect (find-association "stain" LASSOC #f) TODAY)
(check-expect (find-association "philobust" LASSOC "wut") "wut")


(define (parse-list l)
  ; ListOfAssociations -> ListOfStrings
  ; constructs a list of all the association headers from a given track that
  ; are associated with boolean values.
  (cond
    [(empty? l) '()]
    [(boolean? (second (first l)))
     (cons (first (first l)) (parse-list (rest l)))]
    [else (parse-list (rest l))]))



; constants for structure-based representations

(define ITUNES-LOCATION "Library.xml")
(define TODAY (create-date 2023 12 19 12 35 33))
(define EARLIERTODAY (create-date 2023 12 19 12 35 13))
(define MILLENNIUM (create-date 2000 01 01 00 00 00))
(define RANDOMDATE (create-date 2015 05 22 17 32 57))
(define IAM (create-track "I am" "Budd" "Take It!"
                          34567 9 MILLENNIUM 567 TODAY))
(define ZUZZAH (create-track "Zuzzah!" "Budd" "Take It!"
                             144567 12 MILLENNIUM 1 MILLENNIUM))
(define THANKYE (create-track "Thank Ye" "Budd" "Take It!"
                              9778 13 MILLENNIUM 177 TODAY))
(define MYRULES (create-track "My Rules" "J17" "Rizz Monsters"
                              10098 4 TODAY 76 TODAY))
(define TAKEIT (list IAM ZUZZAH THANKYE))
(define RIZZMONSTERS (list MYRULES))
(define TUNES (list IAM ZUZZAH THANKYE MYRULES))

; constants for list-based representations

(define LASSOC (list (list "tweed" #f)
                     (list "herringbone" 77)
                     (list "diamondite" "fake!")
                     (list "stain" TODAY)
                     (list "tweed" "sunny")))
(define LISTOFLASSOC (list (list (list "tweed" #f)
                                 (list "herringbone" 77)
                                 (list "diamondite" "fake!")
                                 (list "stain" TODAY)
                                 (list "tweed" "sunny"))
                           (list (list "tweed" #f)
                                 (list "herringbone" 77)
                                 (list "diamondite" #t)
                                 (list "stain" TODAY)
                                 (list "tweed" "sunny"))))
(define MYRULESLASSOC (list (list "Name" "My Rules")
                            (list "Artist" "J17")
                            (list "Album" "Rizz Monsters")
                            (list "Total Time" 10098)
                            (list "Track Number" 4)
                            (list "Date Added" TODAY)
                            (list "Play Count" 76)
                            (list "Date Modified" TODAY)))
(define SALLYSUELASSOC (list (list "Name" "Sally Sue")
                            (list "Artist" "Budd")
                            (list "Album" "Take It!")
                            (list "Total Time" 22345)
                            (list "Track Number" 8)
                            (list "Date Added" MILLENNIUM)))
(define LLASSOC (list MYRULESLASSOC SALLYSUELASSOC))



;actions!

;(define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))
;(total-time itunes-tracks)
;(define albums (select-all-album-titles itunes-tracks))
;(length albums)
; (select-album  "The Queen Is Dead" itunes-tracks)
;(select-albums itunes-tracks)

(define itunes-tracks/lists (read-itunes-as-lists ITUNES-LOCATION))
;(total-time/lists itunes-tracks/lists)
;(boolean-attributes itunes-tracks/lists)
(llassoc-to-ltr itunes-tracks/lists)
