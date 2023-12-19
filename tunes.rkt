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


; functions

(define (total-track-time tr)
  ; Track -> Natural
  ; computes the total amount of time spent listening to a given track.
  ; Multiply number of plays by track length
  (*  (track-time tr) (track-play# tr)))
;checks
(check-expect (total-track-time IAM) (* (track-time IAM) (track-play# IAM)))


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


(define (equivalent? s1 s2)
  ; ListOfAny ListOfAny -> Boolean
  ; determines if two lists are in fact the same set
  (and (equal? s2 (create-set s1 s2)) (equal? s1 (create-set s2 s1))))
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
  


; constants

(define ITUNES-LOCATION "Library.xml")
(define TODAY (create-date 2023 12 19 12 35 33))
(define MILLENNIUM (create-date 2000 01 01 00 00 00))
(define IAM (create-track "I am" "Budd" "Take It!"
                          34567 9 MILLENNIUM 567 TODAY))
(define ZUZZAH (create-track "Zuzzah!" "Budd" "Take It!"
                             144567 12 MILLENNIUM 1 MILLENNIUM))
(define THANKYE (create-track "Thank Ye" "Budd" "Take It!"
                              9778 13 MILLENNIUM 177 TODAY))
(define MYRULES (create-track "My Rules" "J17" "Rizz Monsters"
                              10098 4 TODAY 76 TODAY))
(define TUNES (list IAM ZUZZAH THANKYE MYRULES))



;actions!

(define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))

(total-time itunes-tracks)

(select-all-album-titles itunes-tracks)