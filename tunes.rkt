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
(check-expect (is-ltracks? TAKEIT) #t)
(check-expect (is-ltracks? (list IAM
                         (create-date 1976 12 19 12 35 33))) #f)
#;
(define (fn-on-ltracks sl)
  (cond
    [(not (is-ltracks? ls)) (error "not a ListOfTracks")]
    [(empty? ls) ...]
     [else (fn-on-track (first ls)) ... (fn-on-ltracks (rest ls))]))


; A Track is a structure:
;   (make-track String String String Natural Natural Date Natural Date)
; interpretation An instance records in order: the Track's  title,its producing
; artist, to which album it belongs, its playing time in milliseconds, its
; position within the album, the date it was added, how often it has been
; played, and the date when it was last played
(define (is-track? s)
  (and
   (track? s)
   (string? (track-name s))
   (string? (track-artist s))
   (string? (track-album s))
   (is-natural? (track-time s))
   (is-natural? (track-track# s))
   (is-date? (track-added s))
   (is-natural? (track-play# s))
   (is-date? (track-played s))))
; checks
(check-expect (is-track? IAM) #t)
(check-expect (is-track? (create-track 77 "Budd" "Take It!" 34567 9
                                   MILLENNIUM 567 TODAY)) #f)
(check-expect (is-track? (create-track  "I am" "Budd" "Take It!" 34567 9
                                    19761222 567 TODAY)) #f)
#;
(define (fn-on-track s)
  (cond
    [(not (is-track? s)) (error "not a Track")]
    [else ... (fn-on-string (track-name s))
          ... (fn-on-string (track-artist s))
          ... (fn-on-string (track-album s))
          ... (fn-on-natural (track-time s))
          ... (fn-on-natural (track-track# s))
          ... (fn-on-date (track-added s))
          ... (fn-on-natural (track-play# s))
          ... (fn-on-date (track-played s))]))


; A Date is a structure:
;   (create-date Natural Natural Natural Natural Natural Natural)
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive),
; day (between 1 and 31), hour (between 0 and 23), minute (between 0 and 59),
; and second (also between 0 and 59).
(define (is-date? dt)
  (and
   (date? dt)
   (is-natural? (date-year dt))
   (is-natural? (date-month dt))
   (is-natural? (date-day dt))
   (is-natural? (date-hour dt))
   (is-natural? (date-minute dt))
   (is-natural? (date-second dt))))
; checks
(check-expect (is-date?  TODAY) #t)
(check-expect (is-date? (create-date 2023 12 19 -12 35 33)) #f)
(check-expect (is-date? (create-date 2023.0 12 19 12 35 33)) #t)
(check-expect (is-date? (create-date 2023 12 19 12 35 "now")) #f)
(check-expect (is-date? 2023) #f)
#;
(define (fn-on-date dt)
  (cond
    [(not (is-date? dt)) (error "not a Date")]
    [else ... (fn-on-natural (date-year dt))
          ... (fn-on-natural (date-month dt))
          ... (fn-on-natural (date-day dt))
          ... (fn-on-natural (date-hour dt))
          ... (fn-on-natural (date-minute dt))
          ... (fn-on-natural (date-second dt))]))


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
(define TAKEIT (list IAM ZUZZAH THANKYE))



;actions!

(define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))

itunes-tracks