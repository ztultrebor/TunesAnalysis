;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tunes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)
(require 2htdp/batch-io)


; data definitions


; An LSongs is one of:
; – '()
; – (cons Song LSongs)
#;(define (is-lsongs? ls)
    (or
     (empty? ls)
     (and
      (is-song? (first ls))
      (is-lsongs? (rest ls)))))


(define-struct song [name artist album time song# added play# played])
; A Song is a structure:
;   (make-song String String String Natural Natural DateTime Natural DateTime)
; interpretation An instance records in order: the song's  title,its producing
; artist, to which album it belongs, its playing time in milliseconds, its
; position within the album, the date-time it was added, how often it has been
; played, and the date-time when it was last played
#;(define (is-song? s)
    (and
     (song? s)
     (string? (song-name s))
     (string? (song-artist s))
     (string? (song-album s))
     (is-natural? (song-time s))
     (is-natural? (song-song# s))
     (is-date-time? (song-added s))
     (s-natural? (song-play# s))
     (is-date-time? (song-played s))))


(define-struct date-time [year month day hour minute second])
; A DateTime is a structure:
;   (make-date-time-time Natural Natural Natural Natural Natural Natural)
; interpretation An instance records six pieces of information:
; the date-time's year, month (between 1 and 12 inclusive),
; day (between 1 and 31), hour (between 0 and 23), minute (between 0 and 59),
; and second (also between 0 and 59).
(define (is-date-time? dt)
  (and
   (date-time? dt)
   (is-natural? (date-time-year dt))
   (is-natural? (date-time-month dt))
   (is-natural? (date-time-day dt))
   (is-natural? (date-time-hour dt))
   (is-natural? (date-time-minute dt))
   (is-natural? (date-time-second dt))))
; checks
(check-expect (is-date-time? (make-date-time 2023 12 19 12 35 33)) #t)
(check-expect (is-date-time? (make-date-time 2023 12 19 -12 35 33)) #f)
(check-expect (is-date-time? (make-date-time 2023.0 12 19 12 35 33)) #t)
(check-expect (is-date-time? (make-date-time 2023 12 19 12 35 "now")) #f)
(check-expect (is-date-time? 2023) #f)
#;
(define (fn-on-date-time dt)
  (cond
    [(not (is-date-time? dt)) (error "not a DateTime")]
    [else ... (fn-on-natural (date-time-year dt))
          ... (fn-on-natural (date-time-month dt))
          ... (fn-on-natural (date-time-day dt))
          ... (fn-on-natural (date-time-hour dt))
          ... (fn-on-natural (date-time-minute dt))
          ... (fn-on-natural (date-time-second dt))]))


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

(define (create-song name artist album time song# added play# played)
  ; Any Any Any Any Any Any Any Any -> Song or #false
  ; creates an instance of Song for legitimate inputs,
  ; otherwise it produces #false
  ...)


(define (create-date-time y mo day h m s)
  ; Any Any Any Any Any Any -> DateTime or #false
  ; creates an instance of DateTime for legitimate inputs 
  ; otherwise it produces #false
  ...)


#(define (read-itunes-as-songs file-name)
   ; String -> LSongs
   ; creates a list-of-songs representation from the
   ; text in file-name (an XML export from iTunes)
   ...)


; constants

(define ITUNES-LOCATION "Library.xml")
; (define itunes-songs (read-itunes-as-songs ITUNES-LOCATION))





;actions!

