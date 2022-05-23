#lang racket

(require net/url)
(require data-science-master)
(require csv-reading)
(require plot)
(require math)
;locating and initialising Data source
(define next-row
  (make-csv-reader
   (open-input-file "C:/Users/Admin/Downloads/Uganda_sampletweets.csv")
   '((separator-chars   #\,)
     (strip-leading-whitespace? . #t)
     (strip-trailing-whitespace? . #t))))

;converting row to a list
(define row-lst
  (csv->list next-row))
(define full-listings (append (car row-lst) (cdr row-lst)))

;convert list to strings
(define list-string
  (~a full-listings))

;extract unique word and its count.
(define words(document->tokens list-string #:sort? #t))

;crosscheck with the lexicons
(define sentiment-nrc (list->sentiment words #:lexicon 'nrc))

;aggregate sum/count of sentiment
(aggregate sum ($ sentiment-nrc 'sentiment) ($ sentiment-nrc 'freq))

;Visualising in a plot
(let ([counts (aggregate sum ($ sentiment-nrc 'sentiment) ($ sentiment-nrc 'freq))])
  (parameterize ((plot-width 800))
                (plot (list
                       (tick-grid)
                       (discrete-histogram
                        (sort counts (λ (x y) (> (second x) (second y))))
                        #:color "Green"
                        #:line-color "Blue"))
                      #:x-label "Sentiment Label"
                      #:y-label "number of times")))
;crosscheck with lexison of  bing
(define sentiment-bing (list->sentiment words #:lexicon 'bing))

;aggregate sum/count
(aggregate sum ($ sentiment-bing 'sentiment) ($ sentiment-bing 'freq))

;visualising in a plot
(let ([counts (aggregate sum ($ sentiment-bing 'sentiment) ($ sentiment-bing 'freq))])
  (parameterize ((plot-width 400))
                (plot (list
                       (tick-grid)
                       (discrete-histogram
                        (sort counts (λ (p q) (> (second p) (second q))))
                        #:color "Green"
                        #:line-color "Blue"))
                      #:x-label "Sentiment Label"
                      #:y-label "number of times")))
                                       
