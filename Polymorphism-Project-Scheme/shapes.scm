(load "box.scm")
(load "cylinder.scm")
(load "sphere.scm")
(load "torus.scm")

(define (sendMessage message object . arguments)
  (let ((method (object message)))
    (apply method arguments)
  )
)

(define (printShapes shapes conditions)
  (for-each (lambda(shape)
              (if (sendMessage 'test shape conditions)
                (sendMessage 'getinfo shape)
              )
            ) shapes)
  (newline)
)

(define (countValidShapes shapes conditions)
  (let* ((count 0))
    (for-each (lambda(shape)
              (if (sendMessage 'test shape conditions)
                (set! count (+ count 1))
              )
            ) shapes)
    count
  )
)

; Given code from lectures.
(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
              ((char=? ch (string-ref str b)) (if (= a b)
                (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
                  (split 0 0))))


(define (processShapeFromFile lineFromFile)

    ; This call to str-split will return a list of substrings
    ; Now we can use this list to find all the attributes
    ; of a shape and create our "class"
    (define substrings(str-split lineFromFile #\space))

    ;(display "Sub strings: ")
    ;(display substrings)
    ;(newline)
    (define nameOfShape (first substrings))
    ;(display "Name of shape: ")
    ;(display nameOfShape)
    (define actualshape (second substrings))
    ;(display " Actual shape: ")
    ;(display actualshape)
    ;(newline)

    (cond 
    
        ((string=? actualshape "box")

         ;(display "Its a box!")
         ;(newline)
         ; Now load the box file so we have access to the createbox function.
         ;(load "box.scm")
         ; Here I am declaring all of the parameters that I need
         ; in order to call my box "class" constructor.
         (let* ((lengthAsString (third substrings))
                (lengthOfBox (string->number lengthAsString))
                (widthAsString (fourth substrings))
                (widthOfBox (string->number widthAsString))
                (heightAsString (fifth substrings))
                (heightOfBox (string->number heightAsString))
               )
               (createbox nameOfShape lengthOfBox widthOfBox heightOfBox)
         )

        )
        ((string=? actualshape "cylinder")

         ;(display "Its a cylinder!")
         ;(newline)

         (let* ((radiusAsString (third substrings))
                (radiusOfCylinder (string->number radiusAsString))
                (heightAsString (fourth substrings))
                (heightOfCylinder (string->number heightAsString))
               )
               (createcylinder nameOfShape radiusOfCylinder heightOfCylinder)
         )

        )
        ((string=? actualshape "sphere")

         ;(display "Its a sphere!")
         ;(newline)

         (let* ((radiusAsString (third substrings))
                (radiusOfCylinder (string->number radiusAsString))
               )
               (createsphere nameOfShape radiusOfCylinder)
         )

        )
        ((string=? actualshape "torus")

         ;(display "Its a torus!")
         ;(newline)

         (let* ((small_radiusAsString (third substrings))
                (small_radiusOfTorus (string->number small_radiusAsString))
                (big_radiusAsString (fourth substrings))
                (big_radiusOfTorus (string->number big_radiusAsString))
               )
               (createtorus nameOfShape small_radiusOfTorus big_radiusOfTorus)
         )
        )
    )
)


; This readFrom function just reads each line
; of the file and calls processShapeFromFile.
(define (readFrom port)
  (do ((lineFromFile (read-line port) (read-line port))
        (shapes '()))
       ((eof-object? lineFromFile) shapes)
    (set! shapes (append shapes (list (processShapeFromFile lineFromFile)))))
)

; Defining the function perform that takes an action, a file, 
; and then a variable amount of arguments.
(define (perform action file . arguments)

    (define lengthOfArguments (length arguments))
    (define isValidNumberOfArguments? (modulo lengthOfArguments 3))

    (cond 
    
        ; If the file that was passed to perform is not the shapes.dat file then this means we won't be able to read the file.
        ((not(file-exists? file)) 
         (display (string-append "Unable to open " file " for reading.")) 
         (newline)
         (newline))
        
        ; If isValidNumberOfArguments? does not equal 0 then this means that we have the wrong number of arguments being passed to our perform function.
        ((not (= isValidNumberOfArguments? 0)) 
         (display "Incorrect number of arguments.") 
         (newline)
         (newline))

        ; If we reach this point then it means we have the correct
        ; file and we have the correct number of arguments.
        ; So now we can start reading the shapes file and gathering shapes.
        (else 

            (let* ((port (open-input-file file))
                   (shapes (readFrom port))
                   (count (countValidShapes shapes arguments))
                  )

                ; Now we can deal with whichever action the user specified.

                (cond 

                    ((string=? action "count")
                     (display (string-append "There are " (number->string count) " shapes.\n\n"))
                    )
                    ((string=? action "print")
                     (if (= count 0)
                      (display "There are no shapes satisfying the condition(s)\n")
                     )
                     (printShapes shapes arguments)
                    )
                    ((string=? action "min")
                     (if (= count 0)
                      (display "There are no shapes satisfying the condition(s)\n")
                     )
                     (let* ((area 0)
                            (volume 0)
                            (area_min 999999999)
                            (volume_min 999999999))
                      (for-each (lambda(shape)
                                    (if (sendMessage 'test shape arguments)
                                      (begin 
                                        (set! area (sendMessage 'getarea shape))
                                        (set! volume (sendMessage 'getvolume shape))
                                        ;(display (string-append "Is area_min: " (number->string area_min) " < area: " (number->string area) "\n"))
                                        (set! area_min (min area_min area))
                                        ;(display (string-append "area_min: " (number->string area_min) "\n"))
                                        ;(display (string-append "Is volume_min: " (number->string volume_min) " < volume: " (number->string volume) "\n"))
                                        (set! volume_min (min volume_min volume))
                                        ;(display (string-append "volume_min: " (number->string volume_min) "\n"))
                                      )
                                    )
                                  ) shapes)
                        (display (string-append "min(Surface Area)=" (number->string area_min) "\n"))
                        (display (string-append "min(Volume)=" (number->string volume_min) "\n\n"))
                      )
                    )
                    ((string=? action "max")
                     (if (= count 0)
                      (display "There are no shapes satisfying the condition(s)\n")
                     )
                     (let* ((area 0)
                            (volume 0)
                            (area_max -1)
                            (volume_max -1))
                      (for-each (lambda(shape)
                                    (if (sendMessage 'test shape arguments)
                                      (begin 
                                        (set! area (sendMessage 'getarea shape))
                                        (set! volume (sendMessage 'getvolume shape))
                                        ;(display (string-append "Is area_max: " (number->string area_max) " < area: " (number->string area) "\n"))
                                        (set! area_max (max area_max area))
                                        ;(display (string-append "area_max: " (number->string area_max) "\n"))
                                        ;(display (string-append "Is volume_max: " (number->string volume_max) " < volume: " (number->string volume) "\n"))
                                        (set! volume_max (max volume_max volume))
                                        ;(display (string-append "volume_max: " (number->string volume_max) "\n"))
                                      )
                                    )
                                  ) shapes)
                        (display (string-append "max(Surface Area)=" (number->string area_max) "\n"))
                        (display (string-append "max(Volume)=" (number->string volume_max) "\n\n"))
                      )
                    )
                    ((or (string=? action "total") (string=? action "avg"))
                     (if (= count 0)
                      (display "There are no shapes satisfying the condition(s)\n")
                     )
                     (let* ((area_total 0)
                            (volume_total 0))
                          (for-each (lambda(shape)
                                    (if (sendMessage 'test shape arguments)
                                      (begin 
                                        (set! area_total (+ area_total (sendMessage 'getarea shape)))
                                        (set! volume_total (+ volume_total (sendMessage 'getvolume shape)))
                                      )
                                    )
                                  ) shapes)
                          (if (string=? action "total")
                            ; if the action is total then we print out the total of the area and volume
                            ; however if the action is avg then we print out the avg of the area and volume.
                            (begin
                              (display (string-append "total(Surface Area)=" (number->string area_total) "\n"))
                              (display (string-append "total(Volume)=" (number->string volume_total) "\n\n"))
                            )
                            (begin
                              (display (string-append "avg(Surface Area)=" (number->string (/ area_total count)) "\n"))
                              (display (string-append "avg(Volume)=" (number->string (/ volume_total count)) "\n\n"))
                            )
                          )
                     )
                    )
                    (else 
                     (display "Invalid action. Please enter count, print, min, max, total, or avg")
                     (newline)
                    )
                )
                (close-input-port port)
            )
        )
    )
)
