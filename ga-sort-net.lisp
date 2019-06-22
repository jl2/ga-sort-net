;;;; ga-sort-net.lisp 
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :ga-sort-net)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                 collect (symb 'a i))
       ,(funcall
         (get-macro-character #\`) stream nil)))

  (set-dispatch-macro-character
   #\# #\` #'|#`-reader|))

(defun make-batcher-sn (n)
  (let* (network
         (tee (ceiling (log n 2)))
         (p (ash 1 (- tee 1))))
    (loop while (> p 0) do
         (let ((q (ash 1 (- tee 1)))
               (r 0)
               (d p))
           (loop while (> d 0) do
                (loop for i from 0 to (- n d 1) do
                     (when (= (logand i p) r)
                         (push (cons i (+ i d)) network)))
                (setf d (- q p)
                      q (ash q -1)
                      r p)))
         (setf p (ash p -1)))
    (nreverse network)))

(defun interpret-sorting-network (data sn predicate)
  ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type list data sn)
           (type function predicate))
  (let ((step 0)
        (swaps 0))
    (declare (type fixnum step swaps))
    (dolist (i sn)
      (declare (type cons i))
      (when (funcall predicate
             #1=(nth (car i) data)
             #2=(nth (cdr i) data))
        (rotatef #1# #2#)
        (incf swaps)))
    (incf step)
    (values swaps data)))
                
(defun sn-to-lambda-form% (sn)
  `(lambda (arr)
     (declare (optimize (speed 3) (safety 0) (debug 0)))
     (declare (type (simple-array fixnum) arr))
     ,@(mapcar 
        #`(when (> #1=(aref arr ,(car a1))
                   #2=(aref arr ,(cdr a1)))
            (rotatef #1# #2#))
        sn)
     arr))

;; (defun create-sorter (network)
;;   (compile "sorter" (sn-to-lambda-form% network)))

(defun choose (n r)
  (/ (alexandria:factorial n)
     (alexandria:factorial (- n r))
     (alexandria:factorial r)))

(defun make-random-sort-network (size)
  (declare (ignorable size))
  (loop for i below (choose size 2) collecting (cons (random size) (random size))))


(defun show-sort-network (network file-name &key (width 1200)
                                              (height 1200)
                                              (image-type "BMP")
                                              (border 20.0)
                                              (box-size 10.0)
                                              (index-color #16rff00ee00)
                                              (compare-color #16rffee0000)
                                              (background-color #16rffdddddd)
                                              (box-color #16rff0000ee)
                                              (line-width 1.0))
  "Draw a wire diagram view of network in the specified file."
  (ensure-directories-exist file-name)
  (when (uiop/filesystem:file-exists-p (home-dir file-name))
    (delete-file (home-dir file-name)))

  (bl:with-image-context*
      (img ctx (format nil "~a" (home-dir file-name))
           :width width
           :height height
           :codec-name image-type)
      ((rect blll:round-rect)
       (line bl:line))
    (let* ((count (1+ (apply #'max 1 (alexandria:flatten network))))
           (x-min border)
           (x-max (- width border))
           
           (compare-start (+ x-min border))
           (compare-end (- x-max border))

           (half-box (/ box-size 2.0))

           (x-step (/ (- compare-end compare-start)
                      (length network)
                      1.0))

           (y-min border)
           (y-max (- height border))
           (y-step (/ (- y-max y-min)
                      (1- count)
                      1.0)))
      (declare (ignorable y-max))
      (bl:context-set-fill-style-rgba32 ctx background-color)
      (bl:context-fill-all ctx)

      (format t "count ~a y-step ~a ~%" count y-step)
      (bl:context-set-stroke-width ctx  line-width)
      (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
      (bl:context-set-stroke-style-rgba32 ctx index-color)
      ;; Draw horizontal lines, representing array indices
      (loop
         for i below count
         for yp from y-min by y-step 
         do
           (format t "line ~a (~a ~a) to (~a ~a)~%" i x-min yp x-max yp)

           (setf (bl:line.x0 line) x-min)
           (setf (bl:line.y0 line) yp)

           (setf (bl:line.x1 line) x-max)
           (setf (bl:line.y1 line) yp)
           (bl:context-stroke-geometry ctx bl:+geometry-type-line+ line))

      ;; Draw vertical lines with boxes on each end point.
      (loop
         for entry in network
         for xp from compare-start by x-step
         do
           (let ((start-x xp)
                 (start-y (+ y-min (* y-step (car entry))))
                 (end-x xp)
                 (end-y (+ y-min (* y-step (cdr entry)))))
             
             (bl:context-set-stroke-style-rgba32 ctx compare-color)
             (setf (bl:line.x0 line) start-x)
             (setf (bl:line.y0 line) start-y)
             (setf (bl:line.x1 line) end-x)
             (setf (bl:line.y1 line) end-y)
             (bl:context-stroke-geometry ctx bl:+geometry-type-line+ line)

             ;; (bl:context-set-stroke-style-rgba32 ctx box-color)
             (setf (bl:round-rect.x rect) (- start-x half-box))
             (setf (bl:round-rect.y rect) (- start-y half-box))
             (setf (bl:round-rect.w rect) box-size)
             (setf (bl:round-rect.h rect) box-size)
             (setf (bl:round-rect.rx rect) 0.5)
             (setf (bl:round-rect.ry rect) 0.5)
             (bl:context-set-fill-style-rgba32 ctx box-color)
             #+sbcl(sb-int:with-float-traps-masked (:invalid) (bl:context-fill-geometry ctx bl:+geometry-type-round-rect+ rect))

             ;; (bl:context-set-stroke-style-rgba32 ctx box-color)
             (setf (bl:round-rect.x rect) (- end-x half-box))
             (setf (bl:round-rect.y rect) (- end-y half-box))
             (setf (bl:round-rect.w rect) box-size)
             (setf (bl:round-rect.h rect) box-size)
             (setf (bl:round-rect.rx rect) 0.5)
             (setf (bl:round-rect.ry rect) 0.5)
             (bl:context-set-fill-style-rgba32 ctx box-color)
             #+sbcl(sb-int:with-float-traps-masked (:invalid) (bl:context-fill-geometry ctx bl:+geometry-type-round-rect+ rect)))))))

(defun evaluate-sort-network (size network &key (iterations 1) (predicate #'<))
  (loop for i below iterations summing 
       (let* ((data (loop for i below size collecting (random 100))))
         (sn:interpret-sorting-network data network predicate)
         (loop for (a b) on data by #'cdr
            when (and b a (not (funcall predicate b a))) sum 1))))

(defun mixup (net1 net2)
  (loop for a in net1
     for b in net2
     collecting (if (< (random 1.0) 0.5)
                    a
                    b)))

(defun mutate-networks (networks keep-count)
  (declare (ignorable keep-count))
  (let ((new-nets (loop for net in networks
                     for i below keep-count
                     collecting net)))
    (loop for i below (- (length networks) keep-count) do
         (push (mixup (alexandria:random-elt new-nets :start 0 :end keep-count)
                      (alexandria:random-elt new-nets :start 0 :end keep-count))
               new-nets))
    new-nets))

(defun evaluate-many (size networks eval-iterations)
  (sort 
   (mapcar (lambda (net)
            (cons
             (evaluate-sort-network size net :iterations eval-iterations)
             net))
           networks)
   #'< :key #'car))

(defun ga-sort-networks (count size &key (keep-count 10) (iterations 1000) (eval-iterations 100))
  (let ((networks (loop for i below count
                     collecting (make-random-sort-network size))))
    (loop for i below iterations do
         (setf networks (mutate-networks
                         (mapcar #'cdr (evaluate-many size networks eval-iterations))
                         keep-count)))
    (evaluate-many size networks eval-iterations)))
