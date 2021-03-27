(in-package :cl-stun.precis)

(defvar *exceptions-map*
  '(
    ;; PVALID -- Would otherwise have been DISALLOWED

    (#\U+00DF . :pvalid) ;; LATIN SMALL LETTER SHARP S
    (#\U+03C2 . :pvalid) ;; GREEK SMALL LETTER FINAL SIGMA
    (#\U+06FD . :pvalid) ;; ARABIC SIGN SINDHI AMPERSAND
    (#\U+06FE . :pvalid) ;; ARABIC SIGN SINDHI POSTPOSITION MEN
    (#\U+0F0B . :pvalid) ;; TIBETAN MARK INTERSYLLABIC TSHEG
    (#\U+3007 . :pvalid) ;; IDEOGRAPHIC NUMBER ZERO

    ;; CONTEXTO -- Would otherwise have been DISALLOWED

    (#\U+00B7 . :contexto) ;; MIDDLE DOT
    (#\U+0375 . :contexto) ;; GREEK LOWER NUMERAL SIGN (KERAIA)
    (#\U+05F3 . :contexto) ;; HEBREW PUNCTUATION GERESH
    (#\U+05F4 . :contexto) ;; HEBREW PUNCTUATION GERSHAYIM
    (#\U+30FB . :contexto) ;; KATAKANA MIDDLE DOT

    ;; CONTEXTO -- Would otherwise have been PVALID

    (#\U+0660 . :contexto) ;; ARABIC-INDIC DIGIT ZERO
    (#\U+0661 . :contexto) ;; ARABIC-INDIC DIGIT ONE
    (#\U+0662 . :contexto) ;; ARABIC-INDIC DIGIT TWO
    (#\U+0663 . :contexto) ;; ARABIC-INDIC DIGIT THREE
    (#\U+0664 . :contexto) ;; ARABIC-INDIC DIGIT FOUR
    (#\U+0665 . :contexto) ;; ARABIC-INDIC DIGIT FIVE
    (#\U+0666 . :contexto) ;; ARABIC-INDIC DIGIT SIX
    (#\U+0667 . :contexto) ;; ARABIC-INDIC DIGIT SEVEN
    (#\U+0668 . :contexto) ;; ARABIC-INDIC DIGIT EIGHT
    (#\U+0669 . :contexto) ;; ARABIC-INDIC DIGIT NINE
    (#\U+06F0 . :contexto) ;; EXTENDED ARABIC-INDIC DIGIT ZERO
    (#\U+06F1 . :contexto) ;; EXTENDED ARABIC-INDIC DIGIT ONE
    (#\U+06F2 . :contexto) ;; EXTENDED ARABIC-INDIC DIGIT TWO
    (#\U+06F3 . :contexto) ;; EXTENDED ARABIC-INDIC DIGIT THREE
    (#\U+06F4 . :contexto) ;; EXTENDED ARABIC-INDIC DIGIT FOUR
    (#\U+06F5 . :contexto) ;; EXTENDED ARABIC-INDIC DIGIT FIVE
    (#\U+06F6 . :contexto) ;; EXTENDED ARABIC-INDIC DIGIT SIX
    (#\U+06F7 . :contexto) ;; EXTENDED ARABIC-INDIC DIGIT SEVEN
    (#\U+06F8 . :contexto) ;; EXTENDED ARABIC-INDIC DIGIT EIGHT
    (#\U+06F9 . :contexto) ;; EXTENDED ARABIC-INDIC DIGIT NINE

    ;; DISALLOWED -- Would otherwise have been PVALID

    (#\U+0640 . :disallowed) ;; ARABIC TATWEEL
    (#\U+07FA . :disallowed) ;; NKO LAJANYALAN
    (#\U+302E . :disallowed) ;; HANGUL SINGLE DOT TONE MARK
    (#\U+302F . :disallowed) ;; HANGUL DOUBLE DOT TONE MARK
    (#\U+3031 . :disallowed) ;; VERTICAL KANA REPEAT MARK
    (#\U+3032 . :disallowed) ;; VERTICAL KANA REPEAT WITH VOICED SOUND MARK
    (#\U+3033 . :disallowed) ;; VERTICAL KANA REPEAT MARK UPPER HALF
    (#\U+3034 . :disallowed) ;; VERTICAL KANA REPEAT WITH VOICED SOUND MARK UPPER HA
    (#\U+3035 . :disallowed) ;; VERTICAL KANA REPEAT MARK LOWER HALF
    (#\U+303B . :disallowed) ;; VERTICAL IDEOGRAPHIC ITERATION MARK
    ))

(defun letter-digits-p (char)
  "PRECIS LetterDigits (A) category"
  (find (general-category char)
        '("Ll" "Lu" "Lo" "Nd" "Lm" "Mn" "Mc")
	:test #'string=))

(defun exceptions-p (char)
  "PRECIS Exceptions (F) category"
  (assoc char *exceptions-map*))

(defun exceptions (char)
  (cdr (assoc char *exceptions-map*)))

(defun backward-compatible-p (char)
  "PRECIS BackwardCompatible (G) category"
  (declare (ignore char))
  nil)

(defun backward-compatible (char)
  "backward compat handler"
  ;; should not be called
  (declare (ignore char))
  :pvalid)

(defun join-control-p (char)
  "PRECIS JoinControl (H) category"
  (has-property char "Join_Control"))

(defun old-hangul-jamo-p (char)
  "PRECIS OldHangulJamo (I) category"
  (hangul-syllable-lvt-p char))

(defun unassigned-p (char)
  "PRECIS Unassigned (J) category"
  (and (string= (general-category char) "Cn")
       (not (has-property char "Noncharacter_Code_Point"))))

(defun ascii7-p (char)
  "PRECIS ASCII7 (K) category"
  (let ((code (char-code char)))
    (and (>= code #x21)
         (<= code #x7E))))

(defun controls-p (char)
  "PRECIS Controls (L) category"
  (has-property char "Control"))

(defun precis-ignorable-properties-p (char)
  "PRECIS PrecisIgnorableProperties (M) category"
  (or (has-property char "Default_Ignorable_Code_Point")
      (has-property char "Noncharacter_Code_Point")))

(defun spaces-p (char)
  "PRECIS Spaces (N) category"
  (string= (general-category char) "Zs"))

(defun symbols-p (char)
  "PRECIS Symbol (O) category"
  (find (general-category char)
        '("Sm" "Sc" "Sk" "So")
	:test #'string=))

(defun punctuation-p (char)
  "PRECIS Punctuation (P) category"
  (find (general-category char)
        '("Pc" "Pd" "Ps" "Pe" "Pi" "Pf" "Po")
	:test #'string=))

(defun has-compat-p (char)
  "PRECIS HasCompat (Q) category"
  (not (equalp (list (char-code char))
	       (normalization-form-k-c char))))

(defun other-letter-digits-p (char)
  "PRECIS OtherLetterDigits (R) category"
  (find (general-category char)
        '("Lt" "Nl" "No" "Me")
	:test #'string=))

(macrolet
    ((precis-class-helper (class code-point)
       (with-gensyms (code-point-s)
	 `(let ((,code-point-s ,code-point))
	    (cond ((exceptions-p ,code-point-s)
		   (exceptions ,code-point-s))
		  ((backward-compatible-p ,code-point-s)
		   (backward-compatible ,code-point-s))
		  ((unassigned-p ,code-point-s) :unassigned)
		  ((ascii7-p ,code-point-s) :pvalid)
		  ((join-control-p ,code-point-s) :contextj)
		  ((old-hangul-jamo-p ,code-point-s) :disallowed)
		  ((precis-ignorable-properties-p ,code-point-s) :disallowed)
		  ((controls-p ,code-point-s) :disallowed)
		  ((has-compat-p ,code-point-s)
		   ,(ecase class
		      (:identifier :disallowed)
		      (:freeform :pvalid)))
		  ((letter-digits-p ,code-point-s) :pvalid)
		  ((other-letter-digits-p ,code-point-s)
		   ,(ecase class
		      (:identifier :disallowed)
		      (:freeform :pvalid)))
		  ((spaces-p ,code-point-s)
		   ,(ecase class
		      (:identifier :disallowed)
		      (:freeform :pvalid)))
		  ((symbols-p ,code-point-s)
		   ,(ecase class
		      (:identifier :disallowed)
		      (:freeform :pvalid)))
		  ((punctuation-p ,code-point-s)
		   ,(ecase class
		      (:identifier :disallowed)
		      (:freeform :pvalid)))
		  (t :disallowed)))))
     (string-loop (string)
       (with-gensyms (string-s)
	 `(let ((,string-s ,string))
	    (dotimes (i (length ,string-s))
	      (if (eql
		   ;(precis-class-helper :freeform (elt i ,string-s))
		   :disallowed)
		  (error "disallowed char at ~a" i)
		  t))))))
  ;(defun identifier-class-p (string)    string)
  (defun freeform-class-p (string)
    (dotimes (i (length string) t)
      (if
       (eql (precis-class-helper :freeform (elt string i))
	    :disallowed)
       (error "disallowed char at ~a" i)
       t))))
