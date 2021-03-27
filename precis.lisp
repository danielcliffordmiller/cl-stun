(in-package :cl)

(defpackage cl-stun.precis
  (:use
   :cl
   :cl-stun
   :alexandria
   :sb-unicode))

(in-package :cl-stun.precis)

(defun letter-digit-p (char)
  "PRECIS LetterDigits (A) category"
  (find (general-category char)
        '(:ll :lu :lo :nd :lm :mn :mc)))

(defun ascii7-p (char)
  "PRECIS ASCII7 (K) category"
  (let ((code (char-code char)))
    (and (>= code #x21)
         (<= code #x7E))))

(defun exceptions-p (char)
  "PRECIS Exceptions (F) category"
  (find char
        '(#\U+00B7 #\U+00DF #\U+0375 #\U+03C2 #\U+05F3 #\U+05F4 #\U+0640 #\U+0660
          #\U+0661 #\U+0662 #\U+0663 #\U+0664 #\U+0665 #\U+0666 #\U+0667 #\U+0668
          #\U+0669 #\U+06F0 #\U+06F1 #\U+06F2 #\U+06F3 #\U+06F4 #\U+06F5 #\U+06F6
          #\U+06F7 #\U+06F8 #\U+06F9 #\U+06FD #\U+06FE #\U+07FA #\U+0F0B #\U+3007
          #\U+302E #\U+302F #\U+3031 #\U+3032 #\U+3033 #\U+3034 #\U+3035 #\U+303B
          #\U+30FB)))

(defun space-p (char)
  "PRECIS Spaces (N) category"
  (eql (general-category char) :zs))

(defun symbol-p (char)
  "PRECIS Symbol (O) category"
  (find (general-category char)
        '(:sm :sc :sk :so)))

(defun punctuation-p (char)
  "PRECIS Punctuation (P) category"
  (find (general-category char)
        '(:pc :pd :ps :pe :pi :pf :po)))

(defun other-letter-digit-p (char)
  "PRECIS OtherLetterDigits (R) category"
  (find (general-category char)
        '(:lt :nl :no :me)))


(defun identifier-class-p (string)
  (null
   (find-if-not
    (conjoin #'letter-digit-p
             #'ascii7-p)
    string)))

;;    If .cp. .in. Exceptions Then Exceptions(cp);
;;    Else If .cp. .in. BackwardCompatible Then BackwardCompatible(cp);
;;    Else If .cp. .in. Unassigned Then UNASSIGNED;
;;    Else If .cp. .in. ASCII7 Then PVALID;
;;    Else If .cp. .in. JoinControl Then CONTEXTJ;
;;    Else If .cp. .in. OldHangulJamo Then DISALLOWED;
;;    Else If .cp. .in. PrecisIgnorableProperties Then DISALLOWED;
;;    Else If .cp. .in. Controls Then DISALLOWED;
;;    Else If .cp. .in. HasCompat Then ID_DIS or FREE_PVAL;
;;    Else If .cp. .in. LetterDigits Then PVALID;
;;    Else If .cp. .in. OtherLetterDigits Then ID_DIS or FREE_PVAL;
;;    Else If .cp. .in. Spaces Then ID_DIS or FREE_PVAL;
;;    Else If .cp. .in. Symbols Then ID_DIS or FREE_PVAL;
;;    Else If .cp. .in. Punctuation Then ID_DIS or FREE_PVAL;
;;    Else DISALLOWED;

;; U+0021 through U+007E

;; Appendix A.1.  ZERO WIDTH NON-JOINER
;; 
;;    Code point:
;;       U+200C
;; 
;;    Overview:
;;       This may occur in a formally cursive script (such as Arabic) in a
;;       context where it breaks a cursive connection as required for
;;       orthographic rules, as in the Persian language, for example.  It
;;       also may occur in Indic scripts in a consonant-conjunct context
;;       (immediately following a virama), to control required display of
;;       such conjuncts.
;; 
;;    Lookup:
;;       True
;; 
;;    Rule Set:
;; 
;;       False;
;; 
;;       If Canonical_Combining_Class(Before(cp)) .eq.  Virama Then True;
;; 
;;       If RegExpMatch((Joining_Type:{L,D})(Joining_Type:T)*\u200C
;; 
;;          (Joining_Type:T)*(Joining_Type:{R,D})) Then True;
;; 
;; 
;; Appendix A.2.  ZERO WIDTH JOINER
;; 
;;    Code point:
;;       U+200D
;; 
;;    Overview:
;;       This may occur in Indic scripts in a consonant-conjunct context
;;       (immediately following a virama), to control required display of
;;       such conjuncts.
;; 
;;    Lookup:
;;       True
;; 
;;    Rule Set:
;; 
;;       False;
;; 
;;       If Canonical_Combining_Class(Before(cp)) .eq.  Virama Then True;
;; 
;; 
;; Appendix A.3.  MIDDLE DOT
;; 
;;    Code point:
;;       U+00B7
;; 
;;    Overview:
;;       Between 'l' (U+006C) characters only, used to permit the Catalan
;;       character ela geminada to be expressed.
;; 
;;    Lookup:
;;       False
;; 
;;    Rule Set:
;; 
;;       False;
;; 
;;       If Before(cp) .eq.  U+006C And
;; 
;;          After(cp) .eq.  U+006C Then True;
;; 
;; 
;; Appendix A.4.  GREEK LOWER NUMERAL SIGN (KERAIA)
;; 
;;    Code point:
;;       U+0375
;; 
;;    Overview:
;;       The script of the following character MUST be Greek.
;; 
;;    Lookup:
;;       False
;; 
;;    Rule Set:
;; 
;;       False;
;; 
;;       If Script(After(cp)) .eq.  Greek Then True;
;; 
;; 
;; Appendix A.5.  HEBREW PUNCTUATION GERESH
;; 
;;    Code point:
;;       U+05F3
;; 
;;    Overview:
;;       The script of the preceding character MUST be Hebrew.
;; 
;;    Lookup:
;;       False
;; 
;;    Rule Set:
;; 
;;       False;
;; 
;;       If Script(Before(cp)) .eq.  Hebrew Then True;
;; 
;; 
;; Appendix A.6.  HEBREW PUNCTUATION GERSHAYIM
;; 
;;    Code point:
;;       U+05F4
;; 
;;    Overview:
;;       The script of the preceding character MUST be Hebrew.
;; 
;;    Lookup:
;;       False
;; 
;;    Rule Set:
;; 
;;       False;
;; 
;;       If Script(Before(cp)) .eq.  Hebrew Then True;
;; 
;; 
;; Appendix A.7.  KATAKANA MIDDLE DOT
;; 
;;    Code point:
;;       U+30FB
;; 
;;    Overview:
;;       Note that the Script of Katakana Middle Dot is not any of
;;       "Hiragana", "Katakana", or "Han".  The effect of this rule is to
;;       require at least one character in the label to be in one of those
;;       scripts.
;; 
;;    Lookup:
;;       False
;; 
;;    Rule Set:
;; 
;;       False;
;; 
;;       For All Characters:
;; 
;;          If Script(cp) .in. {Hiragana, Katakana, Han} Then True;
;; 
;;       End For;
;; 
;; 
;; Appendix A.8.  ARABIC-INDIC DIGITS
;; 
;;    Code point:
;;       0660..0669
;; 
;;    Overview:
;;       Can not be mixed with Extended Arabic-Indic Digits.
;; 
;;    Lookup:
;;       False
;; 
;;    Rule Set:
;; 
;;       True;
;; 
;;       For All Characters:
;; 
;;          If cp .in. 06F0..06F9 Then False;
;; 
;;       End For;
;; 
;; 
;; Appendix A.9.  EXTENDED ARABIC-INDIC DIGITS
;; 
;;    Code point:
;;       06F0..06F9
;; 
;;    Overview:
;;       Can not be mixed with Arabic-Indic Digits.
;; 
;;    Lookup:
;;       False
;; 
;;    Rule Set:
;; 
;;       True;
;; 
;;       For All Characters:
;; 
;;          If cp .in. 0660..0669 Then False;
;; 
;;       End For;

