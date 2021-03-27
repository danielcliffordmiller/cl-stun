(in-package :cl-stun.precis)


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
