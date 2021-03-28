(in-package :cl-stun.precis)

(defconstant +virama+ 9)

(defvar *context-maps*
  '((#\U+200C . zero-width-non-joiner)))

(let ((joining-map
	'((#x0600 . :u) (#x0601 . :u) (#x0602 . :u) (#x0603 . :u)
	  (#x0604 . :u) (#x0605 . :u) (#x0608 . :u) (#x060B . :u)
	  (#x0620 . :d) (#x0621 . :u) (#x0622 . :r) (#x0623 . :r)
	  (#x0624 . :r) (#x0625 . :r) (#x0626 . :d) (#x0627 . :r)
	  (#x0628 . :d) (#x0629 . :r) (#x062A . :d) (#x062B . :d)
	  (#x062C . :d) (#x062D . :d) (#x062E . :d) (#x062F . :r)
	  (#x0630 . :r) (#x0631 . :r) (#x0632 . :r) (#x0633 . :d)
	  (#x0634 . :d) (#x0635 . :d) (#x0636 . :d) (#x0637 . :d)
	  (#x0638 . :d) (#x0639 . :d) (#x063A . :d) (#x063B . :d)
	  (#x063C . :d) (#x063D . :d) (#x063E . :d) (#x063F . :d)
	  (#x0640 . :c) (#x0641 . :d) (#x0642 . :d) (#x0643 . :d)
	  (#x0644 . :d) (#x0645 . :d) (#x0646 . :d) (#x0647 . :d)
	  (#x0648 . :r) (#x0649 . :d) (#x064A . :d) (#x066E . :d)
	  (#x066F . :d) (#x0671 . :r) (#x0672 . :r) (#x0673 . :r)
	  (#x0674 . :u) (#x0675 . :r) (#x0676 . :r) (#x0677 . :r)
	  (#x0678 . :d) (#x0679 . :d) (#x067A . :d) (#x067B . :d)
	  (#x067C . :d) (#x067D . :d) (#x067E . :d) (#x067F . :d)
	  (#x0680 . :d) (#x0681 . :d) (#x0682 . :d) (#x0683 . :d)
	  (#x0684 . :d) (#x0685 . :d) (#x0686 . :d) (#x0687 . :d)
	  (#x0688 . :r) (#x0689 . :r) (#x068A . :r) (#x068B . :r)
	  (#x068C . :r) (#x068D . :r) (#x068E . :r) (#x068F . :r)
	  (#x0690 . :r) (#x0691 . :r) (#x0692 . :r) (#x0693 . :r)
	  (#x0694 . :r) (#x0695 . :r) (#x0696 . :r) (#x0697 . :r)
	  (#x0698 . :r) (#x0699 . :r) (#x069A . :d) (#x069B . :d)
	  (#x069C . :d) (#x069D . :d) (#x069E . :d) (#x069F . :d)
	  (#x06A0 . :d) (#x06A1 . :d) (#x06A2 . :d) (#x06A3 . :d)
	  (#x06A4 . :d) (#x06A5 . :d) (#x06A6 . :d) (#x06A7 . :d)
	  (#x06A8 . :d) (#x06A9 . :d) (#x06AA . :d) (#x06AB . :d)
	  (#x06AC . :d) (#x06AD . :d) (#x06AE . :d) (#x06AF . :d)
	  (#x06B0 . :d) (#x06B1 . :d) (#x06B2 . :d) (#x06B3 . :d)
	  (#x06B4 . :d) (#x06B5 . :d) (#x06B6 . :d) (#x06B7 . :d)
	  (#x06B8 . :d) (#x06B9 . :d) (#x06BA . :d) (#x06BB . :d)
	  (#x06BC . :d) (#x06BD . :d) (#x06BE . :d) (#x06BF . :d)
	  (#x06C0 . :r) (#x06C1 . :d) (#x06C2 . :d) (#x06C3 . :r)
	  (#x06C4 . :r) (#x06C5 . :r) (#x06C6 . :r) (#x06C7 . :r)
	  (#x06C8 . :r) (#x06C9 . :r) (#x06CA . :r) (#x06CB . :r)
	  (#x06CC . :d) (#x06CD . :r) (#x06CE . :d) (#x06CF . :r)
	  (#x06D0 . :d) (#x06D1 . :d) (#x06D2 . :r) (#x06D3 . :r)
	  (#x06D5 . :r) (#x06DD . :u) (#x06EE . :r) (#x06EF . :r)
	  (#x06FA . :d) (#x06FB . :d) (#x06FC . :d) (#x06FF . :d)
	  (#x070F . :t) (#x0710 . :r) (#x0712 . :d) (#x0713 . :d)
	  (#x0714 . :d) (#x0715 . :r) (#x0716 . :r) (#x0717 . :r)
	  (#x0718 . :r) (#x0719 . :r) (#x071A . :d) (#x071B . :d)
	  (#x071C . :d) (#x071D . :d) (#x071E . :r) (#x071F . :d)
	  (#x0720 . :d) (#x0721 . :d) (#x0722 . :d) (#x0723 . :d)
	  (#x0724 . :d) (#x0725 . :d) (#x0726 . :d) (#x0727 . :d)
	  (#x0728 . :r) (#x0729 . :d) (#x072A . :r) (#x072B . :d)
	  (#x072C . :r) (#x072D . :d) (#x072E . :d) (#x072F . :r)
	  (#x074D . :r) (#x074E . :d) (#x074F . :d) (#x0750 . :d)
	  (#x0751 . :d) (#x0752 . :d) (#x0753 . :d) (#x0754 . :d)
	  (#x0755 . :d) (#x0756 . :d) (#x0757 . :d) (#x0758 . :d)
	  (#x0759 . :r) (#x075A . :r) (#x075B . :r) (#x075C . :d)
	  (#x075D . :d) (#x075E . :d) (#x075F . :d) (#x0760 . :d)
	  (#x0761 . :d) (#x0762 . :d) (#x0763 . :d) (#x0764 . :d)
	  (#x0765 . :d) (#x0766 . :d) (#x0767 . :d) (#x0768 . :d)
	  (#x0769 . :d) (#x076A . :d) (#x076B . :r) (#x076C . :r)
	  (#x076D . :d) (#x076E . :d) (#x076F . :d) (#x0770 . :d)
	  (#x0771 . :r) (#x0772 . :d) (#x0773 . :r) (#x0774 . :r)
	  (#x0775 . :d) (#x0776 . :d) (#x0777 . :d) (#x0778 . :r)
	  (#x0779 . :r) (#x077A . :d) (#x077B . :d) (#x077C . :d)
	  (#x077D . :d) (#x077E . :d) (#x077F . :d) (#x07CA . :d)
	  (#x07CB . :d) (#x07CC . :d) (#x07CD . :d) (#x07CE . :d)
	  (#x07CF . :d) (#x07D0 . :d) (#x07D1 . :d) (#x07D2 . :d)
	  (#x07D3 . :d) (#x07D4 . :d) (#x07D5 . :d) (#x07D6 . :d)
	  (#x07D7 . :d) (#x07D8 . :d) (#x07D9 . :d) (#x07DA . :d)
	  (#x07DB . :d) (#x07DC . :d) (#x07DD . :d) (#x07DE . :d)
	  (#x07DF . :d) (#x07E0 . :d) (#x07E1 . :d) (#x07E2 . :d)
	  (#x07E3 . :d) (#x07E4 . :d) (#x07E5 . :d) (#x07E6 . :d)
	  (#x07E7 . :d) (#x07E8 . :d) (#x07E9 . :d) (#x07EA . :d)
	  (#x07FA . :c) (#x0840 . :r) (#x0841 . :d) (#x0842 . :d)
	  (#x0843 . :d) (#x0844 . :d) (#x0845 . :d) (#x0846 . :r)
	  (#x0847 . :r) (#x0848 . :d) (#x0849 . :r) (#x084A . :d)
	  (#x084B . :d) (#x084C . :d) (#x084D . :d) (#x084E . :d)
	  (#x084F . :d) (#x0850 . :d) (#x0851 . :d) (#x0852 . :d)
	  (#x0853 . :d) (#x0854 . :r) (#x0855 . :d) (#x0856 . :r)
	  (#x0857 . :r) (#x0858 . :r) (#x0860 . :d) (#x0861 . :u)
	  (#x0862 . :d) (#x0863 . :d) (#x0864 . :d) (#x0865 . :d)
	  (#x0866 . :u) (#x0867 . :r) (#x0868 . :d) (#x0869 . :r)
	  (#x086A . :r) (#x08A0 . :d) (#x08A1 . :d) (#x08A2 . :d)
	  (#x08A3 . :d) (#x08A4 . :d) (#x08A5 . :d) (#x08A6 . :d)
	  (#x08A7 . :d) (#x08A8 . :d) (#x08A9 . :d) (#x08AA . :r)
	  (#x08AB . :r) (#x08AC . :r) (#x08AD . :u) (#x08AE . :r)
	  (#x08AF . :d) (#x08B0 . :d) (#x08B1 . :r) (#x08B2 . :r)
	  (#x08B3 . :d) (#x08B4 . :d) (#x08B6 . :d) (#x08B7 . :d)
	  (#x08B8 . :d) (#x08B9 . :r) (#x08BA . :d) (#x08BB . :d)
	  (#x08BC . :d) (#x08BD . :d) (#x08BE . :d) (#x08BF . :d)
	  (#x08C0 . :d) (#x08C1 . :d) (#x08C2 . :d) (#x08C3 . :d)
	  (#x08C4 . :d) (#x08C5 . :d) (#x08C6 . :d) (#x08C7 . :d)
	  (#x08E2 . :u) (#x1806 . :u) (#x1807 . :d) (#x180A . :c)
	  (#x180E . :u) (#x1820 . :d) (#x1821 . :d) (#x1822 . :d)
	  (#x1823 . :d) (#x1824 . :d) (#x1825 . :d) (#x1826 . :d)
	  (#x1827 . :d) (#x1828 . :d) (#x1829 . :d) (#x182A . :d)
	  (#x182B . :d) (#x182C . :d) (#x182D . :d) (#x182E . :d)
	  (#x182F . :d) (#x1830 . :d) (#x1831 . :d) (#x1832 . :d)
	  (#x1833 . :d) (#x1834 . :d) (#x1835 . :d) (#x1836 . :d)
	  (#x1837 . :d) (#x1838 . :d) (#x1839 . :d) (#x183A . :d)
	  (#x183B . :d) (#x183C . :d) (#x183D . :d) (#x183E . :d)
	  (#x183F . :d) (#x1840 . :d) (#x1841 . :d) (#x1842 . :d)
	  (#x1843 . :d) (#x1844 . :d) (#x1845 . :d) (#x1846 . :d)
	  (#x1847 . :d) (#x1848 . :d) (#x1849 . :d) (#x184A . :d)
	  (#x184B . :d) (#x184C . :d) (#x184D . :d) (#x184E . :d)
	  (#x184F . :d) (#x1850 . :d) (#x1851 . :d) (#x1852 . :d)
	  (#x1853 . :d) (#x1854 . :d) (#x1855 . :d) (#x1856 . :d)
	  (#x1857 . :d) (#x1858 . :d) (#x1859 . :d) (#x185A . :d)
	  (#x185B . :d) (#x185C . :d) (#x185D . :d) (#x185E . :d)
	  (#x185F . :d) (#x1860 . :d) (#x1861 . :d) (#x1862 . :d)
	  (#x1863 . :d) (#x1864 . :d) (#x1865 . :d) (#x1866 . :d)
	  (#x1867 . :d) (#x1868 . :d) (#x1869 . :d) (#x186A . :d)
	  (#x186B . :d) (#x186C . :d) (#x186D . :d) (#x186E . :d)
	  (#x186F . :d) (#x1870 . :d) (#x1871 . :d) (#x1872 . :d)
	  (#x1873 . :d) (#x1874 . :d) (#x1875 . :d) (#x1876 . :d)
	  (#x1877 . :d) (#x1878 . :d) (#x1880 . :u) (#x1881 . :u)
	  (#x1882 . :u) (#x1883 . :u) (#x1884 . :u) (#x1885 . :t)
	  (#x1886 . :t) (#x1887 . :d) (#x1888 . :d) (#x1889 . :d)
	  (#x188A . :d) (#x188B . :d) (#x188C . :d) (#x188D . :d)
	  (#x188E . :d) (#x188F . :d) (#x1890 . :d) (#x1891 . :d)
	  (#x1892 . :d) (#x1893 . :d) (#x1894 . :d) (#x1895 . :d)
	  (#x1896 . :d) (#x1897 . :d) (#x1898 . :d) (#x1899 . :d)
	  (#x189A . :d) (#x189B . :d) (#x189C . :d) (#x189D . :d)
	  (#x189E . :d) (#x189F . :d) (#x18A0 . :d) (#x18A1 . :d)
	  (#x18A2 . :d) (#x18A3 . :d) (#x18A4 . :d) (#x18A5 . :d)
	  (#x18A6 . :d) (#x18A7 . :d) (#x18A8 . :d) (#x18AA . :d)
	  (#x200C . :u) (#x200D . :c) (#x202F . :u) (#x2066 . :u)
	  (#x2067 . :u) (#x2068 . :u) (#x2069 . :u) (#xA840 . :d)
	  (#xA841 . :d) (#xA842 . :d) (#xA843 . :d) (#xA844 . :d)
	  (#xA845 . :d) (#xA846 . :d) (#xA847 . :d) (#xA848 . :d)
	  (#xA849 . :d) (#xA84A . :d) (#xA84B . :d) (#xA84C . :d)
	  (#xA84D . :d) (#xA84E . :d) (#xA84F . :d) (#xA850 . :d)
	  (#xA851 . :d) (#xA852 . :d) (#xA853 . :d) (#xA854 . :d)
	  (#xA855 . :d) (#xA856 . :d) (#xA857 . :d) (#xA858 . :d)
	  (#xA859 . :d) (#xA85A . :d) (#xA85B . :d) (#xA85C . :d)
	  (#xA85D . :d) (#xA85E . :d) (#xA85F . :d) (#xA860 . :d)
	  (#xA861 . :d) (#xA862 . :d) (#xA863 . :d) (#xA864 . :d)
	  (#xA865 . :d) (#xA866 . :d) (#xA867 . :d) (#xA868 . :d)
	  (#xA869 . :d) (#xA86A . :d) (#xA86B . :d) (#xA86C . :d)
	  (#xA86D . :d) (#xA86E . :d) (#xA86F . :d) (#xA870 . :d)
	  (#xA871 . :d) (#xA872 . :l) (#xA873 . :u) (#x10AC0 . :d)
	  (#x10AC1 . :d) (#x10AC2 . :d) (#x10AC3 . :d) (#x10AC4 . :d)
	  (#x10AC5 . :r) (#x10AC6 . :u) (#x10AC7 . :r) (#x10AC8 . :u)
	  (#x10AC9 . :r) (#x10ACA . :r) (#x10ACB . :u) (#x10ACC . :u)
	  (#x10ACD . :l) (#x10ACE . :r) (#x10ACF . :r) (#x10AD0 . :r)
	  (#x10AD1 . :r) (#x10AD2 . :r) (#x10AD3 . :d) (#x10AD4 . :d)
	  (#x10AD5 . :d) (#x10AD6 . :d) (#x10AD7 . :l) (#x10AD8 . :d)
	  (#x10AD9 . :d) (#x10ADA . :d) (#x10ADB . :d) (#x10ADC . :d)
	  (#x10ADD . :r) (#x10ADE . :d) (#x10ADF . :d) (#x10AE0 . :d)
	  (#x10AE1 . :r) (#x10AE2 . :u) (#x10AE3 . :u) (#x10AE4 . :r)
	  (#x10AEB . :d) (#x10AEC . :d) (#x10AED . :d) (#x10AEE . :d)
	  (#x10AEF . :r) (#x10B80 . :d) (#x10B81 . :r) (#x10B82 . :d)
	  (#x10B83 . :r) (#x10B84 . :r) (#x10B85 . :r) (#x10B86 . :d)
	  (#x10B87 . :d) (#x10B88 . :d) (#x10B89 . :r) (#x10B8A . :d)
	  (#x10B8B . :d) (#x10B8C . :r) (#x10B8D . :d) (#x10B8E . :r)
	  (#x10B8F . :r) (#x10B90 . :d) (#x10B91 . :r) (#x10BA9 . :r)
	  (#x10BAA . :r) (#x10BAB . :r) (#x10BAC . :r) (#x10BAD . :d)
	  (#x10BAE . :d) (#x10BAF . :u) (#x10D00 . :l) (#x10D01 . :d)
	  (#x10D02 . :d) (#x10D03 . :d) (#x10D04 . :d) (#x10D05 . :d)
	  (#x10D06 . :d) (#x10D07 . :d) (#x10D08 . :d) (#x10D09 . :d)
	  (#x10D0A . :d) (#x10D0B . :d) (#x10D0C . :d) (#x10D0D . :d)
	  (#x10D0E . :d) (#x10D0F . :d) (#x10D10 . :d) (#x10D11 . :d)
	  (#x10D12 . :d) (#x10D13 . :d) (#x10D14 . :d) (#x10D15 . :d)
	  (#x10D16 . :d) (#x10D17 . :d) (#x10D18 . :d) (#x10D19 . :d)
	  (#x10D1A . :d) (#x10D1B . :d) (#x10D1C . :d) (#x10D1D . :d)
	  (#x10D1E . :d) (#x10D1F . :d) (#x10D20 . :d) (#x10D21 . :d)
	  (#x10D22 . :r) (#x10D23 . :d) (#x10F30 . :d) (#x10F31 . :d)
	  (#x10F32 . :d) (#x10F33 . :r) (#x10F34 . :d) (#x10F35 . :d)
	  (#x10F36 . :d) (#x10F37 . :d) (#x10F38 . :d) (#x10F39 . :d)
	  (#x10F3A . :d) (#x10F3B . :d) (#x10F3C . :d) (#x10F3D . :d)
	  (#x10F3E . :d) (#x10F3F . :d) (#x10F40 . :d) (#x10F41 . :d)
	  (#x10F42 . :d) (#x10F43 . :d) (#x10F44 . :d) (#x10F45 . :u)
	  (#x10F51 . :d) (#x10F52 . :d) (#x10F53 . :d) (#x10F54 . :r)
	  (#x10FB0 . :d) (#x10FB1 . :u) (#x10FB2 . :d) (#x10FB3 . :d)
	  (#x10FB4 . :r) (#x10FB5 . :r) (#x10FB6 . :r) (#x10FB7 . :u)
	  (#x10FB8 . :d) (#x10FB9 . :r) (#x10FBA . :r) (#x10FBB . :d)
	  (#x10FBC . :d) (#x10FBD . :r) (#x10FBE . :d) (#x10FBF . :d)
	  (#x10FC0 . :u) (#x10FC1 . :d) (#x10FC2 . :r) (#x10FC3 . :r)
	  (#x10FC4 . :d) (#x10FC5 . :u) (#x10FC6 . :u) (#x10FC7 . :u)
	  (#x10FC8 . :u) (#x10FC9 . :r) (#x10FCA . :d) (#x10FCB . :l)
	  (#x110BD . :u) (#x110CD . :u) (#x1E900 . :d) (#x1E901 . :d)
	  (#x1E902 . :d) (#x1E903 . :d) (#x1E904 . :d) (#x1E905 . :d)
	  (#x1E906 . :d) (#x1E907 . :d) (#x1E908 . :d) (#x1E909 . :d)
	  (#x1E90A . :d) (#x1E90B . :d) (#x1E90C . :d) (#x1E90D . :d)
	  (#x1E90E . :d) (#x1E90F . :d) (#x1E910 . :d) (#x1E911 . :d)
	  (#x1E912 . :d) (#x1E913 . :d) (#x1E914 . :d) (#x1E915 . :d)
	  (#x1E916 . :d) (#x1E917 . :d) (#x1E918 . :d) (#x1E919 . :d)
	  (#x1E91A . :d) (#x1E91B . :d) (#x1E91C . :d) (#x1E91D . :d)
	  (#x1E91E . :d) (#x1E91F . :d) (#x1E920 . :d) (#x1E921 . :d)
	  (#x1E922 . :d) (#x1E923 . :d) (#x1E924 . :d) (#x1E925 . :d)
	  (#x1E926 . :d) (#x1E927 . :d) (#x1E928 . :d) (#x1E929 . :d)
	  (#x1E92A . :d) (#x1E92B . :d) (#x1E92C . :d) (#x1E92D . :d)
	  (#x1E92E . :d) (#x1E92F . :d) (#x1E930 . :d) (#x1E931 . :d)
	  (#x1E932 . :d) (#x1E933 . :d) (#x1E934 . :d) (#x1E935 . :d)
	  (#x1E936 . :d) (#x1E937 . :d) (#x1E938 . :d) (#x1E939 . :d)
	  (#x1E93A . :d) (#x1E93B . :d) (#x1E93C . :d) (#x1E93D . :d)
	  (#x1E93E . :d) (#x1E93F . :d) (#x1E940 . :d) (#x1E941 . :d)
	  (#x1E942 . :d) (#x1E943 . :d) (#x1E94B . :t)))
      (hash (make-hash-table)))
  (dolist (p joining-map)
    (setf (gethash (car p) hash) (cdr p)))
  (defun joining-type (char)
    (values (gethash (char-code char) hash))))

(defun joining-type-property-resolver (string)
  "resolver for cl-ppcre"
  (let* ((property-value (split-sequence #\: string))
	 ;(property (first property-value))
	 (joining-type (second property-value))
	 (keyword (intern joining-type :keyword)))
    #'(lambda (char)
	(eql (joining-type char) keyword))))

;; this needs to be fixed to have the regex center around the
;; character in question
(defun zero-width-non-joiner (char string offset)
  (let ((*property-resolver* #'joining-type-property-resolver))
   (if (or (eql (combining-class (elt string (1- offset))) +virama+)
	(scan (create-scanner
	       (concatenate
		'string
		"(\\p{Joining_Type:L}|\\p{Joining_Type:D})\\p{Joining_Type:T}*"
		(list char)
		"\\p{Joining_Type:T}*(\\p{Joining_Type:R}|\\p{Joining_Type:D})"))
	      string))
       :pvalid
       :disallowed)))

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

