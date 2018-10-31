000100141022     H  dftactgrp(*no) bnddir('DISDIR')
000200950831     FXMS010D   CF   E             WORKSTN
000300950831     FXMSAMTF   UF A E           K DISK
000400010918     FEMPSMF    IF   E           K DISK
000500950921     D*
000600950921     D                SDS
000700950921     D  @PGM             *PROC
000800950831     **
000900001129     C     emp_key       klist
001000001129     C                   kfld                    xclass
001100001129     C                   kfld                    xemp
001200001129     **
001300950718     C     TOP           TAG
001400950719     C                   EXFMT     SELOPT
001500950719     C   03              GOTO      BOTTOM
001600001129     **
001700001129     C                   exsr      validate_rtn
001800950831     C     XCLASS        IFNE      *BLANKS
001900001129     **
002000001129     C                   if        xemp <> *zeros
002100001129     C     emp_key       chain     XMSAMTF                            02
002200001129     C                   else
002300001129     C     XCLASS        chain     XMSAMTF                            02
002400001129     C                   endif
002500950831     **
002600950831     C     *IN02         IFEQ      '1'
002700001129     C                   Z-ADD     *zeros        xamt
002800950821     C                   ENDIF
002900950822     **
003000950718     C                   EXFMT     REC
003100950822     **
003200950822     C   03              GOTO      BOTTOM
003300950822     **
003400950719     C     *IN04         IFEQ      '1'
003500950831     C                   DELETE    XMAREC
003600950719     C                   ELSE
003700950719     C     *IN04         IFEQ      '0'
003800950831     C     *IN02         ANDEQ     '0'
003900950831     C                   UPDATE    XMAREC
004000950719     C                   ELSE
004100950719     C     *IN04         IFEQ      '0'
004200950831     C     *IN02         ANDEQ     '1'
004300950831     C                   WRITE     XMAREC
004400950719     C                   ENDIF
004500950719     C                   ENDIF
004600950719     C                   ENDIF
004700950718     C                   GOTO      TOP
004800950831     **
004900950831     C                   ELSE
005000950831     **
005100950831     C     *LOVAL        SETLL     XMSAMTF
005200950831     C     XCHG          IFEQ      '+'
005300960930     C                   eval      xper = (100+xper)/100
005400950831     C                   ELSE
005500950831     C     XCHG          IFEQ      '-'
005600960930     C                   eval      xper = 1/(100+xper)*100
005700950831     C                   ELSE
005800950831     C                   GOTO      BOTTOM
005900950831     C                   ENDIF
006000950831     C                   ENDIF
006100950831     C     *IN05         DOWEQ     '0'
006200950831     C                   READ      XMAREC                                 05
006300950831     C  N05              MULT      XPER          XAMT
006400950831     C  N05              UPDATE    XMAREC
006500950831     C                   ENDDO
006600950831     C                   ENDIF
006700950719     C     BOTTOM        TAG
006800950719     C                   SETON                                        LR
006900001129     **
007000001129     C     validate_rtn  begsr
007100001129     **validate entry fields
007200001129
007300010918     C                   eval      *in01 = *off
007400010918     C                   if        xclass <> *blanks
007500010918     C     XCLASS        CHAIN     Esmfmt                             01
007600001129     C     *IN01         CABEQ     '1'           TOP
007700010918     C                   ENDIF
007800001129     c
007900001129     C     xemp          ifne      *zeros
008000001129     C     xclass        andeq     *blanks
008100001129     C                   seton                                        12
008200001129     C                   goto      top
008300001129     c                   endif
008400001129     C
008500001129     C     xclass        ifne      *blanks
008600001129     C     xemp          andne     *zeros
008700001129     C
008800001129     c                   callb     '@GETEMP'
008900001129     c                   parm                    xemp
009000001129     c                   parm                    dept              3 0
009100001129     c                   parm      'E'           list#            15
009200001129     c                   parm      *blanks       err#              7
009300001129     c                   parm                    elib#            10
009400001129     c                   parm                    efile#           10
009500001129     c                   parm                    name#            40
009600001129     c                   parm                    class             3
009700001129     C                   if        err# <> *blanks
009800001129     C                   seton                                        10
009900001129     C                   else
010000001129     C                   setoff                                       10
010100001129     C                   endif
010200001129     C     *in10         cabeq     '1'           top
010300001129     C
010400001129     C     *like         define    class         hclass
010500001129     C     'P':' '       xlate     class         hclass
010600001129     C                   if        hclass <> xclass
010700001129     C                   seton                                        11
010800001129     C                   goto      top
010900001129     C                   endif
011000001129     C
011100001129     C                   endif
011200001129     C                   endsr
