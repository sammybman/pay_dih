000100950118     H DATEDIT(*DMY)
000200960416     F* PROGRAMMER - S.BRITTON
000300980205     FKRON01D   CF   E             WORKSTN
000400960502     FKEMPML1   UF   E           K DISK
000500960415     FKINTF     O    F 1560        DISK    USROPN
000600980205     **
000700980205     D                SDS
000800980205     D  @PGM             *PROC
000900980205     **
001000960416     D ACF             S             20    DIM(16)
001100960416     D ACP             S             32    DIM(16)
001200020315     D msgid           S              7
001300011011     D TXT             C                   '0000000000'
001400980205     **
001500011011     D CMD             C                   'CLRPFM KINTF'
001600980206     **
001700960206     C                   READ      KEREC                                  10
001800011011     C                   DOW       not *in10
001900960206     C                   ADD       1             TEMP              4 0
002000960206     C                   READ      KEREC                                  10
002100960206     C                   ENDDO
002200960504     **
002300011011     C                   if        temp = *zeros
002400020313     C                   eval      *inlr = *on
002500020313     C                   call      'ESCPMSG'
002600020315     C                   parm                    @pgm
002700020315     C                   parm      'PAY0045'     msgid
002800980205     C                   endif
002900980205     ** CONFIRM PROCESSING
003000980205     C                   exfmt     kconfirm
003100980205     C                   if        not(*in21)
003200980205     C                   seton                                        lr
003300980205     C                   return
003400980205     C                   endif
003500980205     **
003600980205     **
003700960504     C     *IN99         IFEQ      '0'
003800960504     C                   Z-ADD     12            LENGTH           15 5
003900960504     C                   MOVE      CMD           COMMAN           12
004000960504     C                   CALL      'QCMDEXC'
004100960504     C                   PARM                    COMMAN
004200960504     C                   PARM                    LENGTH
004300960504     C                   OPEN      KINTF
004400960504     C                   SETON                                        99
004500960504     C                   ENDIF
004600960413     **
004700960413     C                   MOVE      '0'           FEXP            166
004800960206     C                   MOVE      '00'          KFLD              2
004900960206     C                   EXCEPT    ONE
005000960206     C                   MOVE      TXT           KFLD2            10
005100960205     C                   DO        6
005200960206     C                   EXCEPT    TWO
005300960205     C                   ENDDO
005400960206     C                   Z-ADD     TEMP          CODE3            10 0
005500960206     C                   MOVE      CODE3         KFLD2
005600960206     C                   EXCEPT    TWO
005700960413     C                   EXCEPT    THREE
005800960416     **
005900960416     **
006000960206     C     *LOVAL        SETLL     KEREC
006100960206     C                   READ      KEREC                                  LR
006200960206     C     *INLR         DOWEQ     '0'
006300960206     **
006400960416     C                   CLEAR                   ACF
006500960416     C                   CLEAR                   ACP
006600970130     C                   MOVE      *BLANK        KSTS
006700960413     C                   EXCEPT    ALL
006800960502     C                   MOVE      *BLANK        KSTS
006900960502     C                   UPDATE    KEREC
007000960206     C                   READ      KEREC                                  LR
007100960206     **
007200960206     C                   ENDDO
007300960416     **
007400960416     **
007500960413     OKINTF     E            ONE
007600960206     O                       KFLD                 2
007700960206     O          E            TWO
007800960206     O                       KFLD2               10
007900960206     O          E            THREE
008000960413     O                       FEXP               166
008100960413     O          E            ALL
008200960413     O                       KTYPE                2
008300960413     O                       KREQC                6
008400960413     O                       KRETC               10
008500960413     O                       KREFE               61
008600960413     O                       KEMP                73
008700960413     O                       KEMPST              77
008800960413     O                       KACCNM              99
008900960413     O                       KCLASS             116
009000960413     O                       KRST               133
009100960413     O                       KTERMG             150
009200960413     O                       KENAME             184
009300960413     O                       KOPT               218
009400960413     O                       KOPT               252
009500960413     O                       KOPT               286
009600960413     O                       KOPT               320
009700960413     O                       KOPT               354
009800960413     O                       KOPT               388
009900960413     O                       KSCHDE             405
010000960413     O                       KBADGE             416
010100960413     O                       KACCDT             426
010200960416     O                       ACF(1)             448
010300960416     O                       ACP(1)             482
010400960416     O                       ACF(2)             504
010500960416     O                       ACP(2)             538
010600960416     O                       ACF(3)             560
010700960416     O                       ACP(3)             594
010800960416     O                       ACF(4)             616
010900960416     O                       ACP(4)             650
011000960416     O                       ACF(5)             672
011100960416     O                       ACP(5)             706
011200960416     O                       ACF(6)             728
011300960416     O                       ACP(6)             762
011400960416     O                       ACF(7)             784
011500960416     O                       ACP(7)             818
011600960416     O                       ACF(8)             840
011700960416     O                       ACP(8)             874
011800960416     O                       ACF(9)             896
011900960416     O                       ACP(9)             930
012000960416     O                       ACF(10)            952
012100960416     O                       ACP(10)            986
012200960416     O                       ACF(11)           1008
012300960416     O                       ACP(11)           1042
012400960416     O                       ACF(12)           1064
012500960416     O                       ACP(12)           1098
012600960416     O                       ACF(13)           1120
012700960416     O                       ACP(13)           1154
012800960416     O                       ACF(14)           1176
012900960416     O                       ACP(14)           1210
013000960416     O                       ACF(15)           1232
013100960416     O                       ACP(15)           1266
013200960416     O                       ACF(16)           1288
013300960416     O                       ACP(16)           1322
013400960413     O                       KRATE             1334
013500960413     **
