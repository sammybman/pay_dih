000100980219     H DECEDIT('.') DATEDIT(*dmy/)
000200091121     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300980218     ** Programmer S.Britton
000400020206     ** LIABILITY MASTER FILE
000500980219     Fliabml03  ip   e           k disk
000600091121     Fqsysprt   o    f  132        printer oflind(*inof)
000700020206     **
000800170210      /copy QCPYSRC,genpr
000900980219     **
001000980124     D                sds
001100980124     D  @pgm             *proc
001200170210     D* @user                254    263
001201170210     D  @user                358    367
001300020206     **
001400980124     D la              s             11  2 dim(12)
001500020206     D coname          s             30
001600980124     **
001700980124     Ilifmt
001800980219     I                                          grade         l1
001900980124     I              liamt01                     la(01)
002000980124     I              liamt02                     la(02)
002100980124     I              liamt03                     la(03)
002200980124     I              liamt04                     la(04)
002300980124     I              liamt05                     la(05)
002400980124     I              liamt06                     la(06)
002500980124     I              liamt07                     la(07)
002600980124     I              liamt08                     la(08)
002700980124     I              liamt09                     la(09)
002800980124     I              liamt10                     la(10)
002900980124     I              liamt11                     la(11)
003000980124     I              liamt12                     la(12)
003100980124     **
003200980124     C     *entry        plist
003300981105     C                   parm                    prd               6 0
003400981105     C                   parm                    typ               2 0
003500981105     C                   move      prd           emth              2 0
003600980124     **
003700950927     **
003800980219     C                   if        typ = litype
003900980219     C**                 move      class         grade             1 0
004000980219     C     grade         comp      '6'                                04  04
004100990913     C  n04grade         comp      'D'                                    04
004200980219     C                   if        *in04 and la(emth) <> *zeros
004300980219     C                   except    exec_det
004400980219     C                   endif
004500980219     C     grade         ifeq      '6'
004600980219     C     grade         oreq      '7'
004700980218     C                   add       la(emth)      junexc           10 2
004800980218     C                   add       la(emth)      grdtot           10 2
004900980218     C                   else
005000980219     C     grade         ifeq      '8'
005100980219     C     grade         oreq      '9'
005200981206     C     grade         oreq      'D'
005300980218     C                   add       la(emth)      chfexc           10 2
005400980218     C                   add       la(emth)      grdtot           10 2
005500980218     C                   else
005600980218     C                   add       la(emth)      regbal           10 2
005700980218     C                   endif
005800980218     C                   endif
005900980218     **
006000980218     C                   add       la(emth)      baltot           10 2
006100980219     C                   endif
006200980219     Cl1 04              except    exec_tot
006300020206     *************
006400020206     ** iniz.
006500020206     *************
006600020206     C     *inzsr        begsr
006700020206     C                   eval      coname = get_coname
006800020206     C                   endsr
006900980218     **
007000091121     Oqsysprt   h    1p                     1 02
007100980219     O         or    of
007200980219     O                       @pgm                10
007300020206     O                       coname              75
007400980219     O          h    1p                     1
007500980219     O         or    of
007600980219     O                       @user               10
007700940516     O                                           78 'EMPLOYEES LOAN BALANCES'
007800940516     O                                           90 'DATE:'
007900980219     O                       udate         Y    100
008000980219     O          h    1p                  2  2
008100980219     O         or    of
008200940516     O                                           22 'EMPLOYEE NO.'
008300940516     O                                           37 'NAME'
008400940516     O                                           58 'GRADE'
008500940516     O                                           75 'BALANCE'
008600980219     O          e            exec_det    1
008700980218     O                       emp           Z     18
008800980218     O                       christ              36
008900980218     O                       surnam              50
009000980218     O                       class               56
009100980219     O                       la(emth)      A     75
009200980219     O          e            exec_tot    1  1
009300980218     O                       grdtot        AB    90
009400940516     O                                           94 '***'
009500980218     O          t    lr                  2  2
009600940516     O                                           24 'TOTAL F1 TO 5 BALANCE->'
009700980218     O                       regbal        A     40 '$'
009800980218     O          t    lr                     2
009900940516     O                                           24 'TOTAL 6 AND 7 BALANCE->'
010000980218     O                       junexc        A     40 '$'
010100980218     O          t    lr                     2
010200940516     O                                           24 'TOTAL 8 AND 9 BALANCE->'
010300980218     O                       chfexc        A     40 '$'
010400980218     O          t    lr                     2
010500940516     O                                           24 'TOTAL ='
010600980218     O                       baltot        A     40 '$'
010700980218     O          t    lr
010800940516     O                                           75 '*** END OF REPORT ***'
