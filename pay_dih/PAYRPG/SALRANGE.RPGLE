000100950322     H DATEDIT(*DMY)
000200950704     FEMPMAL13  IP   E           K DISK
000300950630     FREPORT    O    F  132        PRINTER OFLIND(*INOF)
000400001024     **
000500001024     D CNTDS           DS                  OCCURS(200) INZ
000600001024     D  CNT1                          4  0
000700001024     D  TXT                          35
000800001024     **
000900001024     D xarr            s             11  2 dim(200)  ascend
001000001024     D xmin            s             11  2 inz(11000)
001100001024     D xmax            s             11  2 inz(100000)
001200001024     D xincr           s             11  2 inz(1000)
001300001024     D count           s              4  0
001400001024     D xcnt            s              4  0 inz(1)
001500001024     D ycnt            s              4  0 inz(1)
001600001024     D value           s             11  2
001700001024     D valuemax        s             11  2
001800001024     **
001900950704     C     *LIKE         DEFINE    GROSS         GROSS1          + 2
002000950704     C     GROSS         DIV       12            GROSS1
002100950704     C                   MULT      26            GROSS1
002200950704     C     DEPT          IFNE      47
002300950704     C     CLASS         ANDNE     *BLANKS
002400001024     C                   reset                   ycnt
002500001024     C     gross1        lookup    xarr(ycnt)                           9999
002600001024     C                   z-add     xarr(ycnt)    value
002700001024     C     ycnt          occur     CNTDS
002800001024     C                   move      *BLANKS       TXT
002900001024
003000001024     ** Handle text for ranges
003100001024     C                   select
003200001024     C                   when      ycnt = 1
003300001024     C                   dsply                   emp
003400001024     C                   eval      txt = '     BELOW ' +
003500001024     C                                   %editc(value:'1':*cursym)
003600001024     C**                 when      ycnt = xcnt
003700001024     C                   when      ycnt = 200
003800001024     C                   eval      txt = %editc(xmax:'1':*cursym) +
003900001024     C                                   ' AND ABOVE'
004000001024     C                   other
004100001024     C                   eval      valuemax = value + xincr - 1
004200001024     C                   eval      txt = %editc(value:'1':*cursym) +
004300001024     C                             ' -' + %editc(valuemax:'1':*cursym)
004400001024     C                   endsl
004500001024     **
004600001024     C                   add       1             CNT1              4 0
004700950704     C                   ENDIF
004800001024     CLR   count         DOUEQ     200
004900001024     C**   count         DOUEQ     xcnt
005000001024     CLR                 ADD       1             count
005100001024     CLR   count         OCCUR     CNTDS
005200950704     CLR                 ADD       CNT1          TOTCNT            4 0
005300950704     CLR   CNT1          IFNE      *ZEROS
005400950704     CLR                 EXCEPT
005500950704     CLR                 ENDIF
005600950704     CLR                 ENDDO
005700950704     CLR                 EXCEPT    TOTAL
005800001024     **
005900001024     C     *inzsr        begsr
006000001024     C                   z-add     *zeros        xarr(xcnt)
006100001024     C     2             do        200           xcnt
006200001024     C                   z-add     xmin          xarr(xcnt)
006300001024     C                   add       xincr         xmin
006400001024     **
006500001024     C                   if        xmin > xmax
006600001024     C                   leave
006700001024     C                   endif
006800001024     **
006900001024     C                   enddo
007000001024     C                   endsr
007100950704     OREPORT    H    1P                     1 02
007200950704     O         OR    OF
007300950704     O                                           74 'BANKS  D.I.H.  LTD'
007400950704     O          H    1P                     2
007500950704     O         OR    OF
007600950704     O                                           74 '=================='
007700950704     O          H    1P                  0  1
007800950704     O         OR    OF
007900950704     O                                           71 'D"AGUIAR"S   INDUSTRIES'
008000950704     O                                           84 '&  HOLDINGS'
008100950704     O          H    1P                  0  1
008200950704     O         OR    OF
008300950704     O                                           72 '========================'
008400950704     O                                           84 '============'
008500950704     O          H    1P                  0  1
008600950704     O         OR    OF
008700950704     O                                           67 'EMPLOYEES WAGES'
008800950704     O                                         +  1 'BREAKDOWN'
008900950704     O                                          120 'PAGE'
009000950704     O                       PAGE              +  1
009100950704     O          H    1P                  0  1
009200950704     O         OR    OF
009300950704     O                                           67 '==============='
009400950704     O                                         +  0 '========='
009500950704     O          H    1P                  1  1
009600950704     O         OR    OF
009700001024     O                                           54 'CATEGORY'
009800001024     O                                         + 13 'TOTAL EMP.'
009900950704     O          H    1P                  0  1
010000950704     O         OR    OF
010100001024     O                                           54 '--------'
010200001024     O                                         + 13 '----------'
010300950704     O          E                        2
010400950704     O                       TXT                 64
010500950704     O                       CNT1          Z     74
010600950704     O          E            TOTAL       1
010700950704     O                       TOTCNT        Z     74
010800950704     O                                           77 '**'
