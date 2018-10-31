000100950530     H DATEDIT(*DMY)
000200980127     F* BANKS DIH Payroll book program
000300010903     Fpayhisl03 IF   E           K DISK
000400950705     FQSYSPRT   O    F  132        PRINTER
000500010903      /copy genpr
000600980131     Ddnumb            s              3  0
000700950530     D                SDS
000800950530     D  @PGM             *PROC
000900950530     D  @USER                254    263
001000010903     ** system fields for report
001100010903     D@date            S             10
001200010903     D@title           S             60
001300010903     Dconame           S             30
001400010903     D arr             S             40    DIM(1) CTDATA PERRCD(1)
001500980126     C
001600950717     C                   EXCEPT    HEADER
001700980126     C                   EXCEPT    DET
001800980131     C                   z-add     *zeros        dnumb
001900980131     C                   z-add     *zeros        #tot#
002000981202     C
002100980519     C*                  DOW       dnumb <> 92
002200980519     C*                  add       1             dnumb
002300981202     C*    dnumb         chain     brkdwn02                           20
002400981202     C
002500981202     C
002600980519     C     *LOVAL        SETLL     brkdwn02
002700981202     C                   read      brkdwn02                               30
002800980131     C
002900980519     C     *in30         Downe     *on
003000980131     C     reg           ifne      *zeros
003100980131     C     cas           orne      *zeros
003200980131     C     spec          orne      *zeros
003300980131     C     odds          orne      *zeros
003400980131     C     allw          orne      *zeros
003500980131     C     ot            orne      *zeros
003600980131     C     casb          orne      *zeros
003700980131     C                   add       reg           #tot              3 0
003800980131     C                   add       cas           #tot
003900980131     C                   add       spec          #tot
004000980131     C                   add       odds          #tot
004100980131     C                   add       allw          #tot
004200980131     C                   add       ot            #tot
004300980131     C                   add       casb          #tot
004400980131     C
004500981202     C*    *in30         ifne      *on
004600980131     C                   EXCEPT    figs
004700980210     C                   add       #tot          #tot#             4 0
004800981202     C*                  endif
004900980131     C
005000980131     C                   z-add     *zeros        #tot
005100980131     C                   endif
005200980131     C
005300981202     C                   read      brkdwn02                               30
005400980131     C                   EndDo
005500980131     C
005600980210     C                   EXCEPT    GTOT
005700980127     C                   EXCEPT    REPEND
005800980126     C                   seton                                        lr
005900980126     C                   return
006000010903     ***************************************************
006100010903     ** First initilisation routine for all programs  **
006200010903     ***************************************************
006300010903     C     AAINIT        BEGSR
006400010903     C                   eval      coname = get_coname
006500010903     C                   eval      @date = get_sys_date
006600010903     C     type          chain     prtyfmt
006700010903     ** select header for program
006800010903     C                   eval      @title = %trim(ARR(1)) +  ' ' + prtydesc
006900010903     **
007000010903     C                   ENDSR
007100950629     **
007200951120     OQSYSPRT   E            HEADER           01
007300950629     O                       @PGM                10
007400010903     O                       coname              70
007500951120     OQSYSPRT   E            HEADER           02
007600950629     O                       @USER               10
007700010903     O                       @title              91
007800951120     OQSYSPRT   E            HEADER           03
007900950629     O                                          120 'RUN Date: '
008000010903     O                       @date             +  1
008100980127     OQSYSPRT   E            HEADER           04
008200980202     O                                           57 'BEING BINGO-SHEET FOR'
008300980202     O                                         +  1 'W/E OR F/E ENDING'
008400980127     O                       UDATE         Y   +  1
008500980102     OQSYSPRT   E            det         1
008600980202     O                                           59 '---------'
008700980202     O                                         +  1 '---------'
008800980102     OQSYSPRT   E            det         1
008900980202     O                                           59 '| DEPT#  '
009000980202     O                                         +  1 '|SLIP TOT|'
009100980202     OQSYSPRT   E            det         1
009200980202     O                                           59 '---------'
009300980131     O                                         +  1 '---------'
009400980202     OQSYSPRT   E            figs        1  1
009500980519     O                       dnum          1     55
009600980202     O                       #tot          1     65
009700980210     OQSYSPRT   E            gtot        1  1
009800980210     O                                           59 '  TOTAL  '
009900980210     O                       #tot#         1     65
010000971231     OQSYSPRT   E            REPEND      2  1
010100971231     O                                           65 '*** END OF REPORT ***'
010200010903**
010300010903 Summary of Payments For -
