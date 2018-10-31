000100950821     H DECEDIT('.') DATEDIT(*DMY/)
000200950821     F**
000300950821     F**
000400010112     Fpurchsref ip   e           k DISK
000500010821     Fwhtypl01  if   e           k DISK
000600010213     FQSYSPRT   O    F  132        PRINTER oflind(*inof) usropn
000700010810     Fwhrpt22d  cf   e             workstn
000800010115     **
000900010815     D/copy genpr
001000980817     D                SDS
001100980817     D  @PGM             *PROC
001200980817     D  @USER                254    263
001300010821     dcomp             c                   'Report Completed as Requested'
001400010821     d
001500010821     D dept#           s              3  0
001600010821     D date            s               d   datfmt(*iso)
001700010116     D ARR             S             40    DIM(2) CTDATA PERRCD(1)
001800990705     D*cmd             s             75    dim(1) ctdata perrcd(1)
001900010202     d
002000010202     **
002100010810     dsdate            s              8  0
002200010810     dedate            s              8  0
002300010810     dprod_qty         s              7  0
002400010810     dtot_dbt          s             11  2
002500010119     dtot_crd          s             11  2
002600010119     dcrd_amt          s             11  2
002700010115     dpro_tot          s             11  2
002800010112     dfin_tot          s             11  2
002900010112     dgl_account       s              6  0
003000010112     dgl_spc           s              1  0
003100010115     dlast_product     s                   like(dtpcod)
003200010821     dlast_trns_cde    s                   like(dttcod)
003300010813     dpddes#           s                   like(pddes)
003400010821     dtytxt#           s                   like(tytxt)
003500010810     d@date            s             10
003600010810     d@title           s             40
003700010810     dconame           s             30
003800010810     drec_opt          s              1
003900010202     d
004000010221     **
004100010115     C     *entry        plist
004200010810     C                   parm                    sdate
004300010810     C                   parm                    edate
004400010810     C                   parm                    rec_opt
004500010220     c
004600010810     **
004700010810     c                   if        rec_opt = 'Y'
004800010810     C                   eval      *in66 = *off
004900010810     C                   else
005000010810     C                   eval      *in66 = *on
005100010810     C                   endif
005200010810     ** write header to report once
005300010226     c                   if        not(*in88)
005400010213     C                   exsr      aainit
005500010213     C                   eval      *in88 = *on
005600010213     C                   endif
005700010810
005800010213     ** write header for report overflow
005900010213     c                   if        *inof
006000010112     c                   except    hdr
006100010810     c   66              except    dept_hdr
006200010810     c                   except    hdr1
006300010215     c                   except    det
006400010221     c                   endif
006500010221     **
006600010821     C                   if        dttcod <> last_trns_cde
006700010821     C                   eval      last_trns_cde = dttcod
006800010213     c                   endif
006900010821     C                   if        dtpcod <> last_product
007000010821     C                   exsr      prod_lvl_calc
007100010821     c                   endif
007200010116     C                   exsr      calc_totals
007300010116     **
007400010213     C                   if        dtqty <> 0
007500010112     c
007600010221     ** print transaction record
007700010226     c                   except    det
007800010226     c                   endif
007900010226     C
008000010221     ** read next record
008100010221     **
008200010213     Clr                 exsr      prod_lvl_calc
008300010116     Clr                 exsr      final_totals
008400010213     c
008500010116     *****************************************************************
008600010116     ** Begin subroutine to print header
008700010116     *****************************************************************
008800010116     C     AAINIT        BEGSR
008900010821     C                   eval      coname = get_coname
009000010815     C                   eval      @date = get_sys_date
009100010116     C                   TIME                    @TIME             6 0
009200010116     C                   MOVEL     ARR(1)        @TITLE
009300010213     **
009400010213     C                   open      qsysprt
009500010213     c                   except    hdr
009600010810     c   66              except    dept_hdr
009700010810     c                   except    hdr1
009800010116     C
009900010116     C                   ENDSR
010000010116     ***********************************************************
010100010116     C     final_totals  begsr
010200010221     ** print total issues
010300010116     c                   except    protot
010400010116     c                   except    final
010500010213     C                   close     qsysprt
010600010208     c                   endsr
010700010115     ********************************************************
010800010123     C     prod_lvl_calc begsr
010900010124     C
011000010124     C                   if        last_product <> *blanks
011100010116     **
011200010123     C                   except    protot
011300010116     **
011400010124     c                   endif
011500010123     c                   eval      *in77 = *off
011600010123     c                   eval      last_product = dtpcod
011700010813     c                   eval      pddes# = pddes
011800010123     C                   endsr
011900990617     ********************************************************
012000010116     C     calc_totals   begsr
012100010123     c
012200010810     ** Get prodct Quantity
012300010810     C                   eval      prod_qty = prod_qty + dtqty
012400010810     ** Set indicator for printing debit /credit column
012500010116     C                   if        dtqty > 0
012600010116     C                   eval      *in99 = *on
012700010123     C                   eval      fin_tot = fin_tot - dtamt
012800010123     C                   eval      pro_tot = pro_tot - dtamt
012900010213     c                   eval      crd_amt = dtamt * -1
013000010213     c*                  eval      dbt_amt = *zeros
013100010810
013200010810     C                   if        rec_opt = 'Y'
013300010821     C                   eval      dept#  = dtshp
013400010821     C                   eval      gl_account = dtacct
013500010810     C                   eval      gl_spc = dtspc
013600010810     C                   else
013700010821     C                   eval      dept#  = 3
013800010810     C
013900010116     C                   eval      gl_account = dtacctc
014000010116     C                   eval      gl_spc = dtspcc
014100010810     C                   endif
014200010213     c
014300010821     c                   if        crd_amt <> *zeros
014400010213     c                   add       crd_amt       tot_crd          11 2
014500010213     c                   endif
014600010116     C                   else
014700010116     C                   if        dtqty < 0
014800010116     C                   eval      *in99 = *off
014900010123     C                   eval      fin_tot = fin_tot + dtamt
015000010123     C                   eval      pro_tot = pro_tot + dtamt
015100010202     c                   eval      crd_amt = dtamt
015200010213     c*                  eval      crd_amt = *zeros
015300010810
015400010810     C                   if        rec_opt = 'Y'
015500010810     C                   eval      gl_account = dtacctc
015600010810     C                   eval      gl_spc = dtspcc
015700010810     C                   else
015800010810     C
015900010810     C                   eval      gl_account = dtacct
016000010810     C                   eval      gl_spc = dtspc
016100010810     C                   endif
016200010213     c
016300010813     c                   if        crd_amt <> *zeros
016400010213     c                   add       crd_amt       tot_dbt          11 2
016500010213     c                   endif
016600010213     c
016700010116     C                   endif
016800010116     C                   endif
016900010116     C                   endsr
017000010213     c
017100010116     ********************************************************
017200980817     OQSYSPRT   E            HDR            1 01
017300980817     O                       @PGM                10
017400010116     O                       CONAME              88
017500010220     O                       @time              130 '  :  :  '
017600980817     OQSYSPRT   E            HDR            2
017700980817     O                       @USER               10
017800010116     O                       @TITLE              92
017900010810     O                       @date              130
018000010117     oqsysprt   e            hdr            2
018100010220     O                                           70 'REPORT ON TRANSACTIONS FOR'
018200010810     o                                         +  1 'For Period'
018300010810     o                       sdate             +  1 '    /  /  '
018400010810     o                                         +  1 ' - '
018500010810     o                       edate             +  1 '    /  /  '
018600010821     O                                          126 'Page:'
018700010220     O                       PAGE          Z    130
018800010821     oqsysprt   e            Trans_hdr      2
018900010821     O                                           70 'Transaction Type:'
019000010821     o                       tytxt#              +3
019100010821     oqsysprt   e            dept_hdr       2
019200010821     O                                           70 'Transaction Department:'
019300010821     o                       dtdes               +3
019400010810     oqsysprt   e            hdr1           2
019500010220     o                                            5 'DATE'
019600010220     O                                           24 'ACCOUNT CODE'
019700010821     O                                           35 'Trans No'
019800010821     O                                           41 'Dept No'
019900010220     O                                           60 'PARTICULARS'
020000010220     O                                          103 'DEBITS'
020100010220     o                                          125 'CREDITS'
020200010112     o
020300010111     O****************
020400010111     o**OUTPUT DETAILS
020500010220     oqsysprt   e            det            1
020600010112     O                       dtdate              10 '    /  /  '
020700010112     O                       gl_account        +  2
020800010111     O                                         +  1 '-'
020900010112     O                       gl_spc            +  1
021000010117     O                       dttrn#        4   +  3
021100010112     O                       dtqty         4   +  3
021200010821     O                       dept#         z   +  3
021300010813     O                       last_product      +  1
021400010111     O                                         +  1 '-'
021500010813     O                       pddes#            +  2
021600010119     O              n99      dtamt         j   +  5
021700010119     o               99      dtamt         j    127
021800010112     o
021900010115     oqsysprt   e            protot         2
022000010810     o                                           30 'Product Total->'
022100010813     o                       prod_qty      jb    45
022200010813     o                       pro_tot       jb   107
022300010117     o                                          + 2 '*'
022400010112     oqsysprt   e            final          2
022500010117     o                                          109 'Final Total->'
022600010112     o                       fin_tot       j    + 3
022700010117     o                                          + 2 '**'
022800010116     **
022900010116**   Header to be displayed
023000010221      DEPARTMENTS PURCHASE LISTING
