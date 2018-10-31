000100991217     H DATEDIT(*DMY)
000200000629     F******* TRANSFER TO BANK ACCOUNT  --- S.BRITTON ******
000300990201     **
000400000706     Fbnkpysuml1uf   e           k disk    commit
000500000706     Fbnkpylog  uf a e           k disk    commit
000600000630     Fcitibtch  o    e           k disk
000700000630     Fcitidefn  If   e           k disk
000800000706     Fcitirept  o    f  132        printer
000900000701
001000000629     ** SubProcedure PROTOTYPE
001100000629     **
001200000629     Dtrans_up         pr
001300000630     D citi_t#                             like(citi_t)
001400000629     D citi_a#                             like(citi_a)
001500000629     D amount#                             like(amount)
001600000630     D date_crd#                           like(paycrd)
001700000629     D count#                              like(count)
001800000629     D trn_type#                           like(trn_type)
001900000630     D last_trn_flag#                      like(last_trn_flag)
002000000629     **
002100950930     D                SDS
002200950930     D  @PGM             *PROC
002300950930     D  @USER                254    263
002400000629     **
002500000701     Daccount_fld      ds
002600000701     D payacc
002700000701     D  payarr                        1    overlay(payacc) dim(12)
002800990114     **
002900000701     DCiti_acc_fld     ds
003000000701     D bankacc                        9  0
003100000701     D bankarr                        1    overlay(bankacc) dim(9)
003200000701     **
003300000630     Dlast_trn_flag    s               n   inz(*off)
003400000629     Dtrn_type         s                   like(winsta)
003500000629     Damount           s             12  2
003600000630     Dsalary_acc       s              9  0 inz(119000124)
003700000701     d digits          c                   '0123456789'
003800000701     D xcnt            s              2  0 inz(12)
003900000701     D ycnt            s              2  0 inz(9)
004000000629     **
004100000717     Dciti_t         e ds                  extname(citibtch) inz
004200000630     Dciti_a         e ds                  extname(citidefn) inz
004300991207     **
004400000629     C**   *entry        plist
004500000629     C**                 parm                    pbtype            1
004600000629     **
004700000628     C                   z-add     6             dtno              2 0
004800960409     **
004900990413     **-------------------------------------------------------------------------
005000000701     ** Get Division and Bank account information from file
005100000629     C     dtno          chain     payfmt                             88
005200990412     **-------------------------------------------------------------------------
005300990201
005400000706     ** If job is restarted - restore last seq# & total values
005500000706     C     dtno          chain     paylgfmt                           77
005600000706     **
005700000706     C                   if        not(*in77)
005800000706     C                   z-add     logcnt        count
005900000706     C                   z-add     logamt        totpay
006000000706     C                   endif
006100990201     **
006200000706     C                   if        not(*in88) or not(*in77)
006300000706     **
006400000629     C                   dow       not(*in88)
006500990413
006600990114     **
006700971128     **
006800000629     C     payamt        ifgt      *zeros
006900000629     C                   add       payamt        totpay           11 2
007000000630
007100000701     C                   Exsr      get_bank_acc
007200000629     ** get accounting info.
007300000629     C     bankacc       chain     racmst
007400000630     **
007500000630     C                   if        %found
007600000630     C                   add       1             count             4 0
007700000629     C                   z-add     payamt        amount
007800000630     C                   move      'P'           paysts
007900000630     C                   update    payfmt
008000000629     C                   Exsr      write_dtrn
008100000706     C                   Exsr      write_logtrn
008200000706     **
008300000706     C                   commit
008400000630     **
008500000630     C                   else
008600000630     C                   endif
008700000630
008800000629     C                   endif
008900971128     **
009000000629     C     dtno          reade     payfmt                                 88
009100000208     C                   enddo
009200000630
009300000629     ** write final transaction record
009400000630     ** get accounting info. for Banks DiH debit total
009500000630     C     salary_acc    chain     racmst
009600000630     C                   add       1             count             4 0
009700000629     C                   z-add     totpay        amount
009800000630     C                   eval      last_trn_flag = *on
009900000629     C                   Exsr      write_dtrn
010000000630
010100000629     ** write header record
010200000629     C                   Exsr      write_htrn
010300990114     **
010400990114     C                   z-add     *zeros        totpay
010500000517     C                   z-add     *zeros        count
010600000706     C                   Exsr      delete_logtrn
010700000706     C                   commit
010800990114     C                   endif
010900000629
011000000629     C                   seton                                        lr
011100000629     C                   return
011200990413
011300000701     *****************************************************************
011400000630     ** Sub-routine to Handle all transaction recrds
011500000701     *****************************************************************
011600000629     C     write_dtrn    begsr
011700000629     C                   move      'T'           trn_type
011800000630     C                   callp     TRANS_UP(citi_t:citi_a:amount:paycrd:
011900000630     C                                      count:trn_type:last_trn_flag)
012000000630     C                   write     rwinpt                               99
012100000706     C   99              exsr      roll_back
012200000629     C                   endsr
012300000701     *****************************************************************
012400000630     ** Sub-routine to Handle header record
012500000701     *****************************************************************
012600000629     C     write_htrn    begsr
012700000629     C                   move      'B'           trn_type
012800000630     C                   clear                   citi_a
012900000630     **
013000000630     C                   callp     TRANS_UP(citi_t:citi_a:amount:paycrd:
013100000630     C                                      count:trn_type:last_trn_flag)
013200000630     C                   write     rwinpt                               99
013300000706     C   99              exsr      roll_back
013400000629     C                   endsr
013500000706     *****************************************************************
013600000706     ** Sub-routine to write transaction log record
013700000706     *****************************************************************
013800000706     C     write_log_trn begsr
013900000706     C     dtno          chain     paylgfmt
014000000706     **
014100000706     C                   z-add     count         logcnt
014200000706     C                   z-add     totpay        logamt
014300000706     **
014400000706     C                   if        %found
014500000706     C                   update    paylgfmt
014600000706     C                   else
014700000706     **
014800000706     C                   z-add     dtno          logdiv
014900000706     C                   z-add     paycrd        logcrd
015000000706     C                   write     paylgfmt
015100000706     **
015200000706     C                   endif
015300000706     C                   endsr
015400000706     *****************************************************************
015500000706     ** Sub-routine to delete transaction log record
015600000706     *****************************************************************
015700000706     C     delete_log_trnbegsr
015800000706     C     dtno          chain     paylgfmt
015900000706     **
016000000706     C                   if        %found
016100000706     C                   delete    paylgfmt
016200000706     C                   endif
016300000706     **
016400000706     C                   endsr
016500000701     *****************************************************************
016600000701     ** Sub-routine to get bank account for check
016700000701     *****************************************************************
016800000701     C     get_bank_acc  begsr
016900000701     C                   z-add     *zeros        bankacc
017000000701
017100000701     ** check for number in character field
017200000701     C                   dow       xcnt > 0
017300000701     C     digits        check     payarr(xcnt)                           99
017400000701     ** move only numbers to required numeric Bank account field
017500000701     C                   if        not(*in99) and ycnt > 0
017600000701     C                   move      payarr(xcnt)  bankarr(ycnt)
017700000701     C                   sub       1             ycnt
017800000701     C                   endif
017900000701     **
018000000701     C                   sub       1             xcnt
018100000701     C                   enddo
018200000701     ** Reset array fields
018300000701     C                   reset                   xcnt
018400000701     C                   reset                   ycnt
018500000701     C                   endsr
018600000706     *****************************************************************
018700000706     ** roll back Sub-routine
018800000706     *****************************************************************
018900000706     C     roll_back     begsr
019000000706     **
019100000706     C                   rolbk
019200000706     **
019300000706     C                   endsr
019400000629     ***********************************************************************
019500000629     *** SubProcedure to populate citizen Bank transaction file  ***
019600000629     ***********************************************************************
019700000629      /copy CITIUPDP#
