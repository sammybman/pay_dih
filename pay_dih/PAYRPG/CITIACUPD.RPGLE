000100991217     H DATEDIT(*DMY)
000200110121      //****** TRANSFER TO BANK ACCOUNT  --- S.BRITTON ******
000300110121      //
000400110121     Fgenpysuml1uf   e           k disk    commit
000500000706     Fbnkpylog  uf a e           k disk    commit
000600000630     Fcitibtch  o    e           k disk
000700000630     Fcitidefn  If   e           k disk
000800000706     Fcitirept  o    f  132        printer
000900000701
001000110121      // SubProcedure PROTOTYPE
001100110121      //
001200000629     Dtrans_up         pr
001300000630     D citi_t#                             like(citi_t)
001400000629     D citi_a#                             like(citi_a)
001500000629     D amount#                             like(amount)
001600000630     D date_crd#                           like(paycrd)
001700000629     D count#                              like(count)
001800000629     D trn_type#                           like(trn_type)
001900000630     D last_trn_flag#                      like(last_trn_flag)
002000110121      //
002100950930     D                SDS
002200950930     D  @PGM             *PROC
002300950930     D  @USER                254    263
002400110121      //
002500000701     Daccount_fld      ds
002600000701     D payacc
002700000701     D  payarr                        1    overlay(payacc) dim(12)
002800110121      //
002900000701     DCiti_acc_fld     ds
003000000701     D bankacc                        9  0
003100000701     D bankarr                        1    overlay(bankacc) dim(9)
003200110121      //
003300000630     Dlast_trn_flag    s               n   inz(*off)
003400000629     Dtrn_type         s                   like(winsta)
003500000629     Damount           s             12  2
003600000630     Dsalary_acc       s              9  0 inz(119000124)
003700000701     d digits          c                   '0123456789'
003800000701     D xcnt            s              2  0 inz(12)
003900000701     D ycnt            s              2  0 inz(9)
003901110121     D origin          s              3    inz('PAY')
004000110121      //
004100000717     Dciti_t         e ds                  extname(citibtch) inz
004200000630     Dciti_a         e ds                  extname(citidefn) inz
004300110121         //
004400110121        //*   *entry        plist
004500110121        //*                 parm                    pbtype            1
004600110121        //
004700000628     C                   z-add     6             dtno              2 0
004800110121        //
004900110121        //-------------------------------------------------------------------------
005000110121        // Get Division and Bank account information from file
005001110121      /FREE
005100110121          chain (origin:dtno) payfmt;
005101110121          *IN88 = NOT %FOUND;
005200110121          //-------------------------------------------------------------------------
005300990201
005400110121         // If job is restarted - restore last seq# & total values
005500110121           chain dtno paylgfmt;
005501110121         *IN77 = NOT %FOUND;
005600110121         //
005700110121           if not(*in77);
005701110121      /END-FREE
005800000706     C                   z-add     logcnt        count
005900000706     C                   z-add     logamt        totpay
005901110121      /FREE
006000110121         endif;
006100110121         //
006200110121         if not(*in88) or not(*in77);
006300110121          //
006400110121           dow not(*in88);
006500990413
006600110121           //
006700110121           //
006800110121           IF payamt > *zeros;
006801110121      /END-FREE
006900000629     C                   add       payamt        totpay           11 2
007000000630
007001110121      /FREE
007100110121             Exsr get_bank_acc;
007200110121             // get accounting info.
007300110121             chain bankacc racmst;
007400110121             //
007500110121             if %found;
007501110121      /END-FREE
007600000630     C                   add       1             count             4 0
007700000629     C                   z-add     payamt        amount
007800000630     C                   move      'P'           paysts
007801110121      /FREE
007900110121               update payfmt;
008000110121               Exsr write_dtrn;
008100110121               Exsr write_logtrn;
008200110121               //
008300110121               commit;
008400110121               //
008500110121             else;
008600110121             endif;
008700000630
008800110121           endif;
008900110121           //
009000110121           reade (origin:dtno) payfmt;
009001110121           *IN88 = %EOF;
009100110121         enddo;
009200000630
009300110121         // write final transaction record
009400110121         // get accounting info. for Banks DiH debit total
009500110121         chain salary_acc racmst;
009501110121      /END-FREE
009600000630     C                   add       1             count             4 0
009700000629     C                   z-add     totpay        amount
009701110121      /FREE
009800110121         last_trn_flag = *on;
009900110121         Exsr write_dtrn;
010000000630
010100110121         // write header record
010200110121         Exsr write_htrn;
010300110121         //
010301110121      /END-FREE
010400990114     C                   z-add     *zeros        totpay
010500000517     C                   z-add     *zeros        count
010501110121      /FREE
010600110121         Exsr delete_logtrn;
010700110121         commit;
010800110121       endif;
010900000629
011000110121       *INlr = *ON;
011100110121       return;
011200990413
011300110121       //***************************************************************
011400110121       // Sub-routine to Handle all transaction recrds
011500110121       //***************************************************************
011600110121       begsr write_dtrn;
011601110121      /END-FREE
011700000629     C                   move      'T'           trn_type
011701110121      /FREE
011800110121         TRANS_UP(citi_t:citi_a:amount:paycrd:
011900110121             count:trn_type:last_trn_flag);
012000110121         write(E) rwinpt;
012001110121         *IN99 = %ERROR;
012002110121      /END-FREE
012100000706     C   99              exsr      roll_back
012101110121      /FREE
012200110121       endsr;
012300110121       //***************************************************************
012400110121       // Sub-routine to Handle header record
012500110121       //***************************************************************
012600110121       begsr write_htrn;
012601110121      /END-FREE
012700000629     C                   move      'B'           trn_type
012701110121      /FREE
012800110121         clear citi_a;
012900110121         //
013000110121         TRANS_UP(citi_t:citi_a:amount:paycrd:
013100110121             count:trn_type:last_trn_flag);
013200110121         write(E) rwinpt;
013201110121         *IN99 = %ERROR;
013202110121      /END-FREE
013300000706     C   99              exsr      roll_back
013301110121      /FREE
013400110121       endsr;
013500110121       //***************************************************************
013600110121       // Sub-routine to write transaction log record
013700110121       //***************************************************************
013800110121       begsr write_log_trn;
013900110121         chain dtno paylgfmt;
014000110121         //
014001110121      /END-FREE
014100000706     C                   z-add     count         logcnt
014200000706     C                   z-add     totpay        logamt
014300110121      //
014301110121      /FREE
014400110121         if %found;
014500110121           update paylgfmt;
014600110121         else;
014700110121           //
014701110121      /END-FREE
014800000706     C                   z-add     dtno          logdiv
014900000706     C                   z-add     paycrd        logcrd
014901110121      /FREE
015000110121           write paylgfmt;
015100110121           //
015200110121         endif;
015300110121       endsr;
015400110121       //***************************************************************
015500110121       // Sub-routine to delete transaction log record
015600110121       //***************************************************************
015700110121       begsr delete_log_trn;
015800110121         chain dtno paylgfmt;
015900110121         //
016000110121         if %found;
016100110121           delete paylgfmt;
016200110121         endif;
016300110121         //
016400110121       endsr;
016500110121       //***************************************************************
016600110121       // Sub-routine to get bank account for check
016700110121       //***************************************************************
016800110121       begsr get_bank_acc;
016801110121      /END-FREE
016900000701     C                   z-add     *zeros        bankacc
017000000701
017100110121      // check for number in character field
017101110121      /FREE
017200110121         dow xcnt > 0;
017201110121      /END-FREE
017300000701     C     digits        check     payarr(xcnt)                           99
017400110121      // move only numbers to required numeric Bank account field
017401110121      /FREE
017500110121           if not(*in99) and ycnt > 0;
017501110121      /END-FREE
017600000701     C                   move      payarr(xcnt)  bankarr(ycnt)
017700000701     C                   sub       1             ycnt
017701110121      /FREE
017800110121           endif;
017900110121           //
017901110121      /END-FREE
018000000701     C                   sub       1             xcnt
018001110121      /FREE
018100110121         enddo;
018200110121         // Reset array fields
018300110121         reset xcnt;
018400110121         reset ycnt;
018500110121       endsr;
018600110121       //***************************************************************
018700110121       // roll back Sub-routine
018800110121       //***************************************************************
018900110121       begsr roll_back;
019000110121         //
019100110121         rolbk;
019200110121         //
019300110121       endsr;
019400110121       //*********************************************************************
019500110121       //* SubProcedure to populate citizen Bank transaction file  ***
019600110121       //*********************************************************************
019601110121      /END-FREE
019700000629      /copy CITIUPDP#
