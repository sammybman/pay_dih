000100000000     H DATEDIT(*DMY)
000200000000      //****** TRANSFER TO BANK ACCOUNT  --- S.BRITTON ******
000300000000      //
000400000000     Fbnkpysuml1uf   e           k disk    commit  usropn
000500091012     Fshrpysuml1uf   e           k disk    commit  usropn
000600100125     Fcuspysuml1uf   e           k disk    commit  usropn
000700000000     Fbnkpylogl1uf a e           k disk    commit  usropn
000800000000     Fcitibtch  uf a e           k disk    commit  usropn
000900000000     Fcitidefn  If   e           k disk
001000091012
001100000000      //itirept  o    f  132        printer
001200000000
001300000000      // SubProcedure PROTOTYPE
001400000000     Dtrans_up         pr
001500000000     D citi_t#                             like(citi_t)
001600000000     D citi_a#                             like(citi_a)
001700000000     D amount#                             like(amount)
001800000000     D date_crd#                           like(paycrd)
001900000000     D count#                              like(count)
002000000000     D trn_type#                           like(trn_type)
002100000000     D last_trn_flag#                      like(last_trn_flag)
002200000000     D batch#                              like(batch)
002300091012
002400000000     D                SDS
002500000000     D  @PGM             *PROC
002600000000     D  @USER                254    263
002700091012
002800000000     Daccount_fld      ds
002900000000     D payacc
003000090618     D  payarr                        1    overlay(payacc) dim(30)
003100091012
003200000000     DCiti_acc_fld     ds
003300000000     D bankacc                        9  0
003400000000     D bankarr                        1    overlay(bankacc) dim(9)
003500091012
003600000000     D                 ds
003700000000     D  bankAccDS                    25
003800000000     D  salary_Acc                    9  0 overlay(bankAccDS)
003900091012
004000000000     Dlast_trn_flag    s               n   inz(*off)
004100000000     Dtrn_type         s                   like(winsta)
004200000000     Damount           s             12  2
004300000000     d digits          c                   '0123456789'
004400090618     D xcnt            s              2  0 inz(30)
004500000000     D ycnt            s              2  0 inz(9)
004600000000     D batch           s              4  0
004700000000     D count           s              6  0
004800031210     D procesInd       s               n
004900000000     D totpay          s             11  2
005000091012
005100000000     Dciti_t         e ds                  extname(citibtch) inz
005200000000     Dciti_a         e ds                  extname(citidefn) inz
005300091012
005400091012
005500091012     D*-----------------------------------------
005600091012     D* Prototype for procedure: Main_pgm
005700091012     D*-----------------------------------------
005800091012     D  main_pgm       pr                  extpgm('CITI04R')
005900091012     D   bnk_type                     2  0
006000091012     D   countin                      6  0
006100091012     D   eof                           n
006200091012     D   tot_recs                     6  0
006300091012     D   err_ind                      1
006400091012     D   btchlower                    6  0
006500091012     D   btchupper                    6  0
006600091012     D   bankaccount                 25
006700091012     D   origin                       3
006800091012
006900091012     D  main_pgm       pi
007000091012     D   bnk_type                     2  0
007100091012     D   countin                      6  0
007200091012     D   eof                           n
007300091012     D   tot_recs                     6  0
007400091012     D   err_ind                      1
007500091012     D   btchlower                    6  0
007600091012     D   btchupper                    6  0
007700091012     D   bankaccount                 25
007800091012     D   origin                       3
007900091012
008000000000
008100000000      /FREE
008200000000       bankaccDS = bankaccount;
008300000000
008400031210       if procesInd = *off;
008500031210         procesInd = *on;
008600031210         count = *zeros;
008700000000       endif;
008800000000       //
008900000000       if count = 0;
009000000000         open bnkpylogl1;
009100000000         open citibtch;
009200000000         //-------------------------------------------------------------------------
009300000000         // Get Bank account information from file for division 6- citizen bank
009400091012         select;
009500091012           when origin = 'PAY';
009600091012             open bnkpysuml1;
009700091012             chain bnk_type payfmt;
009800091012           when origin = 'SHR';
009900091012             open shrpysuml1;
010000091012             chain bnk_type sbkfmt;
010100100125           when origin = 'CRD';
010200100125             open cuspysuml1;
010300100125             chain bnk_type cbkfmt;
010400091012         endsl;
010500000000         *IN88 = NOT %FOUND;
010600000000         //-------------------------------------------------------------------------
010700000000
010800000000         // If job is restarted - restore last seq# & total values
010900091012         chain (bnk_type:origin) paylgfmt;
011000000000         *IN77 = NOT %FOUND;
011100091012
011200000000         if not(*in77);
011300000000           count = logcnt;
011400000000           totpay = logamt;
011500000000           batch = logbtch;
011600031210           tot_recs = logrecn;
011700000000           exsr vry_restr_seq#;
011800000000         else;
011900000000           exsr get_next_btch#;
012000000000         endif;
012100000000
012200000000       else;
012300091012         select;
012400091012           when origin = 'PAY';
012500091012             reade bnk_type payfmt;
012600091012           when origin = 'SHR';
012700091012             reade bnk_type sbkfmt;
012800100125           when origin = 'CRD';
012900100125             reade bnk_type cbkfmt;
013000091012         endsl;
013100000000         *IN88 = %EOF;
013200000000       endif;
013300091012
013400000000       if not(*in88) or not(*in77);
013500091012
013600000000         if not(*in88);
013700091012
013800091012           select;
013900091012             when origin = 'PAY';
014000091012               // fields ok
014100091012             when origin = 'SHR';
014200091012               payamt = sbkamt;
014300091012               paycrd = sbkcrd;
014400100125             when origin = 'CRD';
014500100125               payamt = cbkamt;
014600100125               paycrd = cbkcrd;
014700091012           endsl;
014800091012
014900000000           if payamt > *zeros;
015000091012             totpay += payamt;
015100000000
015200000000             Exsr get_bank_acc;
015300000000             // get accounting info.
015400000000             chain(E) bankacc racmst;
015500030606
015600030606           if  %error;
015700030606             exsr roll_back;
015800030606           endif;
015900030606
016000000000             if %found;
016100090618               count += 1;
016200000000               amount = payamt;
016300000000
016400000000               // Print list of non active accounts on Remote system
016500000000               if acmsts <> 'A';
016600091012                 //  except  detail;
016700000000               endif;
016800091012
016900091012               select;
017000091012                 when origin = 'PAY';
017100091012                   paysts = 'P';
017200091012                   update payfmt;
017300091012                 when origin = 'SHR';
017400091012                   sbksts = 'P';
017500091012                   update sbkfmt;
017600100125                 when origin = 'CRD';
017700100125                   cbksts = 'P';
017800100125                   update cbkfmt;
017900091012               endsl;
018000000000               Exsr write_dtrn;
018100000000               Exsr write_log_trn;
018200000000               //
018300000000               commit;
018400091012
018500000000             else;
018600000000             endif;
018700000000
018800000000           endif;
018900000000           //
019000000000         endif;
019100000000
019200000000         if count = tot_Recs;
019300000000           // write final transaction record
019400000000           // get accounting info. for debit total
019500000000           chain salary_acc racmst;
019600090618           count += 1;
019700000000           amount = totpay;
019800000000           last_trn_flag = *on;
019900000000           Exsr write_dtrn;
020000000000
020100000000           // write header record
020200000000           Exsr write_htrn;
020300000000           //
020400000000           totpay = *zeros;
020500000000           count = *zeros;
020600000000           Exsr delete_log_trn;
020700000000           commit;
020800091012
020900000000           eof = *on;
021000030606           *inlr = *ON;
021100091012           close(e) bnkpysuml1;
021200091012           close(e) shrpysuml1;
021300100125           close(e) cuspysuml1;
021400000000           close bnkpylogl1;
021500000000           close citibtch;
021600000000         endif;
021700000000
021800000000       endif;
021900000000       return;
022000000000
022100000000       //***************************************************************
022200000000       // Sub-routine to Handle all transaction recrds
022300000000       //***************************************************************
022400000000       begsr write_dtrn;
022500000000         trn_type = 'T';
022600000000         TRANS_UP(citi_t:citi_a:amount:paycrd:
022700000000             count:trn_type:last_trn_flag:
022800000000             batch);
022900000000         write(E) rwinpt;
023000000000         *IN99 = %ERROR;
023100000000      /END-FREE
023200000000     C   99              exsr      roll_back
023300000000      /FREE
023400000000       endsr;
023500091012
023600000000       //***************************************************************
023700000000       // Sub-routine to Handle header record
023800000000       //***************************************************************
023900000000       begsr write_htrn;
024000000000         trn_type = 'B';
024100000000         clear citi_a;
024200000000         //
024300000000         TRANS_UP(citi_t:citi_a:amount:paycrd:
024400000000             count:trn_type:last_trn_flag:
024500000000             batch);
024600000000         write(E) rwinpt;
024700000000         *IN99 = %ERROR;
024800000000      /END-FREE
024900000000     C   99              exsr      roll_back
025000000000      /FREE
025100000000       endsr;
025200000000       //***************************************************************
025300000000       // Sub-routine to write transaction log record
025400000000       //***************************************************************
025500000000       begsr write_log_trn;
025600091012         chain (bnk_type:origin) paylgfmt;
025700091012
025800000000         logrecn = tot_recs;
025900000000         logcnt = count;
026000000000         logamt = totpay;
026100091012
026200000000         if %found;
026300000000           update paylgfmt;
026400000000         else;
026500091012
026600000000           logdiv = bnk_type;
026700000000           logcrd = paycrd;
026800000000           logbtch = batch;
026900091012           logorg = origin;
027000000000           write paylgfmt;
027100091012
027200000000         endif;
027300000000       endsr;
027400091012
027500000000       //***************************************************************
027600000000       // Sub-routine to delete transaction log record
027700000000       //***************************************************************
027800000000       begsr delete_log_trn;
027900091012         chain (bnk_type:origin) paylgfmt;
028000091012
028100000000         if %found;
028200000000           delete paylgfmt;
028300000000         endif;
028400091012
028500000000       endsr;
028600000000       //***************************************************************
028700000000       // Sub-routine to get bank account for check
028800000000       //***************************************************************
028900000000       begsr get_bank_acc;
029000000000
029100091012         bankacc = *zeros;
029200091012         select;
029300091012           when origin = 'PAY';
029400091012            // already set
029500091012           when origin = 'SHR';
029600091012            payacc = sbkacc;
029700100125           when origin = 'CRD';
029800100125            payacc = cbkacc;
029900091012         endsl;
030000000000
030100000000         // check for number in character field
030200000000
030300000000         dow xcnt > 0;
030400000000      /END-FREE
030500000000     C     digits        check     payarr(xcnt)                           99
030600000000      /FREE
030700000000         // move only numbers to required numeric Bank account field
030800000000           if not(*in99) and ycnt > 0;
030900000000            bankarr(ycnt) = payarr(xcnt);
031000000000            ycnt = ycnt - 1;
031100000000           endif;
031200000000
031300000000          xcnt = xcnt - 1;
031400000000
031500000000         enddo;
031600091012
031700000000         // Reset array fields
031800000000         reset xcnt;
031900000000         reset ycnt;
032000000000       endsr;
032100000000       //***************************************************************
032200000000       // Sub-routine to Get Next Batch Number
032300000000       //***************************************************************
032400000000       begsr get_next_btch#;
032500000000
032600000000         // Select one of range of valid batches  for transaction
032700000000
032800000000         for batch = btchlower to btchupper;
032900000000
033000000000           chain batch rwinpt;
033100000000           if not(%found);
033200000000             leave;
033300000000           else;
033400000000             if batch = btchupper;
033500000000
033600030606               err_ind = '2';
033700030606               *inlr = *on;
033800000000               return;
033900000000             endif;
034000000000           endif;
034100000000         endfor;
034200000000       endsr;
034300000000       //***************************************************************
034400000000       // Sub-routine to Verify last sequence number of batch  written
034500000000       // to remote system
034600000000       //***************************************************************
034700000000       begsr vry_restr_seq#;
034800000000
034900000000         //
035000000000      /END-FREE
035100000000     C     last_seq_key  klist
035200000000     C                   kfld                    batch             4 0
035300000000     C                   kfld                    seq#              4 0
035400000000
035500000000      // Ensure that the next number to be written was not already processed
035600000000      /FREE
035700000000         seq# = logcnt + 1;
035800000000         chain last_seq_key rwinpt;
035900000000         if %found;
036000000000           delete(E) rwinpt;
036100000000           *IN99 = %ERROR;
036200000000      /END-FREE
036300000000     C   99              exsr      roll_back
036400000000      /FREE
036500000000         endif;
036600000000       endsr;
036700000000       //***************************************************************
036800000000       // roll back Sub-routine
036900000000       //***************************************************************
037000000000       begsr roll_back;
037100000000         //
037200000000         err_ind = '1';
037300000000         rolbk;
037400030606         *inlr = *on;
037500000000         return;
037600000000         //
037700000000       endsr;
037800091012
037900000000       //itirept  e            detail
038000000000       //
038100090618       //                      payacc              35
038200000000       //                      acmsts             + 1
038300000000
038400000000       //*********************************************************************
038500000000       //* SubProcedure to populate citizen Bank transaction file  ***
038600000000       //*********************************************************************
038700000000      /copy *libl/payrpg,CITI04RPR
038800000000      /END-FREE
