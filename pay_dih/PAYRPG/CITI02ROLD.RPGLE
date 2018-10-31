000100991204     H
000200091012      //*****  PROGRAMMER - S. BRITTON
000300091012      //*****  First Coded - 00/06/19
000400091012     Fbnkpysuml4uf   e           k disk    usropn
000500091012     Fshrpysuml4uf   e           k disk    usropn
000600100125     Fcuspysuml4uf   e           k disk    usropn
000700000707     Fcitidefn  If   e           k disk
000800091012
000900000707     Daccount_fld      ds
001000000707     D payacc
001100090618     D  payarr                        1    overlay(payacc) dim(30)
001200091012
001300000707     DCiti_acc_fld     ds
001400000707     D bankacc                        9  0
001500000707     D bankarr                        1    overlay(bankacc) dim(9)
001600091012
001700091012     Dciti_err         ds                  template
001800000711     D e_payemp                       6  0
001900090618     D e_payacc                      30
002000000711     D e_paycrd                       6  0
002100000711     D e_payname                     26
002200091012
002300000711     D notfound_txt    c                   const('EMPLOYEE NOT FOUND')
002400000707     D digits          c                   '0123456789'
002500090618     D xcnt            s              2  0 inz(30)
002600000707     D ycnt            s              2  0 inz(9)
002700091012
002800091012      //-----------------------------------------
002900091012      // Prototype for procedure: Main_pgm
003000091012      //-----------------------------------------
003100091012     D  main_pgm       pr                  extpgm('CITI02R')
003200091012     D   citi_errDS                        likeds(citi_err) dim(100)
003300091012     D   err_count                    6  0
003400091012     D   eof                           n
003500091012     D   count                        6  0
003600091012     D   bnk_type                     2  0
003700091012     D   origin                       3
003800091012
003900091012     D  main_pgm       pi
004000091012     D   citi_errDS                        likeds(citi_err) dim(100)
004100091012     D   err_count                    6  0
004200091012     D   eof                           n
004300091012     D   count                        6  0
004400091012     D   bnk_type                     2  0
004500091012     D   origin                       3
004600000707
004700091012      // Get Division and Bank account information from file
004800091012      /FREE
004900091012       select;
005000091012         // .. for payroll
005100091012         when origin = 'PAY';
005200091012           if count = 0;
005300091012             open(e) bnkpysuml4;
005400091012             chain(E) bnk_type payfmt;
005500091012             *IN77 = NOT %FOUND OR %ERROR;
005600091012            else;
005700091012             reade(E) bnk_type payfmt;
005800091012             *IN77 = %ERROR;
005900091012             *IN77 = %EOF;
006000091012           endif;
006100091012
006200091012         // .. for share dividends
006300091012         when origin = 'SHR';
006400091012           if count = 0;
006500091012             open(e) shrpysuml4;
006600091012             chain(E) bnk_type sbkfmt;
006700091012             *IN77 = NOT %FOUND OR %ERROR;
006800091012            else;
006900091012             reade(E) bnk_type sbkfmt;
007000091012             *IN77 = %ERROR;
007100091012             *IN77 = %EOF;
007200091012           endif;
007300100125         when origin = 'CRD';
007400100125           if count = 0;
007500100125             open(e) cuspysuml4;
007600100125             chain(E) bnk_type cbkfmt;
007700100125             *IN77 = NOT %FOUND OR %ERROR;
007800100125            else;
007900100125             reade(E) bnk_type cbkfmt;
008000100125             *IN77 = %ERROR;
008100100125             *IN77 = %EOF;
008200100125           endif;
008300091012       endsl;
008400091012
008500091012       if not(*in77);
008600091012         // get accounting info.
008700091012         Exsr get_bank_acc;
008800091012         chain bankacc racmst;
008900091012
009000091012         if %found and acmsts = 'A';
009100091012
009200091012           select;
009300091012             when origin = 'PAY';
009400091012               paysts = 'V';
009500091012               update payfmt;
009600091012             when origin = 'SHR';
009700091012               sbksts = 'V';
009800091012               update sbkfmt;
009900100125             when origin = 'CRD';
010000100125               cbksts = 'V';
010100100125               update cbkfmt;
010200091012           endsl;
010300091012
010400091012         else;
010500091012           err_count += 1;
010600091012
010700091012           select;
010800091012             when origin = 'PAY';
010900091012               citi_errDS(err_count).e_payname = paybname;
011000091012               citi_errDS(err_count).e_payacc = payacc;
011100091012               citi_errDS(err_count).e_payemp = payemp;
011200091012               citi_errDS(err_count).e_paycrd = paycrd;
011300091012
011400091012             when origin = 'SHR';
011500091012               citi_errDS(err_count).e_payname = sbkbname;
011600091012               citi_errDS(err_count).e_payacc = sbkacc;
011700091012               citi_errDS(err_count).e_payemp = sbkshno;
011800091012               citi_errDS(err_count).e_paycrd = sbkcrd;
011900100125             when origin = 'CRD';
012000100125               citi_errDS(err_count).e_payname = cbkbname;
012100100125               citi_errDS(err_count).e_payacc = cbkacc;
012200100125               citi_errDS(err_count).e_payemp = cbkshno;
012300100125               citi_errDS(err_count).e_paycrd = cbkcrd;
012400091012           endsl;
012500091012
012600091012         endif;
012700091012
012800091012       endif;
012900091012
013000091012       if *in77;
013100091012         // err_count -= 1;
013200091012         eof = *on;
013300091012         *inlr = *on;
013400091012       endif;
013500091012
013600091012       return;
013700091012       //***************************************************************
013800091012       // Sub-routine to get bank account for check
013900091012       //***************************************************************
014000091012       begsr get_bank_acc;
014100091012
014200091012         bankacc = *zeros;
014300091012         select;
014400091012           when origin = 'PAY';
014500091012            // already set
014600091012           when origin = 'SHR';
014700091012            payacc = sbkacc;
014800100125           when origin = 'CRD';
014900100125            payacc = cbkacc;
015000091012         endsl;
015100000707
015200091012         // check for number in character field
015300091012         dow xcnt > 0;
015400091012      /END-FREE
015500000707     C     digits        check     payarr(xcnt)                           99
015600091012      // move only numbers to required numeric Bank account field
015700091012      /FREE
015800091012           if not(*in99) and ycnt > 0;
015900091012      /END-FREE
016000000707     C                   move      payarr(xcnt)  bankarr(ycnt)
016100000707     C                   sub       1             ycnt
016200091012      /FREE
016300091012           endif;
016400091012
016500091012      /END-FREE
016600000707     C                   sub       1             xcnt
016700091012      /FREE
016800091012         enddo;
016900091012         // Reset array fields
017000091012         reset xcnt;
017100091012         reset ycnt;
017200091012       endsr;
017300091012      /END-FREE
