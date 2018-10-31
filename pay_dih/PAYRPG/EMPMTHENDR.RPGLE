000100050701     H DECEDIT('.') bnddir('DISDIR') dftactgrp(*no) actgrp(*caller)
000200951129     F**
000300050601     Fliaba_ytd uf a e           k disk    commit
000400000518     **
000500030808     D #date           s              8  0
000600040128     D errmsg          s             75
000700030808     D curprno         s              6  0
000800040128     D prno            s              6  0
000900040111     D amount#         s             11  2
001000040111     D ycnt            s              4  0
001100040111     D gcnt            s              4  0
001200040128     D glarr           s             31    dim(500)                             TOTAL AMOUNT
001300050517     D countr          s              5  0                                      TOTAL AMOUNT
001400050517     D cntr            s              5  0                                      TOTAL AMOUNT
001500050517     D payrun_factor   s              7  3 inz(1)
001600050517     D payrun_qty      s              2  0 inz(1)
001700050518     D hrs_dummy       s              5  2
001800050518     D pay_amt         s             11  2
001900050527     D bill_num        s              9  0
002000050517      //  data structure for parameter
002100050517     dprm_rec        e ds                  extname(gen_plist)
002200000518     **
002300960103     D                SDS
002400960103     D  @PGM             *PROC
002500960103     D  @USER                254    263
002600040128     **
002700150305      /copy QCPYSRC,payPR
002800150305      /copy QCPYSRC,genpr
002900150305      /copy QCPYSRC,apppr
003000150305      /copy QCPYSRC,emppr
003100150305      /copy QCPYSRC,glpr
003200040111     **
003300040111     Dgl_accumd        ds                  occurs(500) inz
003400040111     D gl_prd                         6  0
003500040111     D gl_account                    25
003600040111     D gl_desc                       30
003700040111     D gl_total                      13  2
003800040111     D gl_refno                      15
003900040111     **
004000040111     D                 ds
004100040111     D gl_xxx                        31
004200040111     D gl_prno2                       6  0 overlay(gl_xxx)
004300040111     D gl_out                        25    overlay(gl_xxx:7)
004400000518     **
004500050517     D text_           c                   'Allowance for Period - '
004600040111     D year_const      c                   12
004700000518     **
004800040128      /free
004900030808
005000050521           // read all liability types
005100050517           countr = *zeros;
005200050517           dow not get_ltype_info('RL':-1:ematf_struc:countr);
005300040111           // check if valid for processing
005400050518            if empaytype <> *zeros and
005500050518                emnxtprd <= prno;
005600050518
005700050518             emprdcmplt += 1;
005800050602             // Clear file if last period for the year
005900050602              if emprdcmplt = 1;
006000050602                setll (emseltyp:*zeros) lafmt;
006100050602                reade (emseltyp:*zeros) lafmt;
006200050602
006300050602                dow not %eof;
006400050602                 //laamt = *zeros;
006500050602                 //update lafmt %fields(laamt);
006600050602                 delete lafmt;
006700050602                 reade (emseltyp:*zeros) lafmt;
006800050602                enddo;
006900050602
007000050602              endif;
007100050521             // read all employee records
007200050517             cntr = *zeros;
007300050517             dow not get_emp_info2('R':-1:emp_struc:cntr);
007400050518              if get_cat(status) = 'A';
007500050518                pay_amt = (get_emp_pay(emp:dept:empaytype:
007600050518                                 hrs_dummy:payrun_factor:
007700060510                                  payrun_qty:status));
007800060510                            //    payrun_qty:status)) /
007900060510                            //  emapymth;
008000050518
008100050518                if pay_amt <> *zeros;
008200050528                  // get correct amount
008300050528                   amount# = pay_amt / year_const * emapymth;
008400050601
008500050601                 // write transaction
008600050601                   exsr perform_tr_write;
008700050601
008800050601                 // get / summarise GL. info
008900050601                 //if cydwrite = 'Y';
009000050601                    exsr perform_gl_write;
009100050601                 //endif;
009200050601
009300050518                  chain (emseltyp:*zeros:emp) lafmt;
009400050518
009500050518                  // update Ytd record
009600050518                  if  %found(liaba_ytd);
009700050528                     if emprdcmplt = emprdtotl;
009800050530                      amount# = pay_amt * (latotp - lacomp + 1) / latotp
009900050528                                                    - laamt;
010000071001                       else;
010100071001                      laamt += amount#;
010200071001                     endif;
010300050601                     update lafmt %fields(laamt);
010400071001                    // write Ytd record
010500050518                    else;
010600050518                     laemp = emp;
010700050518                     latype = emseltyp;
010800050518                     lastyp = *zeros;
010900050530                     lacomp = emprdcmplt;
011000050528                     latotp = emprdtotl;
011100050530                     laamt = amount#;
011200050601                     write lafmt;
011300050518                  endif;
011400050518
011500050518                endif;
011600050518              endif;
011700050517             enddo;
011800040111
011900040111
012000040111
012100050602              // Reset periods complete for new  year
012200050521              if emprdcmplt = emprdtotl;
012300050528
012400050528                 emprdcmplt = 0;
012500050521
012600050521              endif;
012700050518
012800050518              emnxtprd = get_new_prd(emnxtprd:emapymth:'A');
012900050518              get_ltype_info('UP':emseltyp:ematf_struc);
013000040111             endif;
013100050517
013200050518           enddo;
013300050518
013400050518
013500040111         // call pgm to write to g/l files
013600040111
013700040111           %occur(gl_accumd) = 1;
013800040111           errmsg = *blanks;
013900050517           write_to_gl(%addr(gl_accumd):gcnt:'EMT':errmsg);
014000040111
014100040111         // If update  did not complete successfully
014200040111
014300040111               if  errmsg <> *blanks;
014400040111                   rolbk;
014500040111                 else;
014600040111                   commit;
014700040111               endif;
014800040111
014900040111         *inlr = '1';
015000040111         return;
015100040111
015200040111        //****************************
015300040111        // Perform transaction write
015400040111        //****************************
015500040111           begsr perform_tr_write;
015600050517
015700050601              gen_amt = - amount#;
015800050517              gen_paye = emp;
015900050527              gen_type = emseltyp;
016000050527              gen_stype = *zeros;
016100050517              bill_num = #date;
016200050517
016300050601              write_x_trans(%addr(prm_rec):#date:bill_num:
016400050601                     (%trimr(text_) + ' ' +  %editc(prno:'4')):
016500050601                                   'EMT':*zeros:'E');
016600050517
016700050517               // update  emp files
016800050517              application_up(bill_num:'EMT':prno:'I');
016900050517
017000040111           endsr;
017100040111
017200040111        //****************************
017300040111        // Perform GL. write sequence
017400040111        //****************************
017500040111           begsr perform_gl_write;
017600040111
017700040111             exsr handl_debit_tot;
017800040111             exsr handl_credit_tot;
017900040111
018000040111           endsr;
018100040111
018200040111        //******************************************************
018300040111        // setup and process debit accounts in transaction file
018400040111        //******************************************************
018500040111           begsr handl_debit_tot;
018600040111
018700050528             gl_out =  proll_gl(emp:dept:empaytype:
018800050528                                  *blanks);
018900040111
019000040111             if  gl_out <> *blanks;
019100040111                 exsr   accum_gl_tot;
019200040111               // accumulate totals
019300050518                 gl_total += amount#;
019400040111             endif;
019500040111           endsr;
019600040111
019700040111        //*********************************************************
019800040111        // * setup and process credit accounts in transaction file
019900040111        //*********************************************************
020000040111           begsr handl_credit_tot;
020100040111
020200050527             gl_out =  crossref_gl(emp:dept:emseltyp:
020300050527                                  *zeros:emacct);
020400040111
020500040111             if  gl_out <> *blanks;
020600040111                exsr  accum_gl_tot;
020700040111           // accumulate totals
020800050517                gl_total -= amount#;
020900040111             endif;
021000040111           endsr;
021100040111
021200040111        //*****************************************************************
021300040111        //Search for gl.code in array.if found set occurance level and add
021400040111        //total ,if not found add new array record and Data Struct. total
021500040111        //*****************************************************************
021600040111           begsr accum_gl_tot;
021700040111
021800040128             gl_prno2 = prno;
021900040111             ycnt = 0;
022000040111             ycnt = %lookup(gl_xxx:glarr);
022100040111             if ycnt = 0;
022200050517                gcnt += 1;
022300040111                glarr(gcnt) = gl_xxx;
022400040111                %occur(gl_accumd) = gcnt;
022500040111                gl_account = gl_out;
022600040111                gl_prd = gl_prno2;
022700040111                gl_desc = %trimr(text_) + ' ' +
022800040128                      %editc(prno:'4');
022900040111
023000040111                 else;
023100040111
023200040111                %occur(gl_accumd) = ycnt;
023300040111             endif;
023400040111
023500040111         endsr;
023600040128      /end-free
023700040111
023800000518     *********************************************************************
023900000518     **  STARTING ROUTINE **
024000000518     *********************************************************************
024100000518     C     *INZSR        BEGSR
024200020925     **
024300020925     C     *ENTRY        PLIST
024400040128     C                   PARM                    prno              6 0
024500030809      *
024600030809      /free
024700030808             get_sys_date(#date);
024800040128             get_prd_info('*prd':*blanks:curprno:#date);
024900040128         endsr;
025000030809      /end-free
025100000518     **
