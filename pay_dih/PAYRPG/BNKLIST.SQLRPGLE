000100170209     H DATEDIT(*DMY) alwnull(*usrctl)
000200090123      //****** Employee Salary --- S.BRITTON ******
000300090123      //
000400010427     Fpaydivl01 ip   E           K DISK
000500010912     Fempmasl1  if   E           K DISK
000600021129     Fpayrtypl01if   e           k disk    usropn
000700990413     FpaybankseqIF   E           K DISK    usropn
000800050211     FpaybankwrkuF a E           K DISK
000900050211     FpaybanknetIF   E           K DISK
001000110121     Fgenpysum  o    E           K DISK    commit
001100100922     F*
001200010912     Fpayhisl02 if   e           k disk    commit
001300021129     Fqsysprt   O    F  132        printer oflind(*inof) usropn
001400090123      //
001500110829      /copy QCPYSRC,genpr
001600050211
001700090123      //----------------------------------------
001800090123      // Prototype for procedure: NetSplitHandlr
001900090123      //----------------------------------------
002000050211     D NetSplitHandlr  PR            11  2
002100050211     D netemp                         6  0 const
002200050211     D netpaytype                     1    const
002300090618     D netbank                       30    const
002400050211     D netdivis                       2  0 const
002500050211     D netamt                        11  2 const
002600050211
002700090123      //------------------------------------
002800090123      // Prototype for procedure: LoadAmount
002900090123      //------------------------------------
003000050211     D LoadAmount      PR            11  2
003100050211     D lemp                           6  0 const
003200050211     D lpaytype                       1    const
003300090618     D lbank                         30    const
003400050211     D ldivis                         2  0 const
003500050211     D lamt                          11  2 const
003600050211
003700090123      //---------------------------------------
003800090123      // Prototype for procedure: ClearWorkFile
003900090123      //---------------------------------------
004000050211     D ClearWorkFile   PR
004100050211
004200090123      //
004300950930     D                SDS
004400950930     D  @PGM             *PROC
004500131031     D  @USER                358    367
004600090123      //
004700010914     Dcrdate           s               D   datfmt(*dmy)
004800010914     Dpydate           s               D   datfmt(*dmy)
004900020326     Dconame           s             30
005000010809     Dmim_type         s              1
005100010809     Drun_type         s              1
005200010809     Dpbtype           s              1
005300010912     Dcount            s              4  0
005400010912     Dtotpay           s             11  2
005500050211     DAccountAmt       s             11  2
005600090123     DAbsoluteAmt      s             11  2
005700050217     DAname            s             40
005800110121     Dorigin           s              3    inz('PAY')
005900050226     Dsplitflag        s              1
006000050226
006100050226     DYES              c                   'Y'
006200100922      *
006300100922      //
006400090123      //
006500010427     C     trn_key       klist
006600010427     C                   kfld                    pbtype            1
006700010427     C                   kfld                    emp
006800090123      //
006900090123      //-------------------------------------------------------------------------
007000090123      // Get Division and Bank account from sequence file
007100090123      /FREE
007200090123       chain dtno pbsfmt;
007300090123       *IN88 = NOT %FOUND;
007400090123       //-------------------------------------------------------------------------
007500990201
007600090123       // Print names on report if Division Print flag is set
007700090123       if dtflag = 'Y';
007800090123         *IN10 = *ON;
007900090123       else;
008000090123         *IN10 = *OFF;
008100090123       endif;
008200090123       //
008300090123       if not(*in88);
008400090123         open qsysprt;
008500090123         except hedr;
008600090123         //
008700090123         dow not(*in88);
008800990413
008900090123           chain emp emprec;
009000090123           if %found;
009100090123             chain trn_key phisfmt;
009200090123             *IN77 = NOT %FOUND;
009300090123             //
009400090123             if not(*in77);
009500090123      /END-FREE
009600010918     C     *iso          move      phispdate     pydate
009700000629     C     pydate        subdur    1:*d          crdate
009800090123      //
009900090123      /FREE
010000090123              if *inof;
010100090123                 except hedr;
010200090123              endif;
010300090123               //
010400090123              dow not(*in77);
010500090123
010600090123                 AccountAmt = NetSplitHandlr(emp:pbtype:pbsbank
010700090123                                        :dtno:phisnpay);
010800090123
010900090123                 if AccountAmt <> *zeros;
011000090123                   totpay += AccountAmt;
011100090123                   count += 1;
011200090123                   if Aname = *blanks;
011300090123                     aname =  %trim(christ) + ' '
011400090123                         +  %trim(surnam);
011500090123                   endif;
011600090123                   exsr write_sum;
011700090123                   except;
011800090123                 endif;
011900090123
012000090123                 reade trn_key phisfmt;
012100090123                 *IN77 = %EOF;
012200090123               enddo;
012300090123             endif;
012400090123             // if valid employee record
012500090123           endif;
012600090123           reade dtno pbsfmt;
012700090123           *IN88 = %EOF;
012800090123         enddo;
012900090123         //
013000090123         except footer;
013100090123         close qsysprt;
013200090123
013300090123         totpay = *zeros;
013400090123         count = *zeros;
013500090123
013600090123       endif;
013700090123
013800050211         ClearWorkFile();
013900990413
014000090123       begsr *inzsr;
014100090123      /END-FREE
014200010911     C     *entry        plist
014300010911     C                   parm                    run_type
014400010911     C                   parm                    mim_type
014500010809
014600090123      /FREE
014700090123         coname = get_coname;
014800090123         // Get correct payrun type for processing
014900090123         if mim_type <> *blanks;
015000090123           pbtype = mim_type;
015100090123         else;
015200090123           pbtype = run_type;
015300090123         endif;
015400090123         // Get Payrun Description
015500090123         open payrtypl01;
015600090123         chain pbtype prtyfmt;
015700090123         close payrtypl01;
015800090123         // Set payroll type for selection
015900090123      /END-FREE
016000020316     C                   callb     'BNKLIST2'
016100991207     C                   parm                    pbtype
016200051118     C                   parm                    mim_type
016300090123      /FREE
016400090123         open paybankseq;
016500090123       endsr;
016600090123
016700090123       begsr write_sum;
016800090123
016900090123         paysts = 'A';
017000090123         paydate = %dec(pydate:*dmy);
017100090123         paycrd = %dec(crdate:*dmy);
017200090123         payacc = pbsbank;
017300090123         paydiv = pbsdivis;
017400090123         payamt = AccountAmt;
017500090123         payemp = emp;
017600100923          //payorg = origin;
017700090123         payrtype  = pbtype;
017800090123         paybname  = aname;
017900090123         payflag  = *blanks;
018000090123
018100090123         if AccountAmt < *zeros;
018200090123
018300090123           AbsoluteAmt = %abs(payamt);
018400090123
018500090123           // update record Bank summary file
018600090123           exec sql
018700100922            //update bnkpysum set payamt = payamt + :payamt
018800100922            // where paysts = :paysts and payemp = :payemp and
018900100922            //  payacc = :payacc and paydate = :paydate and
019000100922             // paydiv = :paydiv and payrtype = :payrtype and
019100100922             // payamt = :AbsoluteAmt;
019200090123
019300110121              update genpysum set payamt = payamt + :payamt
019400100922               where paysts = :paysts and payemp = :payemp and
019500100922                payacc = :payacc and paydate = :paydate and
019600100922                paydiv = :paydiv and payrtype = :payrtype and
019700110121                payamt = :AbsoluteAmt and payorg = :origin;
019800090123             if sqlstt <> '00000';
019900090123               // Write to Bank summary file
020000110121               payorg = origin;
020100090123               write payfmt;
020200090123             endif;
020300090123
020400090123            else;
020500090123           // Write to Bank summary file
020600110121               payorg = origin;
020700090123           write payfmt;
020800090123         endif;
020900090123       endsr;
021000090123       //
021100090123      /END-FREE
021200021129     Oqsysprt   e            hedr        2  1 03
021300950930     O                       @PGM                10
021400020620     O                       coname              70
021500990114     O          e            hedr           2
021600950930     O                       @USER               10
021700990114     O                                           63 '-----------------'
021800990114     O          e            hedr        1
021900990114     O                       dtname              15
022000990114     O          e            hedr        0
022100990114     O                       dtname              15
022200991217     O                                           51 'BANK ACCOUNT LIST - SALA'
022300020808     O                                           75 'RY FOR PERIOD ENDING...:'
022400990114     O                       UDATE         Y     85
022500940516     O                                          117 'PAGE-'
022600940516     O                       PAGE          Z    122
022700990114     O          e            hedr        1  2
022800990114     O                                           51 '------------------------'
022900990114     O                                           75 '------------------------'
023000990114     O                                           85 '---------------'
023100990114     O          e            hedr        1
023200090618     O               10                          28 '--------------------------'
023300090618     O               10                          44 '----------------'
023400090618     O                                           70 '--------------------------'
023500090618     O                                           96 '--------------------------'
023600090618     O                                          100 '----'
023700990114     O          e            hedr        1
023800090618     O               10                           3 '|'
023900090618     O               10                          15 'N A M E'
024000090618     O                                           45 '|'
024100090618     O                                           70 'BANK ACCOUNT NUMBER'
024200090618     O                                           80 '|'
024300090618     O                                           93 'AMOUNT '
024400090618     O                                          100 '|'
024500050217     O                       prtydesc           130
024600990114     O          e            hedr        1
024700090618     O               10                          28 '--------------------------'
024800090618     O               10                          44 '----------------'
024900090618     O                                           70 '--------------------------'
025000090618     O                                           96 '--------------------------'
025100090618     O                                          100 '----'
025200951209     O          E                        2
025300090618     O               10                           3 '|'
025400090618     O               10      aname               44
025500090618     O                                           45 '|'
025600090618     O                       pbsbank             75
025700090618     O                                           80 '|'
025800090618     O                       AccountAmt    ab    98 '$'
025900090618     O                                          100 '|'
026000990114     O          e            footer      1
026100090618     O               10                          28 '--------------------------'
026200090618     O               10                          44 '----------------'
026300090618     O                                           70 '--------------------------'
026400090618     O                                           96 '--------------------------'
026500090618     O                                          100 '----'
026600990114     O          e            footer      1  2
026700050217     O                                           75 'TOTAL AMOUNT:-'
026800050217     O                       totpay        a     90 '$'
026900050217     O                                           93 '**'
027000000517     O                                           28 'No. of Records -'
027100000517     O                       count         Z   +  1
027200990114     O          e            footer      2  2
027300950804     O                                           60 'PREPARED BY'
027400950804     O                                           85 '------------------------'
027500990114     O          e            footer      1  2
027600020326     O                                           45 'COMPANY SECRET'
027700951205     O                                           60 'ARY''S SIGNATURE'
027800940516     O                                           85 '------------------------'
027900990114     O          e            footer      1  2
028000020326     O                                           45 'COMPANY DIRECT'
028100951205     O                                           60 'OR''S  SIGNATURE'
028200940516     O                                           85 '------------------------'
028300990114     O          e            footer      1
028400020326     O                                           54 'AUTHORIZED SIGNA'
028500940516     O                                           58 'TURE'
028600940516     O                                           85 '------------------------'
028700990114     O          e            footer      1
028800960409     O                                           44 'ACCOUNT TO BE CREDITED B'
028900960409     O                                           58 'Y AFTERNOON OF'
029000960409     O                       CRDATE              68
029100090123      //
029200050211
029300090123      //---------------------------------------
029400090123      // Procedure name: NetSplitHandlr
029500090123      // Purpose:        Split Net to different
029600090123      //                 accounts
029700090123      //---------------------------------------
029800050211     P NetSplitHandlr  B
029900050211     D NetSplitHandlr  PI            11  2
030000050211     D netemp                         6  0 const
030100050211     D netpaytype                     1    const
030200090618     D netbank                       30    const
030300050211     D netdivis                       2  0 const
030400050211     D netamt                        11  2 const
030500050211      /free
030600050217           aname = *blanks;
030700050226           splitflag = *blanks;
030800050211           chain (netpaytype:netemp) pbnfmt;
030900051118           if  %found(paybanknet)
031000051118                and  pbnflag = *blanks
031100051118                 or %found(paybanknet)
031200051118                  and  pbnflag = 'Y' and mim_type = *blanks;
031300050211
031400050226             splitflag = YES;
031500050211             chain netemp pbwfmt;
031600050211             if  %found(paybankwrk);
031700050211              chain (netemp:netpaytype:netdivis:netbank)
031800050211                       pbwfmt;
031900050211              if  %found(paybankwrk);
032000050217                aname = pbwname;
032100050211                return pbwamt;
032200050211              endif;
032300050211
032400050211               else;
032500050211                return  LoadAmount(netemp:netpaytype:netbank
032600050211                            :netdivis:netamt);
032700050211             endif;
032800050211             else;
032900050211              return netamt;
033000050211           endif;
033100050211
033200050211      /end-free
033300050211     P NetSplitHandlr  E
033400050211
033500090123      //---------------------------------------
033600090123      // Procedure name: LoadAmount
033700090123      // Purpose:        Split Net to different
033800090123      //                 accounts LOADING
033900090123      //---------------------------------------
034000050211     P LoadAmount      B
034100050211     D LoadAmount      PI            11  2
034200050211     D lemp                           6  0 const
034300050211     D lpaytype                       1    const
034400090618     D lbank                         30    const
034500050211     D ldivis                         2  0 const
034600050211     D lamt                          11  2 const
034700050211
034800050211     D TotalPer        s              5  2
034900050211     D workingAmt      s             11  2
035000140312     D percentAmt      s             11  2
035100050211     D CurrentAmt      s             11  2
035200050211      /free
035300050211
035400050211           workingAmt = lamt;
035500050211           setll (lpaytype:lemp) pbnfmt;
035600050211           reade (lpaytype:lemp) pbnfmt;
035700050211           dow not %eof(paybanknet);
035800050211
035900050211            select;
036000050211             when pbnamt > *zeros;
036100050211              if workingAmt > pbnamt;
036200050211               pbwamt = pbnamt;
036300050211               workingAmt -= pbnamt;
036400050211                else;
036500050211               pbwamt = workingAmt;
036600050211               workingAmt = *zeros;
036700050211              endif;
036800050211
036900050211             when pbnper > *zeros;
037000050211               totalper += pbnper;
037100050211               if totalper = 100;
037200050211                 pbwamt = workingAmt;
037300050211                else;
037400140312                 pbwamt = round_up(((workingAmt + percentAmt) *
037500050211                                   pbnper / 100):'U');
037600140312                 percentAmt += pbwamt;
037700060614                 workingAmt -= pbwamt;
037800050211               endif;
037900050211            endsl;
038000050211
038100050211            if lbank = pbnbank and
038200050211              ldivis = pbndivis;
038300050217              aname = pbnname;
038400050211              currentAmt = pbwamt;
038500050211            endif;
038600050211
038700050211              pbwtype = lpaytype;
038800050211              pbwemp = lemp;
038900050211              pbwdivis = pbndivis;
039000050211              pbwbank = pbnbank;
039100050217              pbwname = pbnname;
039200050211
039300050211              write pbwfmt;
039400050211            reade (lpaytype:lemp) pbnfmt;
039500050211           enddo;
039600050211
039700050211              return CurrentAmt;
039800050211
039900050211      /end-free
040000050211     P LoadAmount      E
040100050211
040200090123      //---------------------------------------
040300090123      // Procedure name: ClearWorkFile
040400090123      // Purpose:        Clear work file at
040500090123      //                 processing end
040600090123      //---------------------------------------
040700050211     P ClearWorkFile   B
040800050211     D ClearWorkFile   PI
040900050211      /free
041000050211
041100050211           setll *loval pbwfmt;
041200050211           read pbwfmt;
041300050211
041400050211           dow not(%eof);
041500050211             delete pbwfmt;
041600050211             read pbwfmt;
041700050211           enddo;
041800050211
041900050211      /end-free
042000050211     P ClearWorkFile   E
