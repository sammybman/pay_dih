000100151026     H dftactgrp(*no) actgrp(*new) bnddir('DISDIR':'PAYDIR')
000200120125     h/define PROFOUNDUI
000300030902      //********************************************************
000400030902      //              B A N K S    D I H    L T D.              *
000500030902      //                  M.I.S. DEPARTMENT                     *
000600030902      //                                                        *
000700030902      //                                                        *
000800151009      //       Program Name......: PAYMNT1201                   *
000900030902      //                                                        *
001000060219      //       Created By........: S. Britton                   *
001100151009      //       Date..............: 09.10.2015                   *
001200030902      //                                                        *
001300151009      //   Description: Performance Incentive Global Parameters *
001400030902      //                                                        *
001500030902      //*********************************************************
001600151009     Fpaymnt12d1CF   E             workstn
001700120125     F/IF DEFINED(WEB)
001800120125     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001900120125     F/ENDIF
002000030902
002100120125      /copy QCPYSRC,genpr
002200120125      /copy QCPYSRC,salpr
002300120125      /copy QCPYSRC,autpr
002400120125      /copy QCPYSRC,emppr
002500120125      /copy QCPYSRC,PAYPR
002600030902
002700030902     Darr              s             40    dim(1) ctdata perrcd(1)
002800060213     D date#           S              8  0
002900060213     D time#           S              6  0
003000060224     D pay_type        s              2  0
003100151102     D altamt          s             13  2
003200060224     D category        s              2
003300151009
003400151009     d VarLen          s             10I 0 Inz(%size(Var))
003500151009     d ApiErr          s             15a
003600151009     d Caller          s             50a
003700151009     d WhoCalled       s             10a
003800151009     d i               s             10I 0
003900060211
004000060213     d msg1            c                   'Invalid Item entered'
004100060224     D msg2            c                   'Invalid Payroll type'
004200151023     D msg3            c                   '- This category is already link-
004300151023     D                                     ed to sales commission system'
004400060224     D msg5            c                   'Payroll type MUST represent an -
004500060224     D                                     Emolument'
004600151012     D msg6            c                   'Incentive  Payroll type not  -
004700151012     D                                     specified'
004800151012     D msg7            c                   'Incentive  Payroll type and ove-
004900060224     D                                     rriddden type is the same'
005000060325     D msg8            c                   'Must keep data at least 12 months '
005100060508     D msg10           c                   'Only ONE selection valid, either -
005200060508     D                                     paytype OVERRIDEN or DEDUCTED'
005300060508     D msg11           c                   'Confirmation Process Terminated'
005400151015     D msg12           c                   'Please Enter a valid Period'
005500060304
005600060211
005700151023     D                sds
005800030902     D  @pgm             *proc
005900120125     D  @user                358    367
006000060304
006100151023       Dcl-S arrValuesH    VarChar(512) dim(30);
006200151023       Dcl-S arrValues     VarChar(512) dim(30);
006300151023       Dcl-S varCountH     Packed(3)  inz(0);
006400151023       Dcl-S varCount      Packed(3)  inz(0);
006500151103       Dcl-S GlobalRecordFound  Ind inz(*off);
006600151102
006700151102       DCL-C SQLStateOK      '00000';
006800151023
006900160408       Dcl-Ds G_          ExtName('WHCOMMGM') inz qualified End-Ds;
007000160408       Dcl-Ds piGlobalDS  ExtName('PICOMMGM') inz End-Ds;
007100151023
007200060224     d emapymth#       s                   like(emapymth)
007300151023     d job_fld         s              2
007400151023     D xcnt            s              3  0
007500060224     D error           s               n
007600060509     d hld_ovrpayt     s              2  0
007700030902     D
007800151012     Dmain_prog        PR                  extpgm('PAYMNT1201')
007900060219
008000060211     Dmain_prog        PI
008100060224
008200060224
008300060509     D payRestrictRTN  pr
008400060508
008500060508     d updemp_rec      pr                  extproc('PAYUPD01R')
008600060508     d                                2  0 const
008700060508     d                               10    const
008800060508     d                                1  0 const
008900060418
009000060224
009100060224     D lst_jobcat      pr                  extpgm('HMRENQ05R')
009200060224     D                                2
009300060224
009400060224     Dlst_employee     PR                  extpgm('HMRENQ09R')
009500060224     D                                6  0
009600060224
009700151009
009800151009             FindCaller(Var:VarLen:'CSTK0100':JobIdInf
009900151009                         :'JIDF0100':ApiErr);
010000151009               For i = 1 to EntryCount;
010100151009                  Entry = %subst(Var:Offset + 1);
010200151009                  Caller = %trim(PgmLib) + '/' + %trim(PgmNam);
010300151009                  If (PgmNam <> @pgm and WhoCalled = *blanks);
010400151009                     WhoCalled = PgmNam;
010500151009                  Endif;
010600151009                  Offset = Offset + EntryLen;
010700151009               Endfor;
010800151009
010900151009            // Program CANNOT be called independently
011000151009            if WhoCalled <> 'PAYMNT12R';
011100151009              *inlr = *on;
011200151009              return;
011300151009           endif;
011400151009
011500060211
011600060211
011700060211         exsr  aainit;
011800060219         // get record
011900151102        //read  cmgfmt;
012000151102          exec sql
012100151102              select * into :piGlobalDS  from picommgm;
012200151102
012300151102          if  sqlstt = SQLStateOK;
012400151102             GlobalRecordFound = *on;
012500151102          endif;
012600060211
012700060509           // save override paytype
012800060509              hld_ovrpayt = cmgovrpayt;
012900151023              reset VarCountH;
013000060509
013100060509           // save job categories
013200060509          if  cmgrlist#1 <> *blanks;
013300151023              parse_commaSeparatedList(cmgrlist#1:%addr(arrValuesH):varCountH);
013400060509          endif;
013500060509
013600060509          if  cmgrlist#2 <> *blanks;
013700151023              parse_commaSeparatedList(cmgrlist#2:%addr(arrValuesH):varCountH);
013800060509          endif;
013900060509
014000060509          if  cmgrlist#3 <> *blanks;
014100151023              parse_commaSeparatedList(cmgrlist#3:%addr(arrValuesH):varCountH);
014200060509          endif;
014300060509
014400060509
014500040120         //**************************
014600040120         //*** Start of Program   ***
014700040120         //**************************
014800060325         dow  0 = 0;
014900060211              exfmt  fmt1;
015000060211
015100060211              errmsg = *blanks;
015200151023              reset VarCount;
015300060508              ovrpaytatr = *blanks;
015400060508              dedpaytatr = *blanks;
015500060211
015600060211             // end program
015700060211               if  *in03;
015800060211                  *inlr =  *on;
015900060211                  return;
016000060211               endif;
016100060211
016200060224          // Display List
016300060224          if  *in04;
016400060224                   select;
016500060224                    // Payroll tyep enquiry
016600060224                      when fld = 'CMGOVRPAYT';
016700060508                        lst_payty(cmgovrpayt);
016800060508                        iter;
016900060508                      when fld = 'CMGDEDPAYT';
017000060508                        lst_payty(cmgdedpayt);
017100060224                        iter;
017200151012                      when fld = 'CMGPITYPE';
017300151012                        lst_payty(cmgpitype);
017400151012                        iter;
017500060224                    // Job Category Enquiry
017600060224                      when fld = 'CMGRLIST#1'
017700060224                        or fld = 'CMGRLIST#2'
017800060224                        or fld = 'CMGRLIST#3';
017900060224                        lst_jobcat(category);
018000060224                        iter;
018100060224                    endsl;
018200060224          endif;
018300060224
018400060325           // Validate Job Category assigned to rates
018500060325          if  cmgrlist#1 <> *blanks;
018600151023              if  validate_commaSeparatedList(cmgrlist#1:
018700151023                                                    2: 2: 7: errmsg);
018800060325                  iter;
018900060325              endif;
019000151023              parse_commaSeparatedList(cmgrlist#1:%addr(arrValues):varCount);
019100060325          endif;
019200060325
019300060325          if  cmgrlist#2 <> *blanks;
019400151023              if  validate_commaSeparatedList(cmgrlist#2:
019500151023                                                    2: 2: 7: errmsg);
019600060325                  iter;
019700060325              endif;
019800151023              parse_commaSeparatedList(cmgrlist#2:%addr(arrValues):varCount);
019900060325          endif;
020000060325
020100060325          if  cmgrlist#3 <> *blanks;
020200151023              if  validate_commaSeparatedList(cmgrlist#3:
020300151023                                                    2: 2: 7: errmsg);
020400060325                  iter;
020500060325              endif;
020600151023              parse_commaSeparatedList(cmgrlist#3:%addr(arrValues):varCount);
020700060325          endif;
020800151012
020900151023          // validate Job categories :
021000151023          // ... ensure same Pay categories cannot be selected for
021100151023          // both Sales commission and  Incentive payments
021200151023          exec sql
021300151023              select * into :G_  from whcommgm;
021400151023
021500151023          for xcnt = 1 to varCount;
021600151023            job_fld = arrValues(xcnt);
021700151023            if   %scan(job_fld:G_.cmgrlist#1) <> *zeros;
021800151023               errmsg = job_fld + ' ' + msg3;
021900151023               iter;
022000151023            endif;
022100151023
022200151023            if   %scan(job_fld:G_.cmgrlist#2) <> *zeros;
022300151023               errmsg = job_fld + ' ' + msg3;
022400151023               iter;
022500151023            endif;
022600151023
022700151023            if   %scan(job_fld:G_.cmgrlist#3) <> *zeros;
022800151023               errmsg = job_fld + ' ' + msg3;
022900151023               iter;
023000151023            endif;
023100151023
023200151023          endfor;
023300151023
023400151012          // validate Incentive type
023500151012          if  cmgpitype <> *zeros;
023600151012             if get_payty_info('RD0':cmgpitype:payty_struc);
023700151012                   errmsg = msg2;
023800151012                   iter;
023900151012                 else;
024000151012                if ptyrtype <> 'E';
024100151012                   errmsg = msg5;
024200151012                   iter;
024300151012                endif;
024400151012             endif;
024500151012            else;
024600151012             errmsg = msg6;
024700151012             iter;
024800151012          endif;
024900060224
025000060224           // Validate overridden  payroll type
025100060508              if  cmgovrpayt <> *zeros
025200060508                or  cmgdedpayt <> *zeros;
025300060508
025400060508                if  cmgovrpayt <> *zeros
025500060508                 and cmgdedpayt <> *zeros;
025600060508                    errmsg = msg10;
025700060508                    ovrpaytatr = x'23';
025800060508                    dedpaytatr = x'23';
025900060508                    iter;
026000060508                endif;
026100060508
026200060508                select;
026300060508                 when cmgovrpayt <> *zeros;
026400060508                  if get_payty_info('RD0':cmgovrpayt:payty_struc);
026500060508                     errmsg = msg2;
026600060508                     iter;
026700060508                  endif;
026800060508                 when cmgdedpayt <> *zeros;
026900060508                  if get_payty_info('RD0':cmgdedpayt:payty_struc);
027000060508                     errmsg = msg2;
027100060508                     iter;
027200060508                  endif;
027300060508                endsl;
027400060224
027500060224                if ptyrtype <> 'E';
027600060224                   errmsg = msg5;
027700060224                   iter;
027800060224                endif;
027900060224
028000060224
028100151012                if  cmgpitype = cmgovrpayt;
028200060224                  errmsg = msg7;
028300060224                  iter;
028400060224                endif;
028500060224
028600060224              endif;
028700060224
028800060325          // Validate Months to keep configured data
028900060325          if  cmgkeepdta <= 12;
029000060325              errmsg = msg8;
029100060509              iter;
029200060325          endif;
029300151015
029400151015          // Validate Monthly Period Entered
029500151015          if cmgnprno = *zeros;
029600151015              errmsg = msg12;
029700151015              iter;
029800151015             else;
029900151015               //  cmgnprno = get_new_prd(cmgnprno:wyscomcyc:'A');
030000151015          endif;
030100151015
030200060325
030300151106          // Validate Year Average setting
030400151106          if cmgavgflag <> '1' and
030500151106             cmgavgflag <> '2';
030600151106             cmgavgflag =  '0';
030700151106          endif;
030800151106
030900040120
031000060219               if  *in15;
031100060508                 exfmt win2;
031200060508                 if OPTION = 'YES';
031300060509                    payRestrictRTN();
031400060508                    exsr  confirm_rtn;
031500060509                    commit;
031600060508                    *inlr =  *on;
031700060508                    return;
031800060508                   else;
031900060508                    errmsg = msg11;
032000060508                    iter;
032100060508                 endif;
032200060508
032300060219               endif;
032400060211
032500060325         enddo;
032600030902
032700060219         //******************************************                       ***
032800060219         //** subroutine to update parameter data  **
032900060219         //******************************************                       ***
033000060325         begsr  confirm_rtn;
033100031111
033200060219                   cmguser = @user;
033300060219                   cmgdate = date#;
033400060219                   cmgtime = time#;
033500031111
033600151102                 if  GlobalRecordFound;
033700151102             //  if  %eof();
033800030902
033900151102                    // Update Record
034000151102                     exec sql
034100151103                        update picommgm set row = :piGlobalDS;
034200060211                    else;
034300151102                     // Write Record
034400151102                     exec sql
034500151102                        insert into picommgm values (:piGlobalDS);
034600060219                 endif;
034700030902
034800060325         endsr;
034900030902
035000060211         //************************************************                 ***
035100060211         //** subroutine to retrieve header information
035200060211         //************************************************                 ***
035300060325         begsr aainit;
035400060211             coname = get_coname();
035500060213             @date  = get_sys_date(date#:time#);
035600060211             @title = arr(1);
035700060325         endsr;
035800060211
035900060224
036000060224
036100060418
036200060418     P*--------------------------------------
036300151023     P* Procedure name: PayRestrictRTN
036400151023     P* Purpose:        PayRestrictRTN
036500060418     P* Returns:        *boolean
036600060418     P* Parameter:      *none
036700060418     P*--------------------------------------
036800060509     P payRestrictRTN  B
036900060509     D payRestrictRTN  PI
037000060418
037100060509     D cntr            s              5  0
037200060509     D limit           s              2  0
037300060509     D job_fld         s              2
037400060418
037500060508
037600060509         // RESTORE employee earnings for specified type
037700060509         //---------------------------------------------
037800060509          if hld_ovrpayt > *zeros;
037900060509
038000151023            for  limit = 1 to varCountH;
038100151023               job_fld = arrValuesH(limit);
038200060509
038300061117               Exec Sql
038400061117                 DELETE FROM EMPEXCPT WHERE exccat = :job_fld and
038500061117                  EXCSTS = 'F' and EXCPTYPE = :hld_ovrpayt;
038600061117
038700060509                // recalculate employee earnings
038800060509                    updemp_rec(hld_ovrpayt:job_fld:3);
038900060509            endfor;
039000060509          endif;
039100060508
039200060509         // REMOVE employee earnings for specified type
039300060509         //--------------------------------------------
039400060509          if cmgovrpayt > *zeros;
039500151023            for  limit = 1 to varCount;
039600151023               job_fld = arrValues(limit);
039700061117
039800061117               Exec Sql
039900061117                INSERT INTO EMPEXCPT VALUES('F', :cmgovrpayt, 0, ' ',
040000061117                  :job_fld);
040100060508
040200060508                 // Delete employee earnings/deduction record
040300060508                   cntr = *zeros;
040400060508                   dow not get_emp_info2('R':-1:emp_struc:cntr);
040500060509                      if get_cat(status) = 'A' and empcat = job_fld;
040600151102                    //  chain (cmgovrpayt:emp) altfmt;
040700151102                        exec sql
040800151102                          select altamt into :altamt from payalwtf
040900151102                            where alttyp = :cmgovrpayt and altemp = :emp;
041000151102
041100151102                        if  sqlstt = SQLStateOK;
041200151102                   //   if   %found;
041300060508                            exsr   write_emp_log;
041400151102                   //       delete  altfmt;
041500151102                            exec sql
041600151102                               delete from payalwtf where
041700151103                                 alttyp = :cmgovrpayt and altemp = :emp;
041800060508                        endif;
041900060508                      endif;
042000060508                   enddo;
042100060509            endfor;
042200060509          endif;
042300060508
042400060508         //***************************************
042500060508         //*** Write employee log for update ***
042600060508         //***************************************
042700060508         begsr  write_emp_log;
042800061117
042900061117           Exec Sql
043000061117             INSERT INTO EMPCHGLOG VALUES(' ', :emp, :cmgovrpayt,
043100061117               :class, :date#, :date#, :altamt, :@user);
043200061117
043300060508         endsr;
043400060418
043500060418
043600060509     P payRestrictRTN  E
043700060508
043800030902
043900030902**  Program Title
044000151104Performance Incentive - Global Parameters
