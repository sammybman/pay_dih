000100941121     H DATEDIT(*DMY)
000200151026     H dftactgrp(*no) actgrp(*new) bnddir('DISDIR')
000300111028     H/define profoundui
000400151013      //*********************************************************
000500151013      //              D I S E R V   S Y S T E M S.              *
000600151013      //                  PAYROLL  SYSTEM                       *
000700151013      //                                                        *
000800151013      //                         BY                             *
000900151013      //                                                        *
001000151013      //             Diserv systems Team                        *
001100151013      //             P.O BOX XXXXX, GEORGETOWN,GUYANA           *
001200151013      //
001300151013      //                                                        *
001400151013      //                                                        *
001500151021      //       RPG PROGRAM NAME..: PAYMNT1203                   *
001600151013      //                                                        *
001700151013      //       PROGRAMMER NAME: Samuel Britton                  *
001800151013      //       DESIGN DATE....: 2015/10/12                      *
001900151013      //       MODULES........: *NONE                           *
002000151013      //       INDICATORS.....:                                 *
002100151013      //                                                        *
002200151013      //                                                        *
002300151013      //      Description: Performance Incentive Types          *
002400151013      //                                                        *
002500151013      //*********************************************************
002600151021     Fpaymnt12d3cf   e             workstn
002700111028     F/IF DEFINED(WEB)
002800111028     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002900111028     F/ENDIF
003000160414     F                                     sfile(sub1:rrn)
003100160414
003200160414     D SubfileData     DS                   Qualified  dim(99)
003300160414     D  subf                                likerec(sub1:*output)
003400160414
003500160415     D*SubfileStruc  e ds                  extname(paymnt12d3:sub1) inz
003600160414
003700151021
003800111028      /copy QCPYSRC,genpr
003900111028      /copy QCPYSRC,salpr
004000111028      /copy QCPYSRC,autpr
004100151029      /copy QCPYSRC,paypr
004200151021
004300160420     D arr             S             40    dim(1) CTDATA PERRCD(1)
004400040302     D found_ind       S               n
004500151029     D error           S               n
004600060612     D counter         S              5  0
004700151029     D dtype           S              1
004800160415     D numberrec       s              3  0
004900160415     D rec_count       s              2  0
005000160411     D whid            S              3  0
005100160415     D rrn             s              2  0
005200160415     D xcnt            s              2  0
005300160415     D cnt             s              2  0
005400160415     D org#chkarr      s              3    dim(25)
005500021018     **
005600021018     D msg1            c                   'Cannot delete - Record In Use'
005700151021     D msg2            c                   'Blank Entry not allowed'
005800151021     D msg3            c                   'Must specify transaction code(s)'
005900151021     D msg4            c                   'Either specify warehouse List or -
006000151021     D                                     use Department defined location'
006100151028     D msg5            c                   'Invalid Incentive Code'
006200151029     D msg6            c                   'Invalid Department'
006300151029     D msg7            c                   'Invalid Application code (origin)'
006400160413     D msg8            c                   'Must specify Warehouse/Location -
006500160413     D                                     code(s)'
006600160415     D msg9            c                   'Multiple records defined: Cannot -
006700160415     D                                     switch now'
006800151021
006900151021       DCL-C SQLStateOK      '00000';
007000151021
007100160411       Dcl-Pr   Lst_dept   Extpgm('HMRENQ02R');
007200160411         *N              Packed(3);
007300160411       End-Pr;
007400160411
007500151021       //---------------------------------------------------------------
007600151021       // Performance Incentive Field Struc
007700151021       //---------------------------------------------------------------
007800151021       Dcl-Ds PIDStruc;
007900151029         pidsts          Char(1);
008000151021         pidept          Packed(3);
008100151026         pitype#         Packed(2);
008200160415         whlist          Char(75);
008300160415         cdlist          Char(30);
008400151021         revflag         Char(1);
008500151021         deptflag        Char(1);
008600160415         origin          Char(3);
008700151021       End-Ds;
008800151021
008900151027       Dcl-Ds piCodemDs   ExtName('PICODEM') End-Ds;
009000151021
009100040302     **
009200010815     D                sds
009300950711     D  @PGM             *PROC
009400111028     D  @USER                358    367
009500020128     d
009600021017     **
009700010301     *******************
009800010301     ** Program start **
009900010301     *******************
010000111028
010100111028
010200021217
010300151021               dow 0 = 0;
010400021017
010500021017                   exsr aainit;
010600021017
010700021017                   exfmt fmt1;
010800021017
010900021017       //  F3 - Program End
011000021017                   if *in03;
011100021017                      *inlr = *on;
011200021017                       return;
011300021017                   endif;
011400021017
011500151021        // F1 - Department  Enquiry
011600021017                   if  *in01;
011700160411                      lst_Dept(pidept#);
011800151021               //     lst_PItypes(pitype#);
011900021017                       iter;
012000021017                    endif;
012100021017
012200021017                   #mode = *blanks;
012300021017
012400021017       //   Validate Entry
012500151021                   if pidept# = *zeros;
012600151021                      errmsg = msg2;
012700151021                      iter;
012800021017                   endif;
012900151029
013000151029       //   Validate department
013100151029                   deptname = get_dept_info(pidept#:error:*omit:*omit:*omit:
013200151029                                      *omit:*omit:*omit:*omit:*omit:dtype);
013300151029                   if error;
013400151029                      errmsg = msg6;
013500151029                      iter;
013600151029                   endif;
013700021018
013800160415              exec sql
013900160415                 select count(*) into :rec_count from pideptm
014000160415                        where piddept = :pidept#;
014100160415
014200151021       //   If P.I. Dept defn. Found
014300151021                  exec sql
014400151021                     select * into :PIDStruc from  pideptm
014500151021                        where piddept = :pidept#;
014600160415
014700160415                  if  rec_count > 1;
014800160415                     LoadRecords();
014900160415                     DisplayRecords();
015000160415                     iter;
015100160415                  endif;
015200151021
015300151021                  if  sqlstt = SQLStateOK;
015400021017
015500021017                          #mode = 'AMEND ';
015600151021                          found_ind = *on;
015700021017
015800151026         // If P.I. Type Not found
015900151021                        else;
016000151021                          #mode = ' ADD ';
016100151021                          clear PIDStruc;
016200151021                          found_ind = *off;
016300151021                  endif;
016400021017
016500151021                 leave;
016600151021               enddo;
016700021017
016800021017       //   Display second screen
016900151021                    dow 0 = 0;
017000021017
017100021018                    exfmt fmt2;
017200021018
017300021018                    clear errmsg;
017400021017
017500021017       //   F2 - Cancel
017600021017                  if  *in02;
017700160415                       reset_sr();
017800060612                       leave;
017900021017                  endif;
018000060612
018100160411          // Warehouse  Location ENQUIRY
018200060612                 if  *in04;
018300160411                   whid = *zeros;
018400160411                // lst_whcon(whid:*blank:'P');
018500160415                   lst_whcon(whid:*blank);
018600160411                   if whid <> *zeros;
018700160411                     if whlist = *blanks;
018800160411                       whlist =  %char(whid);
018900160411                        else;
019000160411                       whlist = %trim(whlist) + ',' + %char(whid);
019100160411                     endif;
019200160411                   endif;
019300060612                   iter;
019400060612                 endif;
019500160415
019600160415
019700160415       //  F7  - Switch to List mode
019800160415                  if  *in07;
019900160415                     LoadRecords();
020000160415                     DisplayRecords();
020100160415                     leave;
020200160415                  endif;
020300151029
020400151029          // Set ORIGIN depending on department type
020500151029                  select;
020600151029                    when dtype = 'P'; // Production Department
020700151029                       origin = 'PRO';
020800151029                    when dtype = 'B'; // Branch Department
020900151029                       origin = 'WHS';
021000151029                    when dtype = 'R'; // Restaurant
021100151029                       origin = 'RES';
021200160420                    other;
021300160420                       origin = 'WHS'; // defaults to W/house
021400151029                  endsl;
021500021017
021600151029       //   Validate Origin
021700151029                  if origin <> *blanks and
021800160416                     check_org(origin:*blanks:origindesc);
021900151029                      errmsg = Msg7;
022000151029                      iter;
022100151029                  endif;
022200151029
022300151028       //   Validate PI Code
022400151028                  exec sql
022500151028                     select * into :PIcodemDS from  picodem
022600151028                        where piccode = :pitype#;
022700151028
022800151028                  if  sqlstt <> SQLStateOK;
022900151028                      errmsg = Msg5;
023000151028                      iter;
023100151028                  endif;
023200151028
023300151021       //   Validate Transaction code List
023400151109                  if cdlist = *blanks
023500151109                    and picwhcdlst = *blanks;
023600151021                        errmsg = Msg3;
023700021017                        iter;
023800021018                  endif;
023900151021
024000160413       //   Validate w/house code entry #1
024100151021                  if whlist <> *blanks and
024200151021                     deptflag = 'Y';
024300151021                        errmsg = Msg4;
024400151021                        iter;
024500151021                  endif;
024600160413
024700160413       //   Validate W/house code entry #2
024800160413                  if whlist = *blanks
024900160413                    and picwhlst = *blanks;
025000160413                        errmsg = Msg8;
025100160413                        iter;
025200160413                  endif;
025300060612
025400151023       //  validate  Whouse Transaction codes
025500160411                  if cdlist <> *blanks
025600151023                    and validate_commaSeparatedList(cdlist:
025700151023                                                    1: 1: 3: errmsg);
025800151023                   iter;
025900151023                  endif;
026000151023
026100151021       //  validate  Whouse Locations
026200151021                  if whlist <> *blanks
026300151022                    and validate_commaSeparatedList(whlist:
026400160419                                   3: 1: 1: errmsg:*blanks:origin);
026500151021                   iter;
026600060612                  endif;
026700060612
026800021017
026900021017       //  F15 - Confirm Record
027000021017                  if *in15;
027100160415                       confirmsr('SNG');
027200021017                       leave;
027300021017                  endif;
027400021017
027500021017       //  F20 - Delete Record
027600021017                  if *in20;
027700160415                      delete_sr();
027800021017                      if errmsg <> *blanks;
027900021017                        iter;
028000021017                       endif;
028100021017
028200021017                  leave;
028300021017                  endif;
028400160415
028500021017
028600021017                  enddo;
028700111028
028800111028         //*****************************
028900111028         // Session Init
029000111028         //*****************************
029100111028
029200111028          begsr *inzsr;
029300151021            get_session_info('PAY':errmsg:@pgm);
029400111028            if errmsg <> *blanks;
029500111028              show_msg(errmsg);
029600111028              *inlr = *on;
029700111028              return;
029800111028            endif;
029900111028
030000111028          endsr;
030100111028
030200021018
030300021018       //***************************************************
030400021018       //** First initilisation routine for all programs  **
030500021018       //***************************************************
030600021018           begsr aainit;
030700021018                   CONAME = get_coname;
030800151021                   @DATE = get_sys_date;
030900151021                 //  select header for program
031000151021                   @title = arr(1);
031100021018
031200151021           ENDSR;
031300160414
031400160414      //--------------------------------------
031500160414      // Procedure name: LoadRecords
031600160414      // Purpose:        Load records to subfile
031700160414      //--------------------------------------
031800160414     P LoadRecords     B
031900160414     D LoadRecords     PI
032000160414
032100160414
032200160414
032300160414       // Build Subfile
032400160415
032500160414                  *in33 = *on;
032600160415                   write subctl1;
032700160415                   rrn = 0;
032800160415                   *in33 = *off;
032900160415                   *in55 = *off;
033000160414
033100160415             exec sql
033200160415                declare pf1 cursor for select *
033300160415                  from pideptm where piddept = :pidept#;
033400160415
033500160415
033600160415             exec sql
033700160415               open pf1;
033800160415
033900160415             //read auth. file
034000160415             dow not read_pf1();
034100160415
034200160414
034300160415                // write to subfile
034400160414
034500160414                   rrn += 1;
034600160414
034700160414                  write sub1;
034800160414
034900160414
035000160414               enddo;
035100160415
035200160415             exec sql
035300160415               close pf1;
035400160415
035500160415
035600160414
035700160414                  *in55 = *on;
035800160414                  *in20 = rrn > 0;
035900160414
036000160415                validate_rtn(errmsg);
036100160414
036200160414              RETURN;
036300160415
036400160414     P LoadRecords     E
036500160414
036600160414      //--------------------------------------
036700160414      // Procedure name: DisplayRecords
036800160414      // Purpose:        Display records of subfile
036900160414      //--------------------------------------
037000160414     P DisplayRecords  B
037100160414     D DisplayRecords  PI
037200160414
037300160415
037400160414       // Your calculation code goes here
037500160414            dow 0 = 0;
037600160414                 exfmt subctl1;
037700160414
037800160414            // F2 = Cancel
037900160414                  if  *in02;
038000160414                   *inlr = *on;
038100160414                   return;
038200160414                  endif;
038300160414
038400160414            // F1=Help Screen
038500160414                  if *in01;
038600160415                //  exfmt winrec;
038700160414                     iter;
038800160414                  endif;
038900160415
039000160414
039100160414            // F10=Add New Line
039200160414                  if *in10;
039300160414                    Add_Blank_Line();
039400160414                     iter;
039500160414                  endif;
039600160414
039700160414            // F11=Insert a New Line
039800160414                  if *in11;
039900160414                    Insert_Blank_Line();
040000160414                     iter;
040100160414                  endif;
040200160414
040300160415            //   GET / CHECK VALID Origin
040400160414                  if  *in04;
040500160415                     lst_whcon(whid:*blank);
040600160414                     iter;
040700160414                  endif;
040800160414
040900160415            // Validate data
041000160415                if  validate_rtn(errmsg);
041100160414                  iter;
041200160414                endif;
041300160415
041400160415            // F7=Switch View
041500160415                  if *in07;
041600160415                    if numberrec > 1;
041700160415                       errmsg = msg9;
041800160415                       iter;
041900160415                       else;
042000160415                    endif;
042100160415                     iter;
042200160415                  endif;
042300160414
042400160414
042500160414            // F15 = Confirm Record
042600160414                  if *in15;
042700160415                    ConfirmSr('LST');
042800160414                    *inlr = *on;
042900160414                    return;
043000160414                  endif;
043100160414
043200160414            enddo;
043300160414
043400160414               RETURN;
043500160414
043600160415
043700160414     P DisplayRecords  E
043800160414
043900160415
044000160415      //--------------------------------------
044100160415      // Procedure name: Add_Blank_Line
044200160415      // Purpose:        Add a blank Line
044300160415      //--------------------------------------
044400160415     P Add_Blank_line  B
044500160415     D Add_blank_line  PI
044600160415
044700160415
044800160415                clear sub1;
044900160415                rrn +=  1;
045000160415                write sub1;
045100160415
045200160415                *in20 = rrn > 0;
045300160415
045400160415
045500160415     P Add_Blank_line  E
045600160415
045700160415      //--------------------------------------
045800160415      // Procedure name: Insert_Blank_Line
045900160415      // Purpose:        Insert a blank Line
046000160415      //                 at cursor position
046100160415      //--------------------------------------
046200160415     P Insert_Blank_line...
046300160415     P                 B
046400160415     D Insert_Blank_line...
046500160415     D                 PI
046600160415
046700160415
046800160415
046900160415       // Build Subfile
047000160415               cnt = rrn;
047100160415
047200160415       // Save Subfile data
047300160415               for xcnt = 1 to cnt;
047400160415
047500160415                  chain xcnt sub1;
047600160415               // eval-corr SubfileData(xcnt).subf = SubfileStruc;
047700160415                  eval-corr SubfileData(xcnt).subf = PIDStruc;
047800160415
047900160415               endfor;
048000160415
048100160415               *in33 = *on;
048200160415               write subctl1;
048300160415               rrn = *zeros;
048400160415               *in33 = *off;
048500160415               *in55 = *off;
048600160415
048700160415       // Insert record at beginning of subfile
048800160415               if getrrn = *zeros;
048900160415                   clear sub1;
049000160415                   rrn +=  1;
049100160415                   write sub1;
049200160415               endif;
049300160415
049400160415               if cnt <> *zeros;
049500160415
049600160415                 FOR xcnt = 1 to cnt;
049700160415
049800160415                    rrn +=  1;
049900160415                   // write to subfile
050000160415                   WRITE sub1 SubfileData(xcnt).Subf;
050100160415
050200160415                   // Insert record after current cursor position
050300160415                     if xcnt = getrrn;
050400160415                      clear sub1;
050500160415                      rrn +=  1;
050600160415                      write sub1;
050700160415
050800160415                     endif;
050900160415
051000160415                 endfor;
051100160415               endif;
051200160415
051300160415
051400160415               *in55 = *on;
051500160415               *in20 = rrn > 0;
051600160415
051700160415
051800160415
051900160415     P Insert_Blank_line...
052000160415     P                 E
052100160415
052200160415
052300160415       //..................................//
052400160415       //Procedure to read target records  //
052500160415       //..................................//
052600160415     p read_pf1        B
052700160415     D read_pf1        PI              n
052800160415
052900160415
053000160415          exec sql
053100160415            fetch next from pf1 into :PIDStruc;
053200160415
053300160415
053400160415        if sqlstt = '02000';
053500160415           return *on;
053600160415          else;
053700160415           return *off;
053800160415        endif;
053900160415
054000160415     p read_pf1        E
054100160415
054200160415      //--------------------------------------
054300160415      // Procedure name: Validate Routine
054400160415      // Purpose:        Validate PI dept entry
054500160415      //--------------------------------------
054600160415     PValidate_rtn     B
054700160415     DValidate_rtn     PI              n
054800160415     D errmsg                        75
054900160415
055000160415     D foundper        s               n
055100160415     D error           s               n
055200160415
055300160415     D ALREADYFOUND    c                   'Already found'
055400160415     D MSGV1           c                   'Cannot Dupicate Origin Entry'
055500160415
055600160415
055700160415               numberrec = *zeros;
055800160415               clear errmsg;
055900160415               clear  org#chkarr;
056000160415               cnt = rrn;
056100160415
056200160415               for xcnt = 1 to cnt;
056300160415
056400160415                  *in40 = *off;
056500160415                  *in41 = *off;
056600160415                  *in42 = *off;
056700160415                  *in43 = *off;
056800160415                  *in44 = *off;
056900160415
057000160415                  chain xcnt sub1;
057100160415
057200160415                  //   Validate Origin
057300160415                  if origin <> *blanks and
057400160416                    check_org(origin:*blanks:origindesc);
057500160415                     errmsg = Msg7;
057600160415                     *in40 = *on;
057700160415                     error = *on;
057800160415                  endif;
057900160415
058000160415                if  origin <> *blanks;
058100160415                  if  %lookup(origin:org#chkarr) > 0;
058200160415                     errmsg = msgV1;
058300160415                     *in40 = *on;
058400160415                     error = *on;
058500160415                  endif;
058600160415                   org#chkarr(xcnt) = origin;
058700160415                   numberrec += 1;
058800160415                endif;
058900160415
059000160415                  //   Validate Transaction code List
059100160415                  if cdlist = *blanks
059200160415                    and picwhcdlst = *blanks;
059300160415                       errmsg = Msg3;
059400160415                       *in41 = *on;
059500160415                       error = *on;
059600160415                  endif;
059700160415
059800160415                  //   Validate W/house code entry #2
059900160415                  if whlist = *blanks
060000160415                    and picwhlst = *blanks;
060100160415                       errmsg = Msg8;
060200160416                       *in42 = *on;
060300160415                       error = *on;
060400160415                  endif;
060500160415
060600160415                  //  validate  Whouse Transaction codes
060700160415                  if cdlist <> *blanks
060800160415                     and validate_commaSeparatedList(cdlist:
060900160419                                          1: 1: 3: errmsg);
061000160415                       *in41 = *on;
061100160415                       error = *on;
061200160415                  endif;
061300160415
061400160415                  //  validate  Whouse Locations
061500160415                  if whlist <> *blanks
061600160415                    and validate_commaSeparatedList(whlist:
061700160419                                   3: 1: 1: errmsg:*blanks:origin);
061800160416                       *in42 = *on;
061900160415                       error = *on;
062000160415                  endif;
062100160415
062200160415
062300160415                  update sub1;
062400160415               endfor;
062500160415
062600160415
062700160415                  RETURN error;
062800160415
062900160415     PValidate_rtn     E
063000160415
063100160415
063200160415
063300160415      //--------------------------------------
063400160415      // Procedure name: ConfirmSr
063500160415      // Purpose:        Confirm transactions
063600160415      //--------------------------------------
063700160415     P ConfirmSr       B
063800160415     D ConfirmSr       PI
063900160415     D confrmtype                     3    const
064000160415
064100160415           delete_sr();
064200160415
064300160415           select;
064400160415             when  confrmtype = 'SNG';
064500160415
064600160415                 update_table();
064700160415
064800160415             when  confrmtype = 'LST';
064900160415
065000160415               cnt = rrn;
065100160415               for xcnt = 1 to cnt;
065200160415
065300160415                 chain xcnt sub1;
065400160415                 update_table();
065500160415
065600160415               endfor;
065700160415
065800160415           endsl;
065900160415
066000160415           commit;
066100160415
066200160415           reset_sr();
066300160415           return;
066400160415
066500160415     P ConfirmSr       E
066600160415
066700151021
066800160415      //--------------------------------------
066900160415      // Procedure name: Update_table
067000160415      // Purpose:        Update PI dept Table
067100160415      //--------------------------------------
067200160415     P Update_table    B
067300160415     D Update_table    PI
067400160415
067500160415              exec sql
067600160415                 merge into pideptm as dept
067700160415                     using  (values ('A', :pidept#, :pitype#, :whlist,
067800160415                                     :cdlist, :revflag, :deptflag, :origin))
067900160415                              NEWROW (pidsts, piddept, pidpicode, pidwhlist,
068000160415                                      pidwhcdlst, pidrevflg, piddflg, pidorg)
068100160415
068200160415                            on dept.piddept = NEWROW.piddept  and
068300160415                               dept.pidorg = NEWROW.pidorg
068400160415
068500160415                       when MATCHED   then
068600160415                         update  set pidpicode =  NEWROW.pidpicode,
068700160415                                  pidwhlist =  NEWROW.pidwhlist,
068800160415                                  pidwhcdlst = NEWROW.pidwhcdlst,
068900160415                                  pidrevflg = NEWROW.pidrevflg,
069000160415                                  piddflg = NEWROW.piddflg,
069100160415                                  pidorg = NEWROW.pidorg
069200160415
069300160415                   //  when MATCHED and :cdlist = ' ' and :whlist = ' '
069400160415                   //    THEN
069500160415                   //      delete
069600160415
069700160415                       when not MATCHED and :pidept# <> 0  and :origin <> ' '
069800160415                         THEN
069900160415                         insert values(NEWROW.pidsts, NEWROW.piddept,
070000160415                                       NEWROW.pidpicode, NEWROW.pidwhlist,
070100160415                                       NEWROW.pidwhcdlst, NEWROW.pidrevflg,
070200160415                                       NEWROW.piddflg, NEWROW.pidorg);
070300160415
070400160415     P Update_table    E
070500160415
070600160415      //--------------------------------------
070700160415      // Procedure name: Reset_sr
070800160415      // Purpose:        reset Fields
070900160415      //--------------------------------------
071000160415     P reset_sr        B
071100160415     D reset_sr        PI
071200160415
071300160415                       pidept# = *zeros;
071400160415                       reset fmt1;
071500160415                       #mode = *blanks;
071600160415
071700160415     P reset_sr        E
071800160415
071900160415      //--------------------------------------
072000160415      // Procedure name: delete_sr
072100160415      // Purpose:        delete routine
072200160415      //--------------------------------------
072300160415     P delete_sr       B
072400160415     D delete_sr       PI
072500160415
072600160415                   exec sql
072700160415                      delete from pideptm
072800160415                           where piddept = :pidept#;
072900160415                  commit;
073000160415
073100160415     P delete_sr       E
073200151021
073300010301     **
073400010301**  Program Title
073500151021       Performance Incentive Department Defn.
