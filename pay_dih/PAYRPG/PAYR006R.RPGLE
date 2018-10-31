000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300090415      //******* PROGRAMMER - S.Britton         ************
000400090415      //*******            - C.Williams        ************
000500111002     Fpayrtypl01if   e           K disk    usropn
000600111002     Fpayrdefl03if   e           K DISK    usropn
000700111002     Fpayrresl01if   e           k disk    usropn
000800111002     Fpaytimfl02uf a e           K disk    usropn
000900090415      // Employee Type Master file
001000111002     Femptypel02if   e           k disk    usropn
001100111002     Fpaycycd   if   e           K DISK    usropn
001200010427     Fpayr006d  CF   e             WORKSTN
001300010426     f                                     sfile(sflrec:rrn)
001400111002     F/IF DEFINED(WEB)
001500111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001600111002     F/ENDIF
001700090415      // Payroll PROTOTYPES
001800110627      /copy QCPYSRC,PAYPR
001900110627      /copy QCPYSRC,genPR
002000110627      /copy QCPYSRC,autPR
002100110627      /copy QCPYSRC,emppr
002200110627
002300010802     D list#           s             15
002400010820     D error           s               n
002500010820     D dept_hrs        s              5  2
002600010802     D name#           s             40
002700010806     D payrun_factor   s              7  3
002800020304     D payrun_qty      s              2  0
002900010806     D sav_type        s              1
003000010806     D trn_hrs         s              5  2
003100010806     D trn_value       s             11  2
003200010802     D err#            s              7
003300010802     D rrn             s              4  0
003400010802     D arr             s             40    DIM(1) CTDATA PERRCD(1)
003500010427     D start           s               n   inz(*on)
003600010427     D valemp          s               n   inz(*off)
003700010427     D calcs           s               n   inz(*off)
003800040717     D rcnt            s              6  0
003900090415      //
004000020113     D msg1            c                   'Invalid Run Type Entered'
004100010724     D msg2            c                   'Invalid Mimic Run Type Entered'
004200010724     D msg3            c                   'Mimic type cannot be Run type'
004300010724     D msg4            c                   'This mimic type cannot be used for +
004400010724     D                                     Processing'
004500020113     D msg5            c                   'Global Change Completed Successfull-
004600040717     D                                     y - '
004700040717     D msg51           c                   ' records updated'
004800020113     D msg6            c                   'Invalid amount Entered'
004900020113     D msg7            c                   'Must make valid selection'
005000020113     D msg8            c                   'Invalid Pay-type or type entered no-
005100020113     D                                     t selected for payroll'
005200090415      //
005300980209     D                SDS
005400980209     D  @PGM             *PROC
005500090415      //
005600010427     C     trn_key       klist
005700010427     C                   kfld                    run_type
005800010427     C                   kfld                    emp#
005900010802     C                   kfld                    emol_typ#
006000090415      //
006100020113     C     val_key       klist
006200020113     C                   kfld                    run_type
006300020113     C                   kfld                    global_typ
006400090415      //
006500020113     C                   move      '0'           *in
006600090415      /FREE
006700090415       clear global_typ;
006800090415       clear global_cod;
006900090415       clear global_amt;
007000010427
007100090415       //*********************
007200090415       // Get run type info **
007300090415       //*********************
007400111002         // authority check
007500111002         monitor;
007600111002           authority_check('PAY':@pgm);
007700111002         on-error;
007800111002           show_msg(get_err_msg('DIH0054'));
007900111002           *inlr = *on;
008000111002           return;
008100111002         endmon;
008200090420
008300090415       exsr aainit;
008400090415       dow start = *on;
008500090420
008600090415         exfmt fmt0;
008700090415         clear errmsg;
008800090420
008900090415         if *in03;
009000090415           *inlr = '1';
009100090415           return;
009200090415         endif;
009300090420
009400090415         chain run_type prtyfmt;
009500090415         // Invalid Run type entered
009600090415         if not(%found);
009700090415           errmsg = msg1;
009800090415           iter;
009900090415         endif;
010000010724
010100090415         //*****************************
010200090415         // SPECIAL Run type entered  **
010300090415         //*****************************
010400090415         if prtyprno = 0;
010500090415           dow 0 = 0;
010600090415             *in99 = *on;
010700090415             exfmt fmt0;
010800090415             clear errmsg;
010900090415             // go back
011000090415             if *in02;
011100090415               *in99 = *off;
011200090415               iter;
011300090415             endif;
011400090415             // End Program
011500090415             if *in03;
011600090415               *inlr = '1';
011700090415               return;
011800090415             endif;
011900090415             //
012000090415             chain mim_type prtyfmt;
012100090415             // Invalid Run type entered
012200090415             if not(%found);
012300090415               errmsg = msg2;
012400090415               iter;
012500090415             endif;
012600090415             // mimic = runtype
012700090415             if mim_type = run_type;
012800090415               errmsg = msg3;
012900090415               iter;
013000090415             endif;
013100090415             // Mimic cannot used for processing
013200090415             if prtyprno = 0;
013300090415               errmsg = msg4;
013400090415               iter;
013500090415             endif;
013600090415             //
013700090415             chain mim_type prrefmt;
013800090415             sav_type = mim_type;
013900090415             leave;
014000090415           enddo;
014100090415           //*****************************
014200090415           // Set check-for-payment option to YES
014300090415           check_opt = 'Y';
014400010724
014500090415         else;
014600090415           chain run_type prrefmt;
014700090415           sav_type = run_type;
014800090415           // Set check-for-payment option to NO
014900090415           check_opt = 'N';
015000090415         endif;
015100090415         //
015200090415         if %found;
015300090415           list# = prrelist;
015400090415         endif;
015500090415         // get Run-type cycle
015600090415         chain prtycycode cycfmt;
015700090415         payrun_factor = cycyearf;
015800090415         payrun_qty  = prtycycqty;
015900090415         //
016000090415         valemp = *on;
016100090415         leave;
016200090415       enddo;
016300010806
016400090415       //*************************
016500090415       // get employee info etc.**
016600090415       //*************************
016700090415       dow valemp = *on;
016800090415         exfmt fmt1;
016900090415         clear errmsg;
017000090415         //
017100090415         if *in03;
017200090415           *inlr = '1';
017300090415           return;
017400090415         endif;
017500090415         // go Back
017600090415         if *in02;
017700090415           leave;
017800090415         endif;
017900020113
018000020113
018100090415         select;
018200090415         when emp# <> *zeros;
018300090415           *in77 = *off;
018400090420
018500090415           // Validate Employee number & get employee info.
018600090415           status = get_emp_info(emp#:dept:list#:
018700090415               name#:*omit:*omit:*omit:err#);
018800090420
018900090415           if err# <> *blanks;
019000090415             // Error message
019100090415      /END-FREE
019200010802     C                   Call      'MESSAGE'
019300010802     C                   Parm      err#          MSGID             7
019400010802     C                   Parm      *BLANKS       ERRMSG
019500090415      /FREE
019600090415           else;
019700090415             emp_name = name#;
019800090415             errmsg = *blanks;
019900090415           endif;
020000090420
020100090415           if errmsg <> *blanks;
020200090415             iter;
020300090415           endif;
020400090420
020500090415           exsr load_sheet;
020600090415           // for Global Update *********
020700090415         when global_cod <> *blanks;
020800090415           // validate amount
020900090415           if global_amt <= *zeros;
021000090415             errmsg = msg6;
021100090415             iter;
021200090415           endif;
021300090415           // Validate paytype
021400090415           chain val_key prdffmt;
021500090415           if not(%found);
021600090415             errmsg = msg8;
021700090415             iter;
021800090415           endif;
021900090420
022000090415           // Show function key for confirming update
022100090415           if not(*in77);
022200090415             *in77 = *on;
022300090415             iter;
022400090415           endif;
022500090415           // F21 = Confirm Global Update
022600090415           if *in21;
022700090415             rcnt = *zeros;
022800090415      /END-FREE
022900020113     C                   callb     'PAYR006R02'
023000020113     C                   parm                    run_type
023100020113     C                   parm                    global_typ
023200020113     C                   parm                    global_cod
023300020113     C                   parm                    global_amt
023400020115     C                   parm                    list#
023500040717     C                   parm                    rcnt
023600040717     C                   parm                    global_cls
023700090420
023800090415      /FREE
023900090415             errmsg = msg5 + %char(rcnt)
024000090415                 + msg51;
024100090415             *in77 = *off;
024200090415             leave;
024300090415           endif;
024400090415           // Invalid Selection
024500090415         other;
024600090415           errmsg = msg7;
024700090415           iter;
024800090415         endsl;
024900090415         //
025000090415         *in60 = *on;
025100090415         calcs = *on;
025200090415         start = *off;
025300090415         leave;
025400090415       enddo;
025500090415       //**********************
025600090415       // Program calcs.
025700090415       //**********************
025800090415       dow calcs = *on;
025900010426
026000090415         write footer;
026100090415         exfmt sflctl;
026200090415         // End Program
026300090415         if *in03;
026400090415           *inlr = '1';
026500090415           return;
026600090415         endif;
026700090415         // go Back
026800090415         if *in02;
026900090415           leave;
027000090415         endif;
027100090415         // Update transactions
027200090415         exsr update_rtn;
027300090415         // f15 = Confirm
027400090415         if *in15;
027500090415           exsr confirm_rtn;
027600090415           leave;
027700090415         endif;
027800010426
027900090415       enddo;
028000111002
028100111002         //*****************************
028200111002         // Session Init
028300111002         //*****************************
028400111002
028500111002          begsr *inzsr;
028600111006            get_session_info('PAY':errmsg:@pgm);
028700111002            if errmsg <> *blanks;
028800111002              show_msg(errmsg);
028900111002              *inlr = *on;
029000111002              return;
029100111002            endif;
029200111002
029300111002            open(e) payrtypl01;
029400111002            open(e) payrdefl03;
029500111002            open(e) payrresl01;
029600111002            open(e) paytimfl02;
029700111002            open(e) emptypel02;
029800111002            open(e) paycycd;
029900111002
030000111002          endsr;
030100111002
030200010426
030300090415       //*****************************
030400090415       // Load time-sheet records   **
030500090415       //*****************************
030600090415       begsr load_sheet;
030700090420
030800090415         chain status empfmt;
030900020113
031000090415         // clear subfile and reset all values
031100090415         *in33 = '1';
031200090415         write sflctl;
031300090415         rrn = 0;
031400090415         *in33 = '0';
031500020113
031600090415         // Load subfile
031700090415         setll sav_type prdffmt;
031800090415         reade sav_type prdffmt;
031900090420
032000090415         dow not(%eof);
032100090420             get_payty_info('RD':prdftype:payty_struc);
032200050519
032300090420            // Show record if link request = run type
032400090415           if ptylink = *blanks
032500090415                 or ptylink <> *blanks
032600090415                 and ptylink = sav_type;
032700020113
032800090415             //  Protect Hours Fields for Money type transactions
032900090415             if emppyopt = 'H' and ptypaytyp = 'B'
033000090415                   or emppyopt = 'H' and ptypaytyp = 'H'
033100090415                   or emppyopt = 'M' and ptypaytyp = 'H';
033200090415               *in88 = *off;
033300090415             else;
033400090415               *in88 = *on;
033500090415             endif;
033600090415             hrs_ind = *in88;
033700090420
033800090420             rrn += 1;
033900090415             emol_typ# = ptyptype;
034000090415             emol_name = ptydesc;
034100090415             ptyrtype# = ptyrtype;
034200090415             ptyprior# = ptyprior;
034300090420
034400090415             chain trn_key timfmt;
034500090415             if %found;
034600090415               wrkhrs# = timhrs;
034700090415               wrkamt# = timamt;
034800090415             else;
034900090415               wrkhrs# = 0;
035000090415               wrkamt# = 0;
035100090415             endif;
035200090415             write sflrec;
035300090415           endif;
035400020113
035500090415           reade sav_type prdffmt;
035600090415         enddo;
035700090415       endsr;
035800090420
035900090415       //************************
036000090415       // Confirm transactions **
036100090415       //************************
036200090415       begsr confirm_rtn;
036300090415      /END-FREE
036400010806     c                   z-add     rrn           x                 3 0
036500010802     c                   do        x             y                 3 0
036600090415      /FREE
036700090415           chain y sflrec;
036800090415           *IN07 = NOT %FOUND;
036900090415           chain trn_key timfmt;
037000090415           //
037100090415           timemp = emp#;
037200090415           timhrs = wrkhrs#;
037300090415           timrtype = run_type;
037400090415           timcode = ptyrtype#;
037500090415           timprior = ptyprior#;
037600090415           timtyp  = emol_typ#;
037700090415           timamt  = wrkamt#;
037800090415
037900090415           if %found;
038000090415             if timamt <> *zeros;
038100090415               update timfmt;
038200090415             Else;
038300090415               delete timfmt;
038400090415             Endif;
038500090420
038600090415           Else;
038700090420
038800090415             // Prevent from writing a record with 0 value
038900090415
039000090415             if timamt <> *zeros;
039100090415               write timfmt;
039200090415             endif;
039300090415
039400090415           endif;
039500090415
039600090415         enddo;
039700090415       endsr;
039800090420
039900090415       //************************
040000090415       // update transactions  **
040100090415       //************************
040200090415       begsr update_rtn;
040300090415      /END-FREE
040400010806     c                   z-add     rrn           x                 3 0
040500010802     c                   do        x             y                 3 0
040600090415      /FREE
040700090415           chain y sflrec;
040800090415           *in88 = hrs_ind;
040900090415           if %found;
041000010820
041100090415             select;
041200090415             when check_opt = 'Y';
041300090420
041400090415      /END-FREE
041500010806     C                   callb     'PAYR006R01'
041600010820     C                   parm                    sav_type
041700010806     C                   parm                    emp#
041800010806     C                   parm                    prtyldate
041900010806     C                   parm                    emol_typ#
042000010806     C                   parm      *zeros        trn_hrs
042100010806     C                   parm      *zeros        trn_value
042200090420
042300090420              // For Basic wage option
042400090415      /FREE
042500090415               if not(*in88) and emol_typ# = 1;
042600090415                 get_Dept_info(dept:error:dept_hrs);
042700090415                 if (wrkhrs# + trn_hrs) > dept_hrs;
042800090415                   wrkhrs# =  dept_hrs - trn_hrs;
042900090415                 endif;
043000090415               endif;
043100090415
043200090420               if  wrkamt# = *zeros
043300090420                 or wrkamt# <> *zeros
043400090420                   and ovrflag <> 'Y';
043500070622                   wrkamt#= get_emp_pay(emp#:dept:emol_typ#:
043600070622                                wrkhrs#:payrun_factor:
043700070622                                       payrun_qty:status);
043800090420               endif;
043900090415
044000090415               if *in88;
044100090415                 wrkamt# = wrkamt# - trn_value;
044200090415                 if wrkamt# < *zeros;
044300090415                   wrkamt# = *zeros;
044400090415                 endif;
044500090415               endif;
044600090415
044700090420             other;
044800090415
044900090415               if not(*in88);
045000090415
045100090415                 if  wrkamt# = *zeros
045200090415                  or wrkamt# <> *zeros
045300090415                  and ovrflag <> 'Y';
045400090415                   wrkamt#= get_emp_pay(emp#:dept:emol_typ#:
045500070621                            wrkhrs#:payrun_factor:
045600070621                                        payrun_qty:status);
045700090415                 endif;
045800090415               endif;
045900090415
046000090415             endsl;
046100090415
046200090415           endif;
046300090415
046400090415           wrkamt#= round_up(wrkamt#:'A');
046500090415           update sflrec;
046600090415         enddo;
046700010802
046800090415       endsr;
046900990413
047000090415       //*************************************************
047100090415       // First initilisation routine for all programs  **
047200090415       //*************************************************
047300090415       BEGSR AAINIT;
047400090415         coname = get_coname;
047500090415         @date = get_sys_date;
047600090420         // Select header for program
047700090420         @title = arr(1);
047800090415      /END-FREE
047900010820     C                   time                    @time             6 0
048000090420
048100090415      /FREE
048200090415       ENDSR;
048300090420
048400090415      /END-FREE
048500010426**  Program Title
048600010426      Payroll Time Sheet Entry
