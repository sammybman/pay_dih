000100000000     H DATEDIT(*YMD)
000200091121     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'CCREDITDIR')
000300111013     H/define profoundui
000400000000      //*PROGRAMMER LESLIE B. DOODNAUTH
000500050514      // amended - allison fung
000600050514      // 14.5.2005
000700050514
000800000000     Fempmnt01d CF   e             Workstn
000900111013     F/IF DEFINED(WEB)
001000111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001100111013     F/ENDIF
001200111013
001300111013      /copy QCPYSRC,genpr
001400111013      /copy QCPYSRC,glpr
001500111013      /copy QCPYSRC,payPR
001600111013      /copy QCPYSRC,autpr
001700111013      /copy QCPYSRC,emppr
001800000000
001900141215     D                 ds                  inz
002000000000     D  emdept#                       3  0
002100000000     D  type_usg                      1  0 overlay(emdept#)
002200000000     D  reset_mth                     2  0 overlay(emdept#:2)
002300000000
002400000000     d  arr            s             40    dim(2) ctdata perrcd(1)
002500000000     D run_list        s              1    dim(2)
002700050528     d emapymth#       s                   like(emapymth)
003400000000     D error           s               n
003500050514     D cnt             s              2  0
003600050514     D found_ind       s               n
003700000000
003800060201     d TWO             c                   2
003900050527     d year_const      c                   12
004000000000     d msg1            c                   'Must Specify Months to Calc. Int.'
004100000000     D msg2            c                   'Invalid Employee Type -'
004200050515     D msg3            c                   'Invalid system org. length:check -'
004300050515     D msg4            c                   ' - System origin not found'
004400050517     D msg5            c                   'Pay Type is NOT an Emolument Type'
004500050528     d msg6            c                   'Invalid Apply Period Cycle'
004600050527     d msg7            c                   'Enter Period for Months to Apply'
004700050527     d msg8            c                   'Periods Completed cannot be +
004800050527     d                                       greater than periods per year'
004900050528     d msg9            c                   'Apply Period Cycle not compatib+
005000050528     d                                     le with  periods completed'
005100050530     d msg10           c                   'Function Keyp not available - +
005200050530     d                                      auto create Indicator not set'
005300050530     d
005400000000     D                SDS
005500000000     D  @PGM             *PROC
005600111013     D  @USER                358    367
005700000000
005800111013     D setup_limit     PR                  extpgm('EMPMNT0101')
005900000000     D                                2  0 const
006000050519     D                                1    const options(*nopass)
006100050515
006200111013     D setup_deduct    PR                  extpgm('EMPMNT0102')
006300050530     D                                2  0 const
006400050530     D                                1    const options(*nopass)
006500050530
007200050518
007300050514     D*--------------------------------------
007400050514     D* Prototype for procedure: ENTER_STYPE
007500050514     D*--------------------------------------
007600050514     D ENTER_STYPE     PR
007700050514
007800000000      /free
007900111013         // authority check
008000111013         monitor;
008100111013           authority_check('EMT':@pgm);
008200111013         on-error;
008300111013           show_msg(get_err_msg('DIH0054'));
008400111013           *inlr = *on;
008500111013           return;
008600111013         endmon;
008700111013
008800000000              exsr reset_sr;
008900000000
009000000000              exsr aainit;
009100000000
009200000000          // display first screen
009300050606         dow   0 = 0;
009400000000                exfmt fmt1;
009500000000
009600000000           // F3 = End Program
009700000000               if *in03;
009800000000                 *inlr = *on;
009900000000                 return;
010000000000               endif;
010100000000
010200000000            // F5 = Liability Type Enquiry
010300050514                 if *in05;
010400050514                     emseltyp# = lst_ltype();
010500000000                    iter;
010600000000                 endif;
010700000000
010800000000           //** Move blank to field
010900000000                  if emseltyp# = *zeros;
011000050517                     errmsg = get_err_msg('DIH0022');
011001141215                     show_msg(errmsg);
011100000000                     *in25 = *on;
011200000000                     iter;
011300000000                  endif;
011400000000
011500000000          //** Check file for record
011600050514                if not get_ltype_info('RD':emseltyp#:ematf_struc);
011700050514                    found_ind = *on;
011800050516                     *in99 = *on;
011900000000                      emdept# = emdept;
012000000000                      emsubtyp# = emsubtyp;
012100000000                      emchgtyp# = emchgtyp;
012200050528                      emapymth# = emapymth;
012300050514                   else;
012400050514                    found_ind = *off;
012500050516                    *in99 = *off;
012600050528                    emapymth# = *zeros;
012700050517                    clear ematf_struc;
012800050514                 endif;
012801141215
013000050530               leave;
013100000000              enddo;
013200000000
013300000000           //**  Display Main screen   **
013400000000
013500000000                    dow  0 = 0;
013501141215
013600000000                    exfmt fmt2;
013601141215
013800000000          //** F2 = Cancel
013900000000                    if *in02;
014000000000                      leave;
014100000000                    endif;
014200000000
014300000000          // ** F3 = End Program
014400000000                     if *in03;
014500000000                        *inlr = *on;
014600000000                        return;
014700000000                     endif;
014800000000
014900050530          // ** F6 = Setup Payrol Deductions
015000050530                if *in06;
015100050530
015200050530                  if  emautpyd <> 'Y';
015300050530                    errmsg = msg10;
015301141215                    show_msg(errmsg);
015400050530                    iter;
015500050530                  endif;
015600050530
015700050530                  setup_deduct(emseltyp#:emsubtyp#);
015800050602                  iter;
015900050530                endif;
016000050530
016100050517         // ** Ensure Description not blank
016200050517               if emdsp = *blanks;
016300050517                  errmsg = get_err_msg('DIH0022');
016301141215                  show_msg(errmsg);
016400050517                  iter;
016500050517               endif;
016600050517
016700050517         // **  Validate G.L.
016800000000               errmsg = *blanks;
016900000000               get_gl_info(emacct:errmsg:'V');
017000000000
017100000000               if errmsg <> *blanks;
017101141215                  show_msg(errmsg);
017200000000                  iter;
017300000000               endif;
017400000000
017500050517         // ** Validate List
017600000000                 if emchgtyp# <> *blanks;
017700000000      /END-FREE
017800000000     C                   movea     emchgtyp#     run_list
017900000000      /FREE
018000000000                cnt  = 1;
018100000000
018200060201             dow cnt <= TWO and run_list(cnt) <> *blank;
018300000000                get_cat(run_list(cnt):*omit:*omit:*omit
018400000000                        :error);
018500000000                if error;
018600000000                  errmsg = msg2 + run_list(cnt);
018700000000                  leave;
018800000000                endif;
018900050514                   cnt +=  1;
019000000000             enddo;
019100000000
019200000000           // display errmsg
019300000000                if errmsg <> *blanks;
019301141215                   show_msg(errmsg);
019400000000                   iter;
019500000000                   endif;
019600000000                endif;
019700000000
019800000000         // Validate Interest Fields
019900000000            if emintper <> *zeros;
020000000000               if emmth <= 0 or emmth > 12;
020100000000                  errmsg = msg1;
020101141215                  show_msg(errmsg);
020200000000                  iter;
020300000000               endif;
020400000000            endif;
020500050515
020600050515         //   Validate System Origins
020701151023                  if validate_commaSeparatedList(emsysflag:
020702151023                                              3: 1: 6: errmsg);
020703141215                     if errmsg <> *blanks;
020704141215                        show_msg(errmsg);
020705141215                     EndIf;
020800050518                    iter;
020900050518                  endif;
021000000000
021100050516           // Validate PAY TYPE FOR ALLOWANCE TO CREDIT  type 's' & emolument
021200050527              if empaytype <> *zeros;
021300050527                  if not get_payty_info('RD':empaytype:payty_struc)
021400050527                           and ptyrtype <> 'E' and ptypaytyp <> 'S'
021500050527                     or  get_payty_info('RD':empaytype:payty_struc);
021600050517                    errmsg = msg5;
021601141215                    show_msg(errmsg);
021700050517                    iter;
021800050527                  endif;
021900050527
022000050527           // Validate APPLY EVERY (PERIODS)
022100050527                 if emapymth <> *zeros;
022200050527                   if %rem(year_const:emapymth) <> *zeros;
022300050527                     errmsg = msg6;
022301141215                      show_msg(errmsg);
022400050527                      iter;
022500050527                   endif;
022600050527                     else;
022700050527                     errmsg = msg7;
022701141215                     show_msg(errmsg);
022800050527                     iter;
022900050527                 endif;
023000050516
023100050527             // periods completed
023200050528                 if  emprdcmplt <> *zeros;
023300050528                   if  emprdcmplt > emprdtotl;
023400050528                      errmsg = msg8;
023401141215                      show_msg(errmsg);
023500050528                      iter;
023600050528                    elseif %rem(emprdcmplt:emapymth) <> *zeros;
023700050528                      errmsg = msg9;
023701141215                      show_msg(errmsg);
023800050528                      iter;
023900050528                   endif;
024000050528
024100050606                   emprdcmplt = emprdcmplt / emapymth * emapymth#;
024200050527                 endif;
024300050530
024400050530             // pgm to field - total periods per year
024500050530                 emprdtotl = year_const / emapymth;
024600050527
024700050527             //  and next period
024800050530                   //errmsg = validate_prd(emnxtprd:'EMT');
024900050604                   //errmsg = validate_prd(emnxtprd);
025000050527                      if  errmsg <> *blanks;
025001141215                          show_msg(errmsg);
025100050527                         iter;
025200050527                      endif;
025300050527
025400050527              endif;
025500050527
025600000000         // if there is a subtype, allow for entry.
025700000000              if *in40;
025800000000                 select;
025900000000                   when emsubtyp# = 'Y';
026000050526                     enter_stype();
026100000000                   when emsubtyp# = 'N';
026200050526                     get_lstype_info('DL':emseltyp#:-1);
026300000000                   other;
026400000000                 endsl;
026500000000                 iter;
026600000000             endif;
026700000000
026800000000               emdept = emdept#;
026900000000               emseltyp = emseltyp#;
027000050514
027100050514               emchgtyp = emchgtyp#;
027200050514
027300050514           if not found_ind;
027400050514                  get_ltype_info('WR':emseltyp#:ematf_struc);
027500000000                  else;
027600000000
027700000000              select;
027800000000                // *in20 = F20 - Delete
027900000000                 when *in20;
028000050514                  get_ltype_info('DL':emseltyp#);
028100050526                  get_lstype_info('DL':emseltyp#:-1);
028200000000               // update record
028300000000                  other;
028400050514                  get_ltype_info('UP':emseltyp#:ematf_struc);
028500000000              endsl;
028600000000
028700000000            endif;
028800000000
028900050526            // ** Set up limits
029000050514                if emenflim = 'Y';
029100050519                   setup_limit(emseltyp#:emsubtyp#);
029200000000                endif;
029300000000
029400000000            leave;
029500000000            enddo;
029600111013
029700111013         //*****************************
029800111013         // Session Init
029900111013         //*****************************
030000111013
030100111013          begsr *inzsr;
030200111013            get_session_info('EMT':errmsg:@pgm);
030300111013            if errmsg <> *blanks;
030400111013              show_msg(errmsg);
030500111013              *inlr = *on;
030600111013              return;
030700111013            endif;
030800111013
030900111013          endsr;
031000111013
031100000000
031200000000          //************************************
031300000000          //   subroutine to clear displays
031400000000          //************************************
031500000000               begsr reset_sr;
031600000000                  emseltyp# = *zeros;
031700050528                  clear ematf_struc;
031800000000               endsr;
031900000000         //**********************************
032000000000         // FIRST INITILISATION ROUTINE    **
032100000000         //**********************************
032200000000             begsr aainit;
032300000000               coname = get_coname;
032400000000               @date = get_sys_date;
032500000000               @title = arr(1);
032600000000             endsr;
032700000000
032800050514      /end-free
032900050514     P*--------------------------------------
033000050514     P* Procedure name: ENTER_STYPE
033100050514     P* Purpose:        enter/create sub type
033200050514     P*--------------------------------------
033300050514     P ENTER_STYPE     B
033400050514     D ENTER_STYPE     PI
033500050514
033600050514     D found_ind       s               n
033700050514     D cnt             s              5  0
033800050514     C* Your calculation code goes here
033900050514      /free
034000050514
034100000000               dow 0 = 0;
034200000000               exfmt fmt3;
034300000000
034400000000               errmsg = *blanks;
034500000000
034600000000               if *in03;
034700000000                 *inlr = *on;
034800000000                return;
034900000000               endif;
035000000000
035100000000                if not get_lstype_info('RD':emseltyp#:easubtyp#:eastf_struc);
035200000000                    found_ind = *on;
035300050526                    easubtyp# = easubtyp;
035400000000                    else;
035500000000                    found_ind = *off;
035600000000                // Get Next Number
035700050514                dow not get_lstype_info('RD':emseltyp#:-1:eastf_struc:cnt);
035800050514                enddo;
035900050514
036000050514                   easubtyp# = easubtyp + 1;
036100050514
036200000000                // Clear input fields
036300050526                  clear eastf_struc;
036400000000               endif;
036500000000
036600000000           // Validate Description
036700000000                  if eadsp = *blanks;
036800000000                     errmsg = get_err_msg('DIH0022');
036801141215                     show_msg(errmsg);
036900000000                    iter;
037000000000                  endif;
037100000000
037200000000           // Changes made
037300000000                 if *in40;
037400000000                    iter;
037500000000                 endif;
037600000000
037700000000                leave;
037800000000               enddo;
037900000000
038000050526               easeltyp = emseltyp#;
038100050526               easubtyp = easubtyp#;
038200050514
038300050514                if found_ind;
038400050514                   get_lstype_info('UP':emseltyp#:easubtyp#:eastf_struc);
038500000000                  else;
038600050514                   get_lstype_info('WR':emseltyp#:easubtyp#:eastf_struc);
038700000000                endif;
038800050514
038900050514           return;
039000050514      /end-free
039100050514     P ENTER_STYPE     E
039200050515
047700000000
047800000000**
047900000000  EMPLOYEE TRANSACTION TYPE MAINTENANCE
048000000000XXXXXXXXXX0123456789XXXXXXXXXX0123456789
