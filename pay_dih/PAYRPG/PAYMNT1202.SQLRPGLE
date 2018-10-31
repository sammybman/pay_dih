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
001500151013      //       RPG PROGRAM NAME..: PAYMNT1202                   *
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
002600151012     Fpaymnt12d2cf   e             workstn
002700111028     F/IF DEFINED(WEB)
002800111028     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002900111028     F/ENDIF
003000111028
003100151021
003200111028      /copy QCPYSRC,genpr
003300111028      /copy QCPYSRC,salpr
003400111028      /copy QCPYSRC,autpr
003500151021
003600020128     D arr             S             40    DIM(1) CTDATA PERRCD(1)
003700040302     D found_ind       S               n
003800060612     D counter         S              5  0
003900021018     **
004000021018     D msg1            c                   'Cannot delete - Record In Use'
004100151021     D msg2            c                   'Blank Entry not allowed'
004200151021
004300151021       DCL-C SQLStateOK      '00000';
004400151021
004500151021       //---------------------------------------------------------------
004600151021       // Performance Incentive Field Struc
004700151021       //---------------------------------------------------------------
004800151021       Dcl-Ds PIStruc;
004900151021         status          Char(1);
005000151021         pitype          Packed(2);
005100151021         desc#           Char(30);
005200151026         cdlist          Char(30);
005300151021         revflag         Char(1);
005400151027         whlist          Char(75);
005500151021       End-Ds;
005600151021
005700160409
005800160409       //   List PI Types
005900160409       // ----------------
006000160409       Dcl-Pr Lst_PItypes          extpgm('PAYENQ06R');
006100160409         *N                      Packed(2);
006200160409       End-Pr;
006300160409
006400151021
006500040302     **
006600010815     D                sds
006700950711     D  @PGM             *PROC
006800111028     D  @USER                358    367
006900020128     d
007000021017     **
007100010301     *******************
007200010301     ** Program start **
007300010301     *******************
007400111028
007500111028
007600021217
007700151021               dow 0 = 0;
007800021017
007900021017                   exsr aainit;
008000021017
008100021017                   exfmt fmt1;
008200021017
008300021017       //  F3 - Program End
008400021017                   if *in03;
008500021017                      *inlr = *on;
008600021017                       return;
008700021017                   endif;
008800021017
008900151021        // F1 - Incentive Types Enquiry
009000021017                   if  *in01;
009100160409                      lst_PItypes(pitype#);
009200021017                       iter;
009300021017                    endif;
009400021017
009500021017                   #mode = *blanks;
009600021017
009700021017       //   Validate Entry
009800151021                   if pitype# = *zeros;
009900151021                      errmsg = msg2;
010000151021                      iter;
010100021017                   endif;
010200021018
010300151021       //   If P.I. Type Found
010400151021                  exec sql
010500151021                     select * into :PIStruc from  picodem
010600151021                        where piccode = :pitype#;
010700151021
010800151021                  if  sqlstt = SQLStateOK;
010900021017
011000021017                          #mode = 'AMEND ';
011100151021                          found_ind = *on;
011200021017
011300151021       //   If P.I. Type Not found
011400151021                        else;
011500151021                          #mode = ' ADD ';
011600151021                          clear PIStruc;
011700151021                          found_ind = *off;
011800151021                  endif;
011900021017
012000151021                 leave;
012100151021               enddo;
012200021017
012300021017       //   Display second screen
012400151021                    dow 0 = 0;
012500021017
012600021018                    exfmt fmt2;
012700021018
012800021018                    clear errmsg;
012900021017
013000021017       //   F2 - Cancel
013100021017                  if  *in02;
013200021018                       exsr reset_sr;
013300060612                       leave;
013400021017                  endif;
013500060612
013600060612          // Production Location ENQUIRY
013700060612                 if  *in04;
013800151021               //  lst_whcon(whid:*blank:'P');
013900060612                   iter;
014000060612                 endif;
014100021017
014200021017       //   Validate Description
014300021017                  if desc# = *blanks;
014400021018                        errmsg = get_err_msg('DIH0022');
014500021017                        iter;
014600021018                  endif;
014700060612
014800151026       //  validate  W/house Transaction Codes
014900151026                  if cdlist <> *blanks
015000151026                    and validate_commaSeparatedList(cdlist:
015100151026                                                    1: 1: 3: errmsg);
015200151021                   iter;
015300060612                  endif;
015400060612
015500151027       //  validate  W/house/Production List
015600151027                  if whlist <> *blanks
015700151027                    and validate_commaSeparatedList(whlist:
015800151027                                                    3: 1: 1: errmsg);
015900151027                   iter;
016000151027                  endif;
016100021017
016200021017       //  F15 - Confirm Record
016300021017                  if *in15;
016400021017                      exsr confirm_sr;
016500021017                       leave;
016600021017                  endif;
016700021017
016800021017       //  F20 - Delete Record
016900021017                  if *in20;
017000021017                     exsr delete_sr;
017100021017                      if errmsg <> *blanks;
017200021017                        iter;
017300021017                       endif;
017400021017
017500021017                  leave;
017600021017                  endif;
017700021017
017800021017                  enddo;
017900111028
018000111028         //*****************************
018100111028         // Session Init
018200111028         //*****************************
018300111028
018400111028          begsr *inzsr;
018500151021            get_session_info('PAY':errmsg:@pgm);
018600111028            if errmsg <> *blanks;
018700111028              show_msg(errmsg);
018800111028              *inlr = *on;
018900111028              return;
019000111028            endif;
019100111028
019200111028          endsr;
019300111028
019400021018       //********************************
019500021018       //** Sub-routine to confirm record
019600021018       //*******************************
019700021017           begsr confirm_sr;
019800021018
019900151021
020000151021              exec sql
020100151021                 merge into picodem as code
020200151026                     using  (values ('A', :pitype#, :desc#, :cdlist,
020300151027                                     :revflag, :whlist))
020400151021                              NEWROW (picsts, piccode, picdesc, picwhcdlst,
020500151027                                      picrevflg, picwhlst)
020600151021
020700151021                            on code.piccode = NEWROW.piccode
020800151021
020900151021                       when MATCHED   then
021000151021                         update  set picdesc =  NEWROW.picdesc,
021100151021                                  picwhcdlst = NEWROW.picwhcdlst,
021200151027                                  picrevflg = NEWROW.picrevflg,
021300151027                                  picwhlst = NEWROW.picwhlst
021400151021
021500151021                       when not MATCHED and :pitype# <> 0
021600151021                         THEN
021700151021                         insert values(NEWROW.picsts, NEWROW.piccode,
021800151021                                       NEWROW.picdesc, NEWROW.picwhcdlst,
021900151027                                       NEWROW.picrevflg, NEWROW.picwhlst);
022000021018
022100151026                        commit;
022200021018                        exsr reset_sr;
022300151021
022400021018                      endsr;
022500021018       //********************************
022600021018       //** Sub-routine to delete record
022700021018       //*******************************
022800021018           begsr delete_sr;
022900021018
023000151021                   exec sql
023100151021                      delete from picodem
023200151021                           where piccode = :pitype#;
023300151026
023400151026                   commit;
023500151021
023600151021           endsr;
023700021018
023800021018       //********************************
023900021018       //** Sub-routine to clear displays
024000021018       //********************************
024100021018           begsr reset_sr;
024200151021                       pitype# = *zeros;
024300151021                       reset fmt1;
024400151021                       #mode = *blanks;
024500151021           endsr;
024600021018       //***************************************************
024700021018       //** First initilisation routine for all programs  **
024800021018       //***************************************************
024900021018           begsr aainit;
025000021018                   CONAME = get_coname;
025100151021                   @DATE = get_sys_date;
025200151021                 //  select header for program
025300151021                   @title = arr(1);
025400021018
025500151021           ENDSR;
025600151021
025700151021
025800151021
025900151021
026000151021
026100010301     **
026200010301**  Program Title
026300151012       Performance Incentive types
