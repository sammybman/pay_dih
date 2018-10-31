000100060330     H DATEDIT (*YMD) BNDDIR('DISDIR') ACTGRP('QILE') DFTACTGRP(*NO)
000401151104      //*********************************************************
000402151104      //              D I S E R V   S Y S T E M S.              *
000403151104      //                  PAYROLL SYSTEM                        *
000404151104      //                                                        *
000405151104      //                         BY                             *
000406151104      //                                                        *
000407151104      //             Diserv systems Team                        *
000408151104      //             P.O BOX XXXXX, GEORGETOWN,GUYANA           *
000409151104      //                                                        *
000500030902      //                                                        *
000600151104      //       Program Name......: PAYMNT1207                   *
000700030902      //                                                        *
000800151104      //       Created By........: Samuel Britton               *
000900151110      //       Date..............: 04.11.2015                   *
001000030902      //                                                        *
001100151104      //   Description: DUPLICATE Incentive Figures             *
001200030902      //                                                        *
001300030902      //*********************************************************
001400151104     Fpaymnt12d7CF   E             workstn
001401151104     F/IF DEFINED(WEB)
001402151104     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001403151104     F/ENDIF
001600030902
001700151104      /copy Qcpysrc,genpr
001701151109      /copy Qcpysrc,autpr
001900030902
002000030902     Darr              s             40    dim(1) ctdata perrcd(1)
002200060404     D descriptor      s            389
002300060211     D err#            s              7
002301151104     D fromPrno        s              6  0
002400151104     D toPrno          s              6  0
002401151109     D date#           s              8  0
002402151109     D time#           s              6  0
002403151109     D month           s              2  0
002500151104     D replace_ind     s               n
002501151109     D data_OK         s               n
002600151104     D xcnt            s              4  0
002601151109
002602151109     d VarLen          s             10I 0 Inz(%size(Var))
002603151109     d ApiErr          s             15a
002604151109     d Caller          s             50a
002605151109     d WhoCalled       s             10a
002606151109     d i               s             10I 0
002607151109
003000060211
003100060327     d msg1            c                   'TO YEAR cannot be same or less +
003200060327     d                                      than FROM YEAR'
003201151109     d msg2            c                   'Target range specified overlaps  +
003202151109     d                                     source period'
003400060327     d msg3            c                   'Second period CANNOT be less than +
003500060327     d                                      first allocated period'
003501151109     d msg4            c                   'Update completed successfully'
003502151109     d msg5            c                   'Nothing Configured - Please Try +
003503151109     d                                     again'
003504151109     d msg6            c                   'Period settings configured: +
003505151109     d                                     Press F15 to process'
003506151109     d msg7            c                   'Year settings configured: +
003507151109     d                                     Press F15 to process'
003508151109     d msg8            c                   'Invalid number of Periods Specified'
003600030902     **
003700030902     d                sds
003800030902     D  @pgm             *proc
003900151107     D  @user                358    367
004000030902     D
004100151109     Dmain_prog        PR                  extpgm('PAYMNT1207')
004200151109     D                                6  0 options(*nopass)
004300060211     Dmain_prog        PI
004400151109     D prno                           6  0 options(*nopass)
004500060211
007400060330
007600060327         //**************************
007700060327         //*** Start of Program   ***
007800060327         //**************************
007900060211
007901151109
007902151109             FindCaller(Var:VarLen:'CSTK0100':JobIdInf
007903151109                         :'JIDF0100':ApiErr);
007904151109               For i = 1 to EntryCount;
007905151109                  Entry = %subst(Var:Offset + 1);
007906151109                  Caller = %trim(PgmLib) + '/' + %trim(PgmNam);
007907151109                  If (PgmNam <> @pgm and WhoCalled = *blanks);
007908151109                     WhoCalled = PgmNam;
007909151109                  Endif;
007910151109                  Offset = Offset + EntryLen;
007911151109               Endfor;
007912151109
007913151109            // Program CANNOT be called independently
007914151109            if %parms  > 0;
007915151109               monitor;
007916151110                authority_check('PAY':'PAYMNT12R'
007917151110                                :get_level('PAYMNT12R':'PIDUPRATE'));
007918151109               on-error;
007919151109                 show_msg(get_err_msg('DIH0054'));
007920151109                 *inlr = *on;
007921151109                 return;
007922151109               endmon;
007923151109
007924151109               *in99 = *on;
007925151109               prno# = prno;
007926151109               choice = 1;
007927151109               else;
007928151109                 if WhoCalled <> 'PAYMNT12R';
007929151109                   *inlr = *on;
007930151109                   return;
007931151109                 endif;
007932151109            endif;
007933151109
007934151109
008100060330           exsr  aainit;
008200060211
008300060330           dow  0 = 0;
008400060211              exfmt  fmt1;
008500060211
008600060211              errmsg = *blanks;
008700060211
008800060327             // F3 = End Program
008900060211               if  *in03;
009000060211                  *inlr =  *on;
009100060211                  return;
009200060211               endif;
009300060211
010000060211
010001151109              if  *in15 and data_OK;
010002151104                 replace_ind =  replaceflg = 'Y';
010003151104                 if choice = 1;
010004151109                     toPrno = toprd1;
010005151109                     dow toPrno <= toprd2;
010006151109                    // toPrno = %int(%subst(%char(prno#):1:4) + %char(xcnt));
010007151104                       Duplicate_stuff(replace_ind : prno# : toPrno);
010008151109                       toPrno = get_new_prd(toPrno:1:'A');
010009151109                     enddo;
010010151104                 elseif choice = 2;
010011151104                     for xcnt = 1 to 12;
010012151104                       fromPrno = %int(%char(toyear) + %char(xcnt));
010013151104                       toPrno = %int(%char(fryear) + %char(xcnt));
010014151104                       Duplicate_stuff(replace_ind : fromPrno : toPrno);
010015151104                     endfor;
010016151104                 endif;
010017151109
010018151109                 commit;
010019151109                 errmsg = msg4;
010020151109                 data_OK  = *off;
010021151109                 iter;
010200060330
010201151109              endif;
010202151109
010203151109              select;
010204151109                when choice = 1;
010205151109                   if validate_prno();
010206151109                      iter;
010207151109                   endif;
010208151109                when choice = 2;
010209151109                   if validate_year();
010210151109                      iter;
010211151109                   endif;
010212151109              endsl;
012900060330
013000060211              leave;
013100060211           enddo;
013101151104
013205031111
013300060330         //************************************************
013400060211         //** subroutine to retrieve header information
013500060328         //************************************************
013600060211           begsr aainit;
013700060211             coname = get_coname();
013800151109             @date  = get_sys_date(date#:time#);
013900060211             @title = arr(1);
014000060211           endsr;
014100060211
014300060330     P********************************************************
014400060328     P* Procedure name: validate_prd                        **
014500060328     P* Purpose:        Validate screen - duplicate periods **
014600060328     P********************************************************
014700060404     Pvalidate_prno    B
014800060404     Dvalidate_prno    PI              n
015000151104
015100060330               write record;
015200060328               exfmt winfmt1;
015300040129
015301151109               data_OK  = *off;
015302151109
015701151109               if  toprd1 = *zeros
015702151109                or  toprd2 = *zeros
015703151109                or  prno# = *zeros;
015704151109                  errmsg = msg5;
015705151109                  return *on;
015706151109               endif;
015707151109
015708151109            // validation from/to month
015709151109               if diff_prd(toprd2: toprd1) > 12;
015712151109                  errmsg = msg8;
015713151109                  return *on;
015714151109               endif;
015802151109
015900151109            // ensure 'to period' is greater than 'from period'
016000060330               if toprd2 < toprd1;
016100060330                  errmsg = msg3;
016200060330                  return *on;
016300060327               endif;
016301151109
016303151109         // month = %int(%subst(%char(prno#):5:2));
016304151109
016305151109            // ensure no overlap for source and target periods
016306151110               if prno# <= toprd2
016307151110                and prno# >= toprd1;
016308151109                  errmsg = msg2;
016309151109                  return *on;
016310151109               endif;
016311151109
016400060327
016401151109               errmsg = msg6;
016402151109               data_OK  = *on;
016500060330               return *off;
016600151104
016700060404     Pvalidate_prno    e
016800060211
016900060328     P*****************************************************
017000060328     P* Procedure name: validate_year                    **
017100060328     P* Purpose:        validate screen - duplicate year **
017200060328     P*****************************************************
017300060327     P validate_year   B
017400060330     D validate_year   PI              n
017500060211
017600151104
017700060330               write record;
017800151104               exfmt winfmt2;
017900060327
017901151109               data_OK  = *off;
017902151109
018000060330            // ensure no blank entries
018100060330               if fryear = *zeros or
018200060330                  toyear = *zeros;
018300060330                  errmsg = get_err_msg('DIH0022');
018400060330                  return *on;
018500060327               endif;
018600060327
018700060327            // ensure 'to year' is greater than 'from year'
018800060327               if toyear <= fryear;
018900060330                  errmsg = msg1;
019000060330                  return *on;
019100060327               endif;
019200060327
019201151109               errmsg = msg7;
019202151109               data_OK  = *on;
019300060330               return *off;
019400151104
019500060330     P validate_year   E
019600060327
025900060330
025901151104
025902151104
025903151104      //........................................//
025904151104      //Procedure to Duplicate PI Target data   //
025905151104      //........................................//
025906151104       Dcl-Proc Duplicate_stuff;
025907151104         Dcl-Pi Duplicate_stuff;
025908151104            Replace_old_data   Ind        Const;
025909151104            current_prno       Packed(6)  Const;
025910151104            next_prno          Packed(6)  Const;
025911151104         End-pi;
025912151104
025913151109
025919151104           // Duplicate PI Rates
025920151104           //--------------------
025921151104          exec sql
025922151104             merge into picommrm as prod
025923151104               using picommrm as newrow
025924151104                  on prod.cmrtype = newrow.cmrtype and
025925151104                  prod.cmrdept# = newrow.cmrdept# and
025926151104                  prod.cmrpcod = newrow.cmrpcod and
025927151110                  prod.cmrper  = newrow.cmrper and
025928151104                  prod.cmrprno =  :next_prno  and
025929151104                  newrow.cmrprno = :current_prno
025930151104
025931151104
025932151104               when MATCHED  and :replace_old_data = '1' then
025933151104                  update  set cmrqty =  NEWROW.cmrqty,
025934151104                              cmrqtyu = NEWROW.cmrqtyu,
025935151104                              cmrper  = NEWROW.cmrper,
025936151104                              cmrperu = NEWROW.cmrperu,
025937151104                              cmrrate#1 = NEWROW.cmrrate#1,
025938151104                              cmrrate#2 = NEWROW.cmrrate#2,
025939151104                              cmrrate#3 = NEWROW.cmrrate#3,
025940151104                              cmruser   = :@user,
025941151109                              cmrdate   = :date#,
025942151109                              cmrtime   = :time#
025943151104
025944151104
025945151109               when not MATCHED  and newrow.cmrprno = :current_prno
025946151104                  THEN
025947151104                    insert values(NEWROW.cmrsts, NEWROW.cmrtype,
025948151104                                  NEWROW.cmrdept#, NEWROW.cmrpcod,
025949151104                                  NEWROW.cmrqty, NEWROW.cmrqtyu,
025950151104                                  NEWROW.cmrper, NEWROW.cmrperu,
025951151104                                  NEWROW.cmrrate#1, NEWROW.cmrrate#2,
025952151109                                  NEWROW.cmrrate#3, :@user, :date# ,
025953151109                                  :time#, :next_prno);
025954151104
025955151104           // Duplicate PI Targets
025956151104           //-----------------------
025957151104          exec sql
025958151104             merge into picommcm as prod
025959151104               using picommcm as newrow
025960151104                  on prod.cmctype = newrow.cmctype and
025961151104                  prod.cmcdept# = newrow.cmcdept# and
025962151104                  prod.cmcpcod = newrow.cmcpcod and
025963151104                  prod.cmcprno =  :next_prno  and
025964151104                  newrow.cmcprno = :current_prno
025965151104
025966151104
025967151104               when MATCHED  and :replace_old_data = '1' then
025968151104                  update  set cmctarget =  NEWROW.cmctarget,
025969151104                              cmcuser   = :@user,
025970151109                              cmcdate   = :date#,
025971151109                              cmctime   = :time#
025972151104
025973151104
025974151104               when not MATCHED  and newrow.cmcprno = :current_prno
025975151104                  THEN
025976151104                    insert values(NEWROW.cmcsts, NEWROW.cmctype,
025977151104                                  NEWROW.cmcdept#, NEWROW.cmcpcod,
025978151104                                  NEWROW.cmctarget, NEWROW.cmcperflag,
025979151109                                  :@user, :date# ,
025980151109                                  :time#, :next_prno);
026042151104
026043151104       End-Proc Duplicate_stuff;
026044151104
027800030902
027900030902**  Program Title
028000151104     Duplicate Incentive Targets
