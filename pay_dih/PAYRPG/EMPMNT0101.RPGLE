000100951017     H DATEDIT(*YMD)
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'CCREDITDIR')
000300111013     H/define profoundui
000400050511     F**Created by - allison fung
000500050511     F**Date       - 11.05.2005
000600050527     Fempsmf    if   e           k disk
000700050530     Fempmnt01d1CF   e             Workstn
000800050513     F                                     sfile(sfile:rrn)
000900111013     F/IF DEFINED(WEB)
001000111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001100111013     F/ENDIF
001200020206     **
001300111013      /copy QCPYSRC,genpr
001400111013      /copy QCPYSRC,glpr
001500111013      /copy QCPYSRC,payPR
001600111013      /copy QCPYSRC,autpr
001700111013      /copy QCPYSRC,emppr
001800111013      /copy QCPYSRC,salpr
001900021004     **
002000980213     d  arr            s             40    dim(2) ctdata perrcd(1)
002100050511     D*run_list        s              1    dim(2)
002200020215     D cnt             s              2  0
002300050513     d rrn             s              4  0
002400020215     D error           s               n
002500050513     D found_ind       s               n
002600050527     D counter         s              5  0
002700020215     **
002800050529     D msg1            c                   'Must enter Amount or Percentage'
002900050512     D msg2            c                   'Error found during update'
003000020206     **
003100941215     D                SDS
003200941215     D  @PGM             *PROC
003300111013     D  @USER                358    367
003400990322     **
003500111013     D main_pgm        PR                  extpgm('EMPMNT0101')
003600050513     D                                2  0 const
003700050519     D                                1    const options(*nopass)
003800050513     D main_pgm        PI
003900050517     D emseltype#                     2  0 const
004000050519     D emsubtyp#                      1    const options(*nopass)
004100050513     **
004200050527     D*-----------------------------------------
004300050527     D* Prototype for procedure: ClearSubfile
004400050527     D*-----------------------------------------
004500050527     D ClearSubfile    PR
004600050527     D
004700050527     D*-----------------------------------------
004800050527     D* Prototype for procedure: LoadSubfile
004900050527     D*-----------------------------------------
005000050527     D LoadSubfile     PR
005100050527     D
005200050527     D*-----------------------------------------
005300050527     D* Prototype for procedure: DisplaySubfile
005400050527     D*-----------------------------------------
005500050527     D DisplaySubfile  PR
005600050527     D
005700050512     D*--------------------------------------
005800050513     D* Prototype for procedure: confirm_cls
005900050512     D*--------------------------------------
006000050513     D confirm_cls     PR              N
006100050513     D*errmsg                        75A
006200050512
006300050512     D*--------------------------------------
006400050512     D* Prototype for procedure: set_cls_limit
006500050512     D*--------------------------------------
006600050517     D set_cls_limit   PR
006700050513     D*errmsg                        75A
006800050512
006900050513     D*--------------------------------------
007000050513     D* Prototype for procedure: enforce_limit
007100050513     D*--------------------------------------
007200050527     D enforce_limit   PR
007300050513     D*errmsg                        75A
007400050513
007500050513     D*--------------------------------------
007600050513     D* Prototype for procedure: confirm_enfor
007700050513     D*--------------------------------------
007800050527     D confirm_enfor   PR
007900050513     D*errmsg                        75A
008000140827     **
008100140827     D*--------------------------------------
008200140827     D* Prototype for procedure: emp_exception
008300140827     D*--------------------------------------
008400140827     D emp_exception   PR                  extpgm('EMPMNT0103')
008500140827     D                                2  0 const
008600140827     D                                3  0 const
008700140827     D                                3  0 const
008800050513
008900050511     **
009000040207      /free
009100990322
009200050527            dow 0 = 0;
009300050512                      exsr aainit;
009400050514                      exfmt fmt0;
009500050514
009600050517           // F3 = End Program
009700050517                      if  *in03;
009800050517                         *inlr = *on;
009900050517                         return;
010000050517                      endif;
010100050517
010200050514            // F4 =Prompt
010300050514                  if *in04;
010400050514                    select;
010500050514             // warehouse code enquiry
010600050514                      when fld = 'LRULLOC#';
010700050514                       lst_whcon(lrulloc#);
010800050514                      other;
010900050514                    endsl;
011000050514                     iter;
011100050514                   endif;
011200050513
011300050514           // Validate Location if not blank
011400050514                     if lrulloc# <> *zeros;
011500050514                         if get_whcon_info('RD':lrulloc#:whcon_struc);
011600050514                           errmsg = get_err_msg('DIH0002');
011700050527                           iter;
011800050514                         endif;
011900050514                     endif;
012000050517
012100050519            // Display Sub-Type, if any, for selection
012200050519                     if emsubtyp# = 'Y';
012300050527                       lst_lstype(lrulstype#);
012400050527                         if lrulstype# = *zeros;
012500050527                         iter;
012600050527                         endif;
012700050519                     endif;
012800050519
012900050517                    set_cls_limit();
013000050517
013100050527            enddo;
013200050512         //***************************************************
013300050512         //** First initilisation routine for all programs  **
013400050512         //***************************************************
013500050512                 begsr aainit;
013600050512                  CONAME = get_coname;
013700050512                   @DATE = get_sys_date;
013800050512                     @title = arr(1);
013900050512                 endsr;
014000050512
014100050512      /end-free
014200050512
014300050513     P*--------------------------------------
014400050513     P* Procedure name: set_cls_limit
014500050513     P* Purpose:        setup class limit
014600050513     P* Returns:        error indicator
014700050513     P* Parameter:      errmsg => error message text
014800050513     P*--------------------------------------
014900050513     P set_cls_limit   B
015000050517     D set_cls_limit   PI
015100050513     D*errmsg                        75A
015200050527     D found_ind       s               n
015300050513
015400050513     C* Your calculation code goes here
015500050513      /free
015600050527         dow 0 = 0;
015700050527              lrultype# = emseltype#;
015800050519
015900050517              get_whcon_info('RD':lrulloc#:whcon_struc);
016000050527              get_ltype_info('RD':emseltype#:ematf_struc);
016100050527              get_lstype_info('RD':lrultype#:lrulstype#:eastf_struc);
016200050513
016300050527               if get_cllim_info('RD':lrultype#:lrulstype#:lrulloc#:
016400050527                                             class_struc);
016500050527                found_ind = *off;
016600050527                 else;
016700050527                found_ind = *on;
016800050527               endif;
016900050527
017000050519                      exfmt fmt1;
017100050529                    errmsg = *blanks;
017200050519
017300140827           // F2 = Go Back
017400140827                      if  *in02;
017600050513                         return;
017700050513                      endif;
017701140827
017702140827           // F3 = End Program
017703140827                      if  *in03;
017704140827                         *inlr = *on;
017705140827                         return;
017706140827                      endif;
017800050513
017900050529                  if  lrulalllim = *zeros and
018000050529                     lrulperlim = *zeros;
018100050529                      errmsg = msg1;
018200050529                      iter;
018300050529                  endif;
018400050529
018500050527                  if  lrulclslim = 'Y';
018600050527                      enforce_limit();
018700050527                      return;
018800050527                  endif;
018900050527
019600140827
019700050514           // F15= Confirm
019800050514              // check if to SETUP CLASS LIMIT  before confirming
019900050527                   if  *in15;
020000050514                        confirm_cls();
020100050514                        return;
020200050514                   endif;
020300050527
020400050527         enddo;
020500050514
020600050513      /end-free
020700050513     P set_cls_limit   E
020800050527
020900050512     P*--------------------------------------
021000050513     P* Procedure name: confirm_cls
021100050513     P* Purpose:        confirm class limit setup
021200050512     P* Returns:        error indicator
021300050512     P* Parameter:      errmsg => error message text
021400050512     P*--------------------------------------
021500050513     P confirm_cls     B
021600050513     D confirm_cls     PI              N
021700050513     D*errmsg                        75A
021800050512
021900050512     D* Local fields
022000050512
022100050512     C* Your calculation code goes here
022200050512      /free
022300050512
022400050514                     lrulloc = lrulloc#;
022500050514                     lrultype = lrultype#;
022600050514                     lrulstype = lrulstype#;
022700050512
022800050512         // if record was changed
022900050512                      if found_ind;
023000050514                      if get_cllim_info('UP':lrultype#:lrulstype#:lrulloc#:
023100050527                                             class_struc);
023200050512                          errmsg = msg2;
023300050512                          return  *off;
023400050512                       endif;
023500050512         // if a new record
023600050512                          else;
023700050512                          lrulsts = 'A';
023800050512                          lrultype = lrultype#;
023900050514                          lrulloc = lrulloc#;
024000050514                          lrulstype = lrulstype#;
024100050514                          get_cllim_info('WR':lrultype#:lrulstype#:lrulloc#:
024200050527                                             class_struc);
024300050512                       endif;
024400050512
024500050512            return  *off;
024600050512      /end-free
024700050513     P confirm_cls     E
024800050527
024900050517     P*--------------------------------------
025000050517     P* Procedure name: enforce_limit
025100050517     P* Purpose:        load classes to enforce limits
025200050517     P* Returns:        error indicator
025300050517     P* Parameter:      errmsg => error message text
025400050517     P*--------------------------------------
025500050517     P enforce_limit   B
025600050527     D enforce_limit   PI
025700050517     D*errmsg                        75A
025800050517
025900050517     C* Your calculation code goes here
026000050517      /free
026100050517
026200050527            ClearSubfile();
026300050527
026400050527            LoadSubfile();
026500050527
026600050527            DisplaySubfile();
026700050527
026800050527      /end-free
026900050527     P enforce_limit   E
027000050527
027100050527     P*--------------------------------------
027200050527     P* Procedure name: ClearSubfile
027300050527     P* Purpose:  Clear subfile records
027400050527     P* Returns:        *none
027500050527     P* Parameter:      *none
027600050527     P*--------------------------------------
027700050527     P ClearSubfile    B
027800050527     D ClearSubfile    PI
027900050527      /free
028000050527
028100050527                *in33 = *on;
028200050527                 write footer;
028300050527                 write sfctl;
028400050527                 rrn = 0;
028500050527                 *in33 = *off;
028600050517
028700050527                 counter = *zeros;
028800050527
028900050527      /end-free
029000050527     P ClearSubfile    E
029100050527
029200050527     P*--------------------------------------
029300050527     P* Procedure name: LoadSubfile
029400050527     P* Purpose:  Load subfile records
029500050527     P* Returns:        *none
029600050527     P* Parameter:      *none
029700050527     P*--------------------------------------
029800050527     P LoadSubfile     B
029900050527     D LoadSubfile     PI
030000050527      /free
030100050527
030200050527               setll *loval esmfmt;
030300050527               read esmfmt;
030400050527               dow not(%eof);
030500050527
030600050517           // Get transaction records
030700050527              if get_enfor_info('RD':lrultype#:lrulstype#:
030800050527                                    lrulloc#:esmclass:enfor_struc);
030900050527                 clear enfor_struc;
031000050527              endif;
031100050527
031200050517
031300050517          // write to subfile
031400050517                     rrn += 1;
031500050517                      write sfile;
031600050517
031700050527                   read esmfmt;
031800050527               enddo;
031900050517
032000050527                  counter = rrn;
032100050517                  *in55 = rrn > 0;
032200050517
032300050527      /end-free
032400050527     P LoadSubfile     E
032500050527
032600050527     P*--------------------------------------
032700050527     P* Procedure name: DisplaySubfile
032800050527     P* Purpose:  Display subfile records
032900050527     P* Returns:        *none
033000050527     P* Parameter:      *none
033100050527     P*--------------------------------------
033200050527     P DisplaySubfile  B
033300050527     D DisplaySubfile  PI
033400050527      /free
033500050527            dow 0=0;
033600050527
033700050527            *in75 = rrn > 0;
033701141202      /if not defined(web)
033800050527            write  footer;
033801141202      /endif
033900050527            exfmt  sfctl;
034000050527
034100050527           // F3= End process
034200050527              // check if to SETUP CLASS LIMIT  before confirming
034300050527                   if  *in03;
034400050527                        return;
034500050527                   endif;
034501140827
034502140827           // F05= Employee exception
034503140827              // check if to SETUP CLASS LIMIT  before confirming
034504140827                   if  *in05;
034505140827                        emp_exception(lrultype#:lrulstype#:lrulloc#);
034506140827                        iter;
034507140827                   endif;
034600050527
034700050527           // F15= Confirm
034800050527              // check if to SETUP CLASS LIMIT  before confirming
034900050527                   if  *in15;
035000050527                        confirm_cls();
035100050527                        confirm_enfor();
035200050527                        return;
035300050527                   endif;
035400050527            enddo;
035500050527
035600050527      /end-free
035700050527     P DisplaySubfile  E
035800050527
035900050517     P*--------------------------------------
036000050517     P* Procedure name: confirm_enfor
036100050517     P* Purpose:        confirm changes to enforce limits
036200050517     P* Returns:        error indicator
036300050517     P* Parameter:      errmsg => error message text
036400050517     P*--------------------------------------
036500050517     P confirm_enfor   B
036600050527     D confirm_enfor   PI
036700050517     D*errmsg                        75A
036800050527     D cnt             s              2  0
036900050527     d y               s              2  0
037000050527     d enfor_struc#    ds                  likeds(enfor_struc)
037100050517
037200050517     C* Your calculation code goes here
037300050517      /free
037400050517
037500050527              cnt = rrn;
037600050527
037700050527              *in99 = *on;
037800050527
037900050527              for  y = 1 to cnt;
038000050527
038100050527                chain y sfile;
038200050527
038300050527                 if get_enfor_info('RD':lrultype#:lrulstype#:
038400050527                                    lrulloc#:esmclass:enfor_struc#);
038500050527                    if lrul2allwl <> *zeros  or lrul2prlim <> *zeros
038600050527                                  or lrul2iglim = 'Y';
038700050527
038800050527                   // if a new record
038900050527                       lrul2sts = 'A';
039000050527                       lrul2loc = lrulloc#;
039100050527                       lrul2type = lrultype#;
039200050527                       lrul2stype = lrulstype#;
039300050527                       lrul2class = esmclass;
039400050527                       get_enfor_info('WR':lrultype#:lrulstype#:lrulloc#:
039500050527                                    esmclass:enfor_struc);
039600050527                  endif;
039700050527
039800050527                else;
039900050527
040000050527               select;
040100050527                  when lrul2allwl = 0 and lrul2prlim = 0 and lrul2iglim <> 'Y';
040200050527                    get_enfor_info('DL':lrultype#:lrulstype#:lrulloc#:
040300050527                                   esmclass);
040400050527                  other;
040500050531                     lrul2sts = 'A';
040600050531                     lrul2loc = lrulloc#;
040700050531                     lrul2type = lrultype#;
040800050531                     lrul2stype = lrulstype#;
040900050531                     lrul2class = esmclass;
041000050527                    get_enfor_info('UP':lrultype#:lrulstype#:lrulloc#:
041100050527                                   esmclass:enfor_struc);
041200050527               endsl;
041300050527
041400050527                endif;
041500050527              endfor;
041600050517
041700050527         // return  *off;
041800050517      /end-free
041900050517
042000050517     P confirm_enfor   E
042100980213**
042200050527    SETUP/ENFORCE CLASS CREDIT LIMITS
042300980213XXXXXXXXXX0123456789XXXXXXXXXX0123456789
