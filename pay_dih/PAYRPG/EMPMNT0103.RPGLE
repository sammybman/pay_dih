000100951017     H DATEDIT(*YMD)
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'CCREDITDIR')
000300111013     H/define profoundui
000400050511     F**Created by - allison fung
000500050511     F**Date       - 11.05.2005
000600050527     Fempsmf    if   e           k disk
000700140827     Fempmnt01d3CF   e             Workstn
000900111013     F/IF DEFINED(WEB)
001000111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001100111013     F/ENDIF
001200020206     **
001300111013      /copy QCPYSRC,genpr
001400111013      /copy QCPYSRC,payPR
001500111013      /copy QCPYSRC,autpr
001600111013      /copy QCPYSRC,emppr
001700111013      /copy QCPYSRC,salpr
001800140827
001900980213     d  arr            s             40    dim(2) ctdata perrcd(1)
002200050513     D found_ind       s               n
002301140827     D err_code        s              7
002302140827     D emp_name        s             40
002400020215     **
002500050529     D msg1            c                   'Must enter Amount or Percentage'
002600050512     D msg2            c                   'Error found during update'
002601140827     D msg3            c                   'Must enter Employee number'
002602140827     D msg4            c                   'Invalid Employee No. Entered'
002700020206     **
002800941215     D                SDS
002900941215     D  @PGM             *PROC
003000111013     D  @USER                358    367
003100990322     **
003200140827     D main_pgm        PR                  extpgm('EMPMNT0103')
003300140827     D                                2  0
003400140827     D                                3  0
003500140827     D                                3  0
003600140827
003700050513     D main_pgm        PI
003800140827     D lrultype#                      2  0
003900140827     D lrulstype#                     3  0
004000140827     D lrulloc#                       3  0
004100140827
004200050512
004201140827     Dlst_employee     PR                  extpgm('HMRENQ09R')
004202140827     D                                6  0
004203140827
004300050513
004400050513     D*--------------------------------------
004500140827     D* Prototype for procedure: confirm_empxc
004600050513     D*--------------------------------------
004700140827     D confirm_empxc   PR
004800050513     D*errmsg                        75A
004900050513
005100140827
005101140827            if lrulloc# <> *zeros;
005102140827              get_whcon_info('RD':lrulloc#:whcon_struc);
005103140827            endif;
005104140827
005105140827            get_ltype_info('RD':lrultype#:ematf_struc);
005106140827
005107140827            if lrulstype# <> *zeros;
005108140827              get_lstype_info('RD':lrultype#:lrulstype#:eastf_struc);
005109140827            endif;
005200990322
005300050527            dow 0 = 0;
005400140827                  exsr aainit;
005500140827                  exfmt fmt1;
005600050514
005601140827               // F2 = Go Back
005602140827                  if  *in02;
005603140827                     *in99 = *off;
005604140827                     iter;
005605140827                  endif;
005606140827
005700140827               // F3 = End Program
005800140827                  if  *in03;
005900140827                     *inlr = *on;
006000140827                     return;
006100140827                  endif;
006200050517
006300140827               // F4 =Prompt
006400050514                  if *in04;
006500050514                    select;
006600140827                     // employee enquiry
006700140827                      when fld = 'EMPNO';
006800140827                       lst_employee(empno);
006900050514                      other;
007000050514                    endsl;
007100050514                     iter;
007200050514                   endif;
007300050513
007301140827                  if  empno = *zeros;
007302140827                     errmsg = msg3;
007303140827                     iter;
007304140827                  endif;
007305140827
007306140827                  if not *in99;
007307140827                     get_emp_info(empno:*omit:*blank:
007308140827                           emp_name:class:*omit:*omit:err_code);
007309140827                    if  err_code <> ' ';
007310140827                        errmsg = msg4;
007311140827                        iter;
007312140827                       else;
007313140827                        name = emp_name;
007314140827                        get_empxc_info('RD':lrultype#:lrulstype#:
007315140827                                     lrulloc#:empno:empxc_struc);
007316140827                        *in99 = *on;
007317140827                        iter;
007318140827                    endif;
007319140827
007320140827                    else;
007321140827
007322140827                    //get_empxc_info('RD':lrultype#:lrulstype#:
007323140827                    //                 lrulloc#:empno:empxc_struc);
007324140827                    //set_emp_limit(errmsg);
007325140827
007326140827                      //  class = %xlate('P':' ':class);
007327140827                      // F15= Confirm
007328140827                      if  *in15;
007329140827                        confirm_empxc();
007331140827                        *in99 = *off;
007332140827                        empno = *zeros;
007333140827                        name = *blanks;
007334140827                        lrul4allwl = *zeros;
007335140827                        lrul4iglim = *blanks;
007336140827                        iter;
007337140827                      endif;
007338140827                  endif;
007339140827
008700050519
008900050517
009000050527            enddo;
009001140827
009100050512         //***************************************************
009200050512         //** First initilisation routine for all programs  **
009300050512         //***************************************************
009400050512                 begsr aainit;
009500050512                  CONAME = get_coname;
009600050512                   @DATE = get_sys_date;
009700050512                     @title = arr(1);
009800050512                 endsr;
009900050512
010000050512      /end-free
010100050512
010200050513     P*--------------------------------------
010300140827     P* Procedure name: set_emp_limit
010400140827     P* Purpose:        setup Employee Limit
010500050513     P* Returns:        error indicator
010600050513     P* Parameter:      errmsg => error message text
010700050513     P*--------------------------------------
010800140827     P set_emp_limit   B
010900140827     D set_emp_limit   PI
011000140827     D errmsg                        75A
011100050527     D found_ind       s               n
011200050513
011300140827
012400140827           //   found_ind = *off;
015700050514
015900140827     P set_emp_limit   E
024300050527
027700050527
031000050527
031100050517     P*--------------------------------------
031200140827     P* Procedure name: confirm_empxc
031300140827     P* Purpose:        confirm changes to emp. excpt
031400050517     P* Returns:        error indicator
031500050517     P* Parameter:      errmsg => error message text
031600050517     P*--------------------------------------
031700140827     P confirm_empxc   B
031800140827     D confirm_empxc   PI
031900050517     D*errmsg                        75A
032100050527     d y               s              2  0
032200140827     d empxc_struc#    ds                  likeds(empxc_struc)
032300050517
032400050517
032500050527
032600050527              *in99 = *on;
032700050527
032800050527
032900140827                 if get_empxc_info('RD':lrultype#:lrulstype#:
033000140827                                    lrulloc#:empno:empxc_struc#);
033100140827                    if lrul4allwl <> *zeros
033200140827                                  or lrul4iglim = 'Y';
033300050527
033400050527                   // if a new record
033500140827                       lrul4sts = 'A';
033600140827                       lrul4loc = lrulloc#;
033700140827                       lrul4type = lrultype#;
033800140827                       lrul4stype = lrulstype#;
033900140827                       lrul4emp   = empno;
034000140827                       lrul4class = esmclass;
034100140827                       get_empxc_info('WR':lrultype#:lrulstype#:lrulloc#:
034200140827                                    empno:empxc_struc);
034300050527                  endif;
034400050527
034500050527                else;
034600050527
034700050527               select;
034800140827                  when lrul4allwl = 0 and lrul4iglim <> 'Y';
034900140827                    get_empxc_info('DL':lrultype#:lrulstype#:lrulloc#:
035000140827                                   empno);
035100050527                  other;
035200140827                     lrul4sts = 'A';
035300140827                     lrul4loc = lrulloc#;
035400140827                     lrul4type = lrultype#;
035500140827                     lrul4stype = lrulstype#;
035600140827                     lrul4emp   = empno;
035700140827                    get_empxc_info('UP':lrultype#:lrulstype#:lrulloc#:
035800140827                                   empno:empxc_struc);
035900050527               endsl;
036000050527
036100050527                endif;
036200050517
036300050527         // return  *off;
036400050517      /end-free
036500050517
036600140827     P confirm_empxc   E
036700980213**
036800140827   EMPLOYEE EXCEPTION FOR CREDIT LIMITS
036900980213XXXXXXXXXX0123456789XXXXXXXXXX0123456789
