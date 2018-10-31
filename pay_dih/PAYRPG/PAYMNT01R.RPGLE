000100941121     H DATEDIT(*DMY)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000300111002     H/define profoundui
000400111002     Fpayrtypl01if   e           k disk    usropn
000500111002     Fpayrdefl02uf   e           k disk    usropn
000600090415     Fpaycrosl01if   e           k disk    usropn
000700151009     Fpaymnt01d cf   e             workstn
000800151009     F/IF DEFINED(WEB)
000900151009     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001000151009     F/ENDIF
001100151009      //f defined(WEB)
001200151009      // dcl-f paymnt01d workstn;
001300151009      // HANDLER('PROFOUNDUI(HANDLER)');
001400151009      // else
001500151009      // dcl-f paymnt01d workstn;
001600151009      //NDIF
001700151009
001800110901      /copy QCPYSRC,genpr
001900110901      /copy QCPYSRC,glpr
002000110901      /copy QCPYSRC,autpr
002100110901      /copy QCPYSRC,emppr
002200050524     **
002300010815     D                sds
002400950711     D  @PGM             *PROC
002500111002     D  @USER                358    367
002600040705
002700110901     D arr             S             40    dim(1) CTDATA PERRCD(1)
002800050524     D factor          S                   like(ptyfactor)
002900050524     D percen          S                   like(ptypercen)
003000090416     D rtype           S                   like(ptyrtype)
003100050524     D@date            s             10
003200050524     D found_ind       s               n
003300050524     **
003400050524     **
003500111002     Dset_prd_per      pr                  extpgm('PAYMNT0501')
003600040705     D run_type                       1    const
003700040705     D no_Of_Prd                      2  0 const
003800040705     D pay_type                       2  0 const
003900050524     **
004000111002     D pay_penalty     pr                  extpgm('PAYMNT01R1')
004100050524     D ptyptype#                      2  0
004200050524     D ptyapypen                      1
004300050524     D ptydesc                       25
004400050524     **
004500050524     d updclass_rec    pr                  extproc('PAYUPD02R')
004600050524     d                                2  0
004700050524     d                                6  3
004800050524     d                                4  2
004900050524     **
005000050524     d updemp_rec      pr                  extproc('PAYUPD01R')
005100060222     d                                2  0 const
005200060222     d                               10    const
005300060222     d                                1  0 const
005400010716     **
005500021024     D  err1           c                   'Description/short name cannot be bl-
005600021024     D                                     ank'
005700021024     D  err2           c                   'Type cannot be zero'
005800021024     D  err3           c                   'Invalid Payroll run type'
005900021024     D  err4           c                   'Pay factor or pay percent cannot be-
006000021024     D                                      zeros'
006100010716     D  err5           c                   Const('Pay factor &  pay percent can-
006200010716     D                                     not both have values')
006300010824     D  err6           c                   'Cannot base on department if no per-
006400010824     D                                     centage or factor is defined'
006500030209     D  err7           c                   'Deductions Cannot be Taxable'
006600090415     D  err8           c                   'Delete Liability cross ref. before -
006700090415     D                                     changes to payment method can be -
006800090415     D                                     made'
006900090416     D  err9           c                   'Cannot change type from Emolument -
007000090416     D                                     to Deduction and visa versa'
007100050524     d
007200950711     **
007300010301     *******************
007400010301     ** Program start **
007500010301     *******************
007600050524      /free
007700111002
007800111002         // authority check
007900111002         monitor;
008000111002           authority_check('PAY':@pgm);
008100111002         on-error;
008200111002           show_msg(get_err_msg('DIH0054'));
008300111002           *inlr = *on;
008400111002           return;
008500111002         endmon;
008600111002
008700050524                dow 0 = 0;
008800050524                exsr aainit;
008900050524                #mode = *blanks;
009000010301
009100050524          //** First screen to request type
009200050524                   exfmt fmt1;
009300050524                    errmsg = *blanks;
009400050524
009500050524           //  F3 - Program End
009600050524                   if *in03;
009700050524                      *inlr = *on;
009800050524                       return;
009900050524                   endif;
010000050524
010100010419
010200050524          //** Setoff Indicator used to Protect Entry Fields
010300050524                  *in88 = *off;
010400050524
010500050524            // F1 - Pay Types Enquiry
010600050524                   if  *in01;
010700050525                       lst_payty(ptyptype#);
010800050524                       iter;
010900050524                    endif;
011000010419
011100050524          //** Validate type Entry
011200050524                 if ptyptype# = *zeros;
011300050524                     errmsg = err2;
011400050524                     iter;
011500050524                 endif;
011600050524
011700050524           //   If Item Type Found
011800050524                    if not get_payty_info('RD':ptyptype#:payty_struc);
011900050524                        found_ind = *on;
012000050524                        #mode = 'AMEND ';
012100050524                        factor = ptyfactor;
012200050524                        percen = ptypercen;
012300050524                        *in99 = *on;
012400050524
012500050524          //** Protect Entry fields for FIXED Types
012600050524                    if ptysts = 'F';
012700050524                       *in88 = *on;
012800050524                    endif;
012900050524
013000050524                    else;
013100050524             // If Type Not found
013200050524                      found_ind = *off;
013300050524                      #mode = ' ADD ';
013400050524                      clear payty_struc;
013500050524
013600050524                       *in99 = *off;
013700050524                       factor = *zeros;
013800050524                       percen = *zeros;
013900050524          //** Set Default for accrue Flag
014000050524                   endif;
014100010301
014200050524                leave;
014300050524                enddo;
014400050524
014500050524          //***********************
014600050524          //** Body of Program   **
014700050524          //***********************
014800050524                    dow 1 = 1;
014900050524
015000050524                    exfmt fmt2;
015100050524
015200050524                    clear errmsg;
015300050524
015400050524          //   F2 = Cancel
015500050524                  if  *in02;
015600050524                       leave;
015700050524                  endif;
015800050524
015900050524          //** Cycle Code List
016000050524                  if  *in01;
016100050525                    lst_pacycle(ptyclcode);
016200050524                    iter;
016300050524                 endif;
016400050524
016500050524                 errmsg = *blanks;
016600050524
016700050524          //  Validate Description
016800050524                  if ptyshrtnm = *blanks or ptydesc = *blanks;
016900050524                     errmsg = get_err_msg('DIH0022');
017000050524                      iter;
017100050524                  endif;
017200050524
017300050524          //** Validate payroll type link
017400050524                 if ptylink <> *blanks;
017500050524                     chain ptylink prtyfmt;
017600050524                        if not%found;
017700050524                           errmsg = err3;
017800050524                           iter;
017900050524                         endif;
018000010723
018100050524          //** Find all occurrances where said record is selected and
018200050524          //** clear selection field
018300050524
018400050524                 setll ptyptype# prdffmt;
018500050524                 reade ptyptype# prdffmt;
018600050524                    dow  not (%eof);
018700090417                       if  ptylink <> prdfptype;
018800050524                         prdfflag = ' ';
018900050524                         update prdffmt;
019000050524                       endif;
019100050524                      reade  ptyptype# prdffmt;
019200050524                    enddo;
019300010723
019400050524                 endif;
019500040705
019600040705         // Set period payment percentages
019700050524                 if  *in09
019800110901                    and not get_payty_info('RD':ptyptype#:payty_struc)
019900110901                      and ptyclcode = *blanks;
020000050524                    set_prd_per(ptylink:prtyprno:ptyptype#);
020100050524                    iter;
020200050524                 endif;
020300050524
020400050524          //** Validate pay Method dependancies
020500050524                 if ptypaytyp = 'H';
020600050524                    if ptyfactor = *zeros and ptypercen = *zeros;
020700050524                      errmsg = err4;
020800050524                      iter;
020900050524                    endif;
021000050524                 endif;
021100090415
021200050524
021300050524          //** Validate Based on department entry
021400050524                 if ptyfactor = *zeros and ptypercen = *zeros
021500050524                              and ptybasdep = 'Y';
021600050524                      errmsg = err6;
021700050524                      iter;
021800050524                 endif;
021900050524
022000050524          //** Validate pay Method dependancies
022100050524                 if ptyfactor <> *zeros and ptypercen <> *zeros;
022200050524                    errmsg = err5;
022300050524                    iter;
022400050524                 endif;
022500050524
022600050524          //** Validate Tax code wid payment type code
022700050524                 if ptytcode = 'Y' and ptyrtype = 'D';
022800050524                    errmsg = err7;
022900050524                    iter;
023000050524                 endif;
023100090415
023200090415          //** Validate pay Method liability link
023300090415                 if ptypaytyp <> 'S'
023400090415                     and ptyrtype = 'E';
023500090415
023600090415                   open(e) paycrosl01;
023700090415                   chain ptyptype  pcrsfmt;
023800090415                   if  %found;
023900090415                      errmsg = err8;
024000090415                      iter;
024100090415                    endif;
024200090415                   close(e) paycrosl01;
024300090415
024400090415                 endif;
024500050524
024600050524          //**  Validate Default Gl. Code
024700050524                  get_gl_info(ptydglcod:errmsg:'V');
024800050524                     if errmsg <> *blanks;
024900050524                       iter;
025000050524                     endif;
025100050524
025200050524          //** Handling payroll type Penalties
025300050524                    pay_penalty(ptyptype#:ptyapypen:ptydesc);
025400050524
025500050524          //** Update Transaction
025600050524                   if *in99;
025700050524
025800050525                     get_payty_info('UP':ptyptype#:payty_struc);
025900050524
026000050525                     select;
026100050524                       when ptyfactor <> factor
026200050524                              or ptypercen <> percen;
026300050524
026400050525                      //** class records
026500050524                       updclass_rec(ptyptype:factor:percen);
026600050524
026700050525                      //** update  employee records
026800060222                       updemp_rec(ptyptype:*blanks:0);
026900050524
027000050524                   endsl;
027100050524                   commit;
027200050524
027300050524                   else;
027400050524                   ptysts = 'A';
027500050524                   ptyptype = ptyptype#;
027600050524
027700060328                   get_payty_info('WR':ptyptype#:payty_struc);
027800050524
027900050524                   commit;
028000050524                   endif;
028100050524                   leave;
028200050524
028300050524                   enddo;
028400111002
028500111002         //*****************************
028600111002         // Session Init
028700111002         //*****************************
028800111002
028900111002          begsr *inzsr;
029000111006            get_session_info('PAY':errmsg:@pgm);
029100111002            if errmsg <> *blanks;
029200111002              show_msg(errmsg);
029300111002              *inlr = *on;
029400111002              return;
029500111002            endif;
029600111002
029700111002            open(e) payrtypl01;
029800111002            open(e) payrdefl02;
029900111002
030000111002          endsr;
030100111002
030200050524          //***************************************************
030300050524          //** First initilisation routine for all programs  **
030400050524          //***************************************************
030500050524               begsr aainit;
030600050524                  coname = get_coname;
030700050524                   @date = get_sys_date;
030800050524                   @title = arr(1);
030900050524
031000050524               endsr;
031100050524      /end-free
031200010301     **
031300010301**  Program Title
031400011011      Payment Type Maintenance
