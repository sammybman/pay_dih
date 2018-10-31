000100170220     H datedit(*dmy) alwnull(*usrctl)
000200960426
000300090415      //*****  PROGRAMMER - SAMUEL BRITTON    *******
000400090415      //*****  Create payroll file from Kronos data   ******
000500960426
000600090415      // This program uses a file created by the conversion utility
000700090415      // of Kronos TKC version 8d to produce the payroll file in the
000800090415      // format needed by Banks DiH Limited.
000900090415      // NOTE: Upgrades to newer versions of the Kronos Software might
001000090415      //       necessitate changes to this program.
001100960506
001200090415      // Kronos payroll data file
001300010214     Ftimef     if   f  850        disk
001400090415      // Employees' Master file (Regular & Executive staff)
001500010621     Fempmast   if   e           k disk
001600090415      // Department Master file
001700010901     Fdeptl05   if   e           k disk
001800010820     Fpayrtypl01if   e           k disk
001900090415      // Kronos Employee Master file
002000960522     Fkempm     if   e           k disk
002100090415      // Employee Type Master file
002200010619     Femptypel02if   e           k disk
002300100729      // Payroll run time defaults
002400100729     Fpayrdefl03if   e           K DISK
002500090415      // Payroll Time-card file
002600010902     Ftimecrdf  uf a e           k disk    commit
002700090415      // Payroll PROTOTYPES
002800110531      /copy *libl/qcpysrc,PAYPR
002900090415      //
003000010820     Dempn             s             10
003100010820     Dkempn            s             10  0
003200010820     Dsposn            s              5  0
003300010820     Depos             s              5  0
003400010820     Dkfndate          s             10
003500010820     Dnumdays          s              5  0
003600010820     Dhclass           s                   like(class)
003700090415      //
003800090415      // Initialize array index to start from the first "hours" position
003900010820     Dxcnt             s              3  0 inz(6)
004000010820     Dycnt             s              3  0 inz(7)
004100010820     Dfld_6pos         s              6
004200010820     Dfld_5pos         s              5
004300010820     Dfld_4pos         s              4
004400010820     Dx                s              1  0 inz(2)
004500090415      //
004600010820     Dfld_length       s              4  0
004700061018     D@msgid           s              7    inz('PAY0042')
004800061018     Daryrec           s              1    DIM(850) inz
004900061018     Daryscn           s              3  0 DIM(850) inz
005000010820     Dnrhrs            s              5  2
005100010820     Dothrs            s              5  2
005200010820     Dhours            s                   like(othrs)
005300090415      //
005400010820     Dfn_date_fld      s               d   datfmt(*iso)
005500980710     Dkr_date_fld      s               d   datfmt(*usa)
005600090415      //
005700960427     D                sds
005800960426     D  @user                254    263
005900960426     D  @pgm             *proc
006000100707
006100100707         // Escape Program   Prototype
006200100707     D  escape_pgm     pr                  extpgm('ESCPMSG')
006300100707     D                               10    const
006400100707     D                                7    const
006500100707
006600010820     D                 ds
006700970715     Dfldhrs                   1      6
006800960427     Dfld1w                    1      3  0
006900960427     Dfld1d                    5      6  2
007000010816     Dtrn_type         s              2  0
007100010816     Drun_type         s              1
007200090415      //
007300960427     Itimef     aa
007400010214     I                                  1  850  trec
007500010214     I                                  1  850  aryrec
007600090415      //
007700960427
007800980209     C     *entry        plist
007900010816     C                   parm                    run_type
008000010816
008100010816     C     trn_key       klist
008200010816     C                   kfld                    run_type
008300010816     C                   kfld                    trn_type
008400010816     C                   kfld                    emp
008500090415      //
008600090415      // Get fortnight from Kronos file for verification
008700090415      /FREE
008800090415       read timef;
008900090415       *IN10 = %EOF;
009000090415       if *in10;
009100090415         *inlr = *on;
009200090415         return;
009300090415       endif;
009400090415       //
009500090415       // Get current fortnight's selection start date
009600090415       chain run_type prtyfmt;
009700090415       kfndate = %subst(trec:2:10);
009800090415       //
009900090415      /END-FREE
010000020227     C                   move      prtyndate     fn_date_fld
010100980710     C                   move      kfndate       kr_date_fld
010200010820     C     fn_date_fld   subdur    kr_date_fld   numdays:*d
010300090415      //
010400090415      /FREE
010500100707       //if numdays > 2 and run_type = 'R';
010600100707       //  *inlr = *on;
010700100707       //  escape_pgm(@pgm:'PAY0048');
010800100707       //endif;
010900090415
011000090415       //
011100090415       // Skip Header Records  (4 records)
011200090415      /END-FREE
011300980710     C                   do        4
011400090415      /FREE
011500090415         read timef;
011600090415         *IN10 = %EOF;
011700090415       enddo;
011800960426
011900090415       read timef;
012000090415       *INlr = %EOF;
012100090415       dow *inlr = *off;
012200090415         //  Get employee number from file
012300090415         clear aryscn;
012400090415      /END-FREE
012500960427     C     '"'           scan      trec          aryscn
012600960427     C     aryscn(3)     add       1             epos
012700090415      /FREE
012800090415         empn = %subst(trec:epos:10);
012900960427
013000090415         //  Skip some records (4 records)
013100090415      /END-FREE
013200960426     C                   do        4
013300090415      /FREE
013400090415           read timef;
013500090415           *IN10 = %EOF;
013600090415         enddo;
013700960427
013800090415         //  Read record to get Employees' hours
013900090415      /END-FREE
014000960426     C                   do
014100090415      /FREE
014200090415           read timef;
014300090415           *IN10 = %EOF;
014400090415      /END-FREE
014500960427     C     redo          tag
014600090415      /FREE
014700090415           clear aryscn;
014800090415      /END-FREE
014900960427     C     ','           scan      trec          aryscn
015000090415      // Skip calculations if there's no summary record
015100090415      /FREE
015200090415           if aryscn(6) = *zeros;
015300090415      /END-FREE
015400960522     C                   goto      continue
015500090415      /FREE
015600090415           endif;
015700960427
015800090415           exsr cal_hours;
015900100707
016000090415         enddo;
016100960427
016200090415         // Skip one record if no more employee records
016300090415         read timef;
016400090415         *IN10 = %EOF;
016500090415      /END-FREE
016600960522     C     continue      tag
016700960427     C     '/'           scan      trec          epos
016800090415      // Calculations will continue if the employee has worked in
016900090415      // more than one Labour Accounts (Departments)
017000960427     C     epos          cabne     4             redo
017100090415      //
017200960427     C                   move      empn          emp
017300960522     C                   move      empn          kempn
017400090415      /FREE
017500090415         chain emp emprec;
017600090415         *IN77 = NOT %FOUND;
017700090415         chain dept dhrec;
017800090415         *IN55 = NOT %FOUND;
017900090415         chain status empfmt;
018000090415         //
018100090415         //
018200090415         // Process record if the employee is an active employee and is
018300090415         // not an executive .
018400960506
018500090415         if not(*in77) and emppyopt = 'H';
018600100707
018700100707            // Terminate program if data from Time system not
018800100707            // from correct Period
018900100707            if numdays > 2;
019000100707
019100100707              if nrhrs > 0 or othrs > 0;
019200100729                 chain (run_type:1) prdffmt;
019300100729                if  not %found;
019400100729                  chain (run_type:2) prdffmt;
019500100729                  if  %found;
019600100729                    // terminate program
019700100729                   *inlr = *on;
019800100729                   escape_pgm(@pgm:'PAY0048');
019900100729                  endif;
020000100915                 else;
020100100915                   *inlr = *on;
020200100915                   escape_pgm(@pgm:'PAY0048');
020300100729                endif;
020400100707              endif;
020500100707
020600100707            endif;
020700100707
020800090415           //-------------------------------------------------------------------*-
020900090415           // Add 16 hours to engine room shift staff - (32 hour work week)
021000090415           if dept = 49  and nrhrs > 0;
021100090415             chain kempn kempm;
021200090415             *IN66 = NOT %FOUND;
021300090415           endif;
021400090415           //-------------------------------------------------------------------*-
021500960702
021600090415           //-------------------------------------------------------------------*-
021700090415           // Add 3.5 hours to 81.5 Hrs staff
021800090415           if dhours = 81.5 and nrhrs > 0;
021900090415             //
022000090415             if nrhrs > 81.5;
022100090415      /END-FREE
022200960819     C                   z-add     81.5          nrhrs
022300090415      /FREE
022400090415             endif;
022500090415           endif;
022600090415           //-------------------------------------------------------------------*-
022700960702
022800010627
022900090415           //  Change clerical emp. to 20 hrs max. ot. all others are on payroll
023000090415           //                  if        dtype = 'A'
023100090415           //    'P':' '       xlate     class         hclass
023200090415           //
023300090415           //  NO LONGER VALID - UNION AGREEMENT OCT' 2008
023400090415           //                  if        hclass <> '  5' and nrhrs <> *zeros
023500090415           //                  z-add     10            othrs
023600090415           //                  endif
023700090415           //                  endif
023800090415           //-------------------------------------------------------------------*-
023900090415           // Write record if regular or overtime hours is greater than zero
024000090415           // Process Normal time hours
024100090415           if nrhrs > 0;
024200090415             trn_type = 1;
024300090415             hours = nrhrs;
024400090415             exsr write_trn;
024500090415           endif;
024600010816
024700090415           // Process Overtime hours
024800090415           if othrs > 0;
024900090415             trn_type = 2;
025000081107
025100090415             //  NO LONGER VALID - UNION AGREEMENT OCT' 2008
025200090415             // Give Grade's 5 employees 60% of their overtime
025300090415             //                  if        class = '  5' or class = ' P5'
025400090415             //                  eval      othrs = othrs * 0.6
025500090415             //                  endif
025600090415             //
025700090415             hours = othrs;
025800090415             exsr write_trn;
025900090415             //
026000090415           endif;
026100090415           //
026200090415         endif;
026300090415         nrhrs = *zeros;
026400090415         othrs = *zeros;
026500090415         clear timcfmt;
026600090415         //
026700090415         read timef;
026800090415         *INlr = %EOF;
026900090415       enddo;
027000090415      /END-FREE
027100010626     Clr   *loval        setll     emprec
027200960427
027300090415      //*********************************************************************
027400090415      //  Routine to calculate employee hours **
027500090415      //*********************************************************************
027600960427
027700090415      // NOTE: To include a new pay category , add 1 to the limit value of
027800090415      //*      the index field (xcnt) as specified in the following 'DO' loop
027900090415      //*      Change all references to this value in the program and include
028000090415      //*      a calculation block of code for this new category in the
028100090415      //*      'SELECT' routine.
028200090415      /FREE
028300090415       begsr cal_hours;
028400090415         dow xcnt <= 130;
028500090415      /END-FREE
028600970715     C                   add       1             aryscn(xcnt)
028700090415      //
028800090415      /FREE
028900090415           if xcnt <> 130;
029000090415      /END-FREE
029100970715     C     aryscn(ycnt)  sub       aryscn(xcnt)  fld_length
029200090415      /FREE
029300090415           else;
029400090415             //  LAST PAY CATEGORY
029500090415             // For last pay category - check for last non-blank character.
029600090415      /END-FREE
029700970715     C     ' '           checkr    trec          result_fld        3 0
029800970715     C                   add       1             result_fld
029900970715     C     result_fld    sub       aryscn(xcnt)  fld_length
030000090415      /FREE
030100090415           endif;
030200090415           // --------------------------------------------------**
030300090415           // For last category check for new additions to file **
030400090415           // --------------------------------------------------**
030500090415           IF fld_length > 6;
030600090415      /END-FREE
030700970715     C                   call      'ESCPMSG'
030800970715     C                   parm                    @pgm
030900970715     C                   parm                    @msgid
031000090415      /FREE
031100090415           endif;
031200090415           //
031300090415      /END-FREE
031400970715     C                   move      '000.00'      fldhrs
031500970715     C                   z-add     aryscn(xcnt)  sposn
031600090415      /FREE
031700090415           IF fld_length = 6;
031800090415      /END-FREE
031900970715     c                   movea     aryrec(sposn) fld_6pos
032000970715     C                   move      fld_6pos      fldhrs
032100090415      /FREE
032200090415           else;
032300090415             IF fld_length = 5;
032400090415      /END-FREE
032500970715     C                   movea     aryrec(sposn) fld_5pos
032600970715     C                   move      fld_5pos      fldhrs
032700090415      /FREE
032800090415             else;
032900090415               IF fld_length = 4;
033000090415      /END-FREE
033100970715     C                   movea     aryrec(sposn) fld_4pos
033200970715     C                   move      fld_4pos      fldhrs
033300090415      /FREE
033400090415               endif;
033500090415             endif;
033600090415           endif;
033700090415           //
033800090415           select;
033900090415             // Regular Hours
034000090415           when xcnt = 6;
034100090415      /END-FREE
034200970715     C                   add       fld1w         nrhrs
034300970715     C                   add       fld1d         nrhrs
034400090415      //
034500090415      // Overtime Hours
034600090415      /FREE
034700090415           when xcnt = 7;
034800090415      /END-FREE
034900970715     C                   add       fld1w         othrs
035000970715     C                   add       fld1d         othrs
035100090415      //
035200090415      // Overtime Hours (1.5) - Sundays etc.
035300090415      /FREE
035400090415           when xcnt = 8;
035500090415             othrs = othrs + ((fld1w + fld1d) * 1.5);
035600090415             //
035700090415             // Overtime Hours (2.0)  - Holidays etc.
035800090415           when xcnt = 9;
035900090415             othrs = othrs + ((fld1w + fld1d) * 2.0);
036000090415             //
036100090415             // Afternoon Shift Hours  - Engine Room, Warehouse etc.
036200090415           when xcnt = 10;
036300090415             nrhrs = nrhrs + ((fld1w + fld1d) * 0.08);
036400090415             //
036500090415             // Night Shift Hours      - Engine Room, Well Operators etc.
036600090415           when xcnt = 11;
036700090415             nrhrs = nrhrs + ((fld1w + fld1d) * 0.15);
036800090415             //
036900090415             // Holiday Hours  - Everyone except Security Personnel
037000090415           when xcnt = 12;
037100090415             nrhrs = nrhrs + fld1w + fld1d;
037200090415             //
037300090415             // Holiday Overtime (2.00) - Christmas , etc.
037400090415           when xcnt = 13;
037500090415             othrs = othrs + ((fld1w + fld1d) * 2.00);
037600090415             //
037700090415             // Overtime Hours (1.25)  - Over 8hrs on saturday
037800090415           when xcnt = 21;
037900090415             othrs = othrs + ((fld1w + fld1d) * 1.25);
038000090415             //
038100090415             // Overtime Hours (1.75)  - Over 12hrs on Sat. ,Salesmen on Sun. ,Hol.
038200090415           when xcnt = 22;
038300090415             othrs = othrs + ((fld1w + fld1d) * 1.75);
038400090415             // Holiday  Overtime 3    - not used
038500090415           when xcnt = 23;
038600090415             othrs = othrs + ((fld1w + fld1d) * 1.00);
038700090415             // Afternoon Shift Hours  - Bakery
038800090415           when xcnt = 24;
038900090415             nrhrs = nrhrs + ((fld1w + fld1d) * 0.03);
039000090415             // Holiday Overtime (1.75) - Diwali,etc
039100090415           when xcnt = 25;
039200090415             othrs = othrs + ((fld1w + fld1d) * 1.75);
039300090415           endsl;
039400090415           // Increment array index count
039500090415      /END-FREE
039600970715     C                   add       1             xcnt
039700970715     C                   add       1             ycnt
039800090415      /FREE
039900090415         enddo;
040000960506
040100090415         // For next employee or new labour account, reset indices for **
040200090415         // Hours record calculation.                                  **
040300090415         reset xcnt;
040400090415         reset ycnt;
040500960506
040600090415       endsr;
040700090415       //*****************************************
040800090415       // Write hours to time CARD file
040900090415       //*****************************************
041000090415       begsr write_trn;
041100090415         chain trn_key timcfmt;
041200090415         if %found;
041300090415           timc_hrs = timc_hrs + hours;
041400090415           update timcfmt;
041500090415         else;
041600090415           timc_type = trn_type;
041700090415           timc_ptyp = run_type;
041800090415           timc_hrs = hours;
041900090415           timc_emp = emp;
042000090415           write timcfmt;
042100090415         endif;
042200090415       endsr;
042300090415      /END-FREE
