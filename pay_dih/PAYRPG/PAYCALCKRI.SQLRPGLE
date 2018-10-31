000100141007     H datedit(*dmy)
000200170220     H
000300960426
000400090415      //*****  PROGRAMMER - SAMUEL BRITTON    *******
000500090415      //*****  Create payroll file from Kronos data   ******
000600960426
000700110307      //--------------------------------------------------------------
000800110303      // This program uses a number of files from the KRONOS Iseries
000900110303      // Central version 6.1 to produce the payroll file in the
001000090415      // format needed by Banks DiH Limited.
001100090415      // NOTE: Upgrades to newer versions of the Kronos Software might
001200090415      //       necessitate changes to this program.
001300110307      //--------------------------------------------------------------
001400960506
001500010820     Fpayrtypl01if   e           k disk
001600090415      // Payroll Time-card file
001700010902     Ftimecrdf  uf a e           k disk    commit
001800110308
001900090415      // Payroll PROTOTYPES
002000110622      /copy QCPYSRC,PAYPR
002100110307
002200010820     Dempn             s             10
002300110518     D cmd             s            200
002400110518     D library         s             10
002500110303
002600090415      // Initialize array index to start from the first "hours" position
002700110308     D xcnt            s              3  0
002800140930     D ycnt            s              3  0
002900140930     D zcnt            s              3  0
003000110308     D pay_opt         s              1
003100110303
003200110311     D prdend          s              8  0
003300110620     D kr_prdend       s             14  0
003400140930     D pc              s                   dim(10) like(tppc01) based(cptr)
003500140930     D ph              s                   dim(10) like(tphr01) based(hptr)
003600140930     D cptr            s               *   inz(%addr(tppc01))
003700140930     D hptr            s               *   inz(%addr(tphr01))
003800110303
003900110620
004000110620     D MIDNIGHT        c                   '235900'
004100110303
004200960427     D                sds
004300960426     D  @user                254    263
004400960426     D  @pgm             *proc
004500140930
004600140930     d employee_hours...
004700141007     d                 ds                   qualified  dim(99) inz
004800140930     d  paytype                       2  0
004900140930     d  hours                         5  2
005000140930
005100141006
005200141006     d InterfaceDefaultsDS...
005300141006     d                 ds                   qualified
005400141006     d  NTtype                        2  0
005500141006     d  OTtype                        2  0
005600141006     d  OTmultiplier                  7  5
005700141006     d  KronosCompany                 2  0
005800141007     d  NTmultiplier                  7  5
005900110303
006000110303         // Entry Program Prototype
006100110307     D  kronpay_pgm    pi                  extpgm('PAYCALCKRI')
006200110303     D  run_type                      1    const
006300100707
006400100707         // Escape Program   Prototype
006500100707     D  escape_pgm     pr                  extpgm('ESCPMSG')
006600100707     D                               10    const
006700100707     D                                7    const
006800110518
006900110518     D Line_command    PR                  extpgm('QCMDEXC')
007000110518     D cmd                          100    options(*varsize) const
007100110518     D cmdlen                        15p 5 const
007200110518      *
007300110303
007400110303     D  cktimeDS     e DS                  extname('CKTIMEPS')
007500110303     D  ckworkcdDS   e DS                  extname('CKWORKCD')
007600100707
007700010816
007800141006
007900110518         // Get Kronos data Library from config. table
008000110518         exec sql
008100110518            select hckrondta into :library from hmrcon;
008200110518
008300110518         if library = *blanks;
008400110518            *inlr = *on;
008500110518            escape_pgm(@pgm:'PAY0049');
008600110518         endif;
008700110518
008800110518         // Add Kronos data Library from library list
008900110518         cmd = 'ADDLIBLE LIB(' + library + ')' +
009000110518               ' POSITION(*LAST)';
009100110518         callp(e) line_command(cmd:%size(cmd));
009200110518
009300110307         // Get fortnight from Kronos file for verification
009400110307         // Get current fortnight's selection start date
009500110307         chain run_type prtyfmt;
009600110307
009700110311         prdend = %dec(%date(prtyndate) - %days(1));
009800110620         kr_prdend = %int(%char(prdend) + MIDNIGHT);
009900110311
010000141006         // get interface default information for processing
010100141006         // payroll timesheets
010200141006         exec sql
010300141006            select * into :InterfaceDefaultsDS from
010400141006                payintf2;
010500141007
010600141007         if InterfaceDefaultsDS.NTtype = *zeros
010700141007           or  InterfaceDefaultsDS.KronosCompany = *zeros;
010800141007            *inlr = *on;
010900141007            escape_pgm(@pgm:'PAY0050');
011000141007         endif;
011100110622
011200141006         //executing procedure to set up file position cursor//
011300110307         open_filesr();
011400110308
011500110308         //executing the procedure to read the data//
011600110308         dow not read_datasr();
011700110622
011800110308           //executing  procedure to write to the file//
011900110308           cal_hours();
012000110307
012100110308           emp = tpempn;
012200110307
012300110307           // Process record if the employee is an active
012400110307           // employee and is not salaried .
012500110307
012600110308           if not get_emp_info2('R':emp:emp_struc)
012700110308             and get_Cat(status:*omit:pay_opt) = 'A'
012800110308               and pay_opt = 'H';
012900110307
013000110307
013100110308              //-------------------------------------------------------------*-
013200110308              // Write record if regular or overtime hours is greater than zero
013300110308              // Process Normal time hours
013400140930
013500141007              if zcnt > *zeros;
013600141007                for xcnt = 1 to zcnt;
013700141007                  write_trn(Employee_hours(xcnt).paytype:
013800141007                          Employee_hours(xcnt).hours);
013900141007                endfor;
014000141007              endif;
014100140930
014200110307
014300110308           endif;
014400110307
014500110308           clear timcfmt;
014600110308
014700110307         enddo;
014800110307
014900110307         close_filesr();
015000110307
015100110518         // Remove Kronos data Library from library list
015200110518         cmd = 'RMVLIBLE LIB(' + library +')';
015300110518         callp(e) line_command(cmd:%size(cmd));
015400110518
015500090415         *inlr = *on;
015600090415         return;
015700110307
015800090415
015900960427
016000110307       //.........................................//
016100110307       //Procedure to set up file position cursor//
016200110307       //.......................................//
016300110307     p open_filesr     B
016400110307     D open_filesr     PI
016500141006
016600110307
016700110307        exec sql
016800110307          declare kfile cursor for select * from  cktimeps
016900141007           where tpcono = :InterfaceDefaultsDS.KronosCompany
017000110620           and tppend = :kr_prdend and tpptyp = 'H';
017100110307
017200110307        exec sql
017300110307          open kfile;
017400110307
017500141006
017600110307     p open_filesr     E
017700110307
017800110307       //................................//
017900110307       //Procedure to get data from file//
018000110307       //........................ .....//
018100110307     p read_datasr     B
018200110307     D read_datasr     PI              n
018300141006
018400110307
018500110307        exec sql
018600110307          fetch next from kfile into :cktimeDS;
018700110307
018800110307        if sqlstt = '02000';
018900110307           return *on;
019000110307          else;
019100110307           return *off;
019200110307        endif;
019300110307
019400141006
019500110307     p read_datasr     E
019600110307
019700110307      //............................//
019800110307      //procedure to close the file//
019900110307      //..........................//
020000110307     p close_filesr    B
020100110307     D close_filesr    PI
020200141006
020300110307
020400110307       exec sql
020500110307         close kfile;
020600110307
020700141006
020800110307     p close_filesr    E
020900110307
021000110307      //........................................//
021100110307      //  Routine to calculate employee hours **
021200110307      //........................................//
021300110307     p cal_hours       B
021400110307     D cal_hours       PI
021500110308
021600110308     D paycode         s              5
021700110308     D multiplier      s              7  4
021800110309     D otflag          s              1
021900110623     D non_work        s              1
022000110819     D non_pay         s              1
022100141007     D intMultiplier   s              7  5
022200141007     D intPaytype      s              2  0
022300110315
022400141006
022500110518       // NOTE: Current Afternoon Shift and Night shift calculations are
022600110518       //*      typed with this version of Kronos Iseries Central.          op
022700110307
022800140930         zcnt = *zeros;
022900141007         reset employee_hours;
023000140930
023100110307         for xcnt = 1 to 10;
023200110307              if pc(xcnt) <> *blanks;
023300110307
023400110307                 paycode = pc(xcnt);
023500110622
023600110307                 exec sql
023700110819                     select wcmult, wcover, wcnonw, wcnonp
023800110819                       into :multiplier, :otflag, :non_work, :non_pay
023900110309                        from ckworkcd where wccode = :paycode;
024000110307
024100110819                 select;
024200110819                  // IGNORE BUCKETS WITH NON PAY HOURS
024300110819                  when non_pay = 'Y';
024400110819
024500110819                  other;
024600110819                   // buckets that do not calculate OT.
024700141007
024800141007                       exec sql
024900141007                          select pin1paytp, pin1mult
025000141007                            into :intpaytype, :intmultiplier
025100141007                            from payintf1 where pin1kpayc = :paycode
025200141007                              and pin1sts = 'A';
025300141007
025400141007                       if  sqlstt = '00000';
025500141007                          accumulate_HRS(intpaytype:multiplier:intmultiplier);
025600141007                         else;
025700141007                          // default Handling
025800141007                          if  otflag = 'Y';
025900141007                             if non_work = 'N';
026000141007                                accumulate_HRS(InterfaceDefaultsDS.NTtype:
026100141007                                            multiplier:
026200141007                                            InterfaceDefaultsDS.NTmultiplier);
026300141007                             endif;
026400141007                            else;
026500141007                             accumulate_HRS(InterfaceDefaultsDS.OTtype:
026600141007                                            multiplier:
026700141007                                            InterfaceDefaultsDS.OTmultiplier);
026800141007                          endif;
026900141007                       endif;
027000140930
027100110819
027200110819                 endsl;
027300110819
027400110819              endif;
027500110307
027600110307         endfor;
027700110307
027800141006
027900110307     p cal_hours       E
028000140930
028100140930      //........................................//
028200140930      // Accumulate Hours
028300140930      //........................................//
028400140930     p Accumulate_HRS  B
028500140930     D Accumulate_HRS  PI
028600140930     D pay_type                       2  0 const
028700141007     D kr_multiplier                  7  4 const
028800141007     D pay_multiplier                 7  5 const
028900140930
029000141007          ycnt = %lookup(pay_type: Employee_hours(*).paytype);
029100140930          if ycnt = *zeros;
029200140930             zcnt += 1;
029300140930             Employee_hours(zcnt).paytype = pay_type;
029400141007             Employee_hours(zcnt).hours =
029500141007                ph(xcnt) * kr_multiplier * pay_multiplier;
029600140930            else;
029700140930             Employee_hours(ycnt).hours +=
029800141007                ph(xcnt) * kr_multiplier * pay_multiplier;
029900140930          endif;
030000141007
030100140930
030200140930
030300140930     p Accumulate_HRS  E
030400110308
030500140930
030600110308      //........................................//
030700110308      // Write hours to time CARD file
030800110308      //........................................//
030900110308     p write_trn       B
031000110308     D write_trn       PI
031100110308     D trn_type                       2  0 const
031200110308     D hours                          5  2 const
031300110308
031400141007
031500110308         chain (run_type:trn_type:emp) timcfmt;
031600110308         if %found;
031700110308           timc_hrs += hours;
031800110308           update timcfmt;
031900110308         else;
032000110308           timc_type = trn_type;
032100110308           timc_ptyp = run_type;
032200110308           timc_hrs = hours;
032300110308           timc_emp = emp;
032400110308           write timcfmt;
032500110308         endif;
032600141007
032700110308     p write_trn       E
