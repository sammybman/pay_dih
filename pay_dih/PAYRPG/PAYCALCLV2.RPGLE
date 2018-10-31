000100000000     H DATEDIT(*DMY)
000200000000      // Programmed by Sammy Britton
000300000000      //        and by Chris Williams
000400000000      // 01/07/06
000500000000     Fvacation  up   e             disk    commit
000600000000      // Payroll Time-card file
000700000000     Ftimecrdf  uf a e           k disk    commit
000800000000      // Department Master File
000900000000     Fdeptl05   if   e           k disk
001000000000      // Holiday Master File
001100000000     Fholl01    if   e           k disk
001200000000      // Paycycle Master File
001300000000     Fpaycycd   if   e           k disk
001400000000      // Payroll PROTOTYPES
001500110803      /copy QCPYSRC,PAYPR
001600110803      /copy QCPYSRC,GENPR
001700000000
001800000000      //
001900000000     DLast_Date        s              8  0
002000000000     DNext_Date        s              8  0
002100000000     Ddate_field       s               d   datfmt(*iso)
002200000000     DPay_cycle        s              2
002300000000     DPrd_range        s              3  0
002400000000     DDays_leave       s              3  0
002500000000     DTrn_Ptype        s              2  0 inz(1)
002600000000     DPay_factor       s                   like(cycyearf)
002700040224     D*ept             s              3  0
002800000000     Dxcnt             s                   like(holiday_cnt)
002900000000     Dholiday_cnt      s              2  0 inz(0)
003000000000     Drun_Type         s              1
003100000000     Dday              s              1s 0
003200000000      //
003300000000     D                SDS
003400000000     D  @PGM             *PROC
003500000000     D  @USER                254    263
003600000000      //
003700000000     C     vac_key       klist
003800000000     C                   kfld                    run_Type
003900000000     C                   kfld                    trn_Ptype
004000000000     C                   kfld                    vacemp
004100040224        //
004200000000
004300000000      /FREE
004400000000       exsr @calc;
004500000000       //
004600000000       //***********************************************************
004700000000       //***    Update Time Record File and Vacation File     ******
004800000000       //***********************************************************
004900000000       begsr @Calc;
005000000000         timc_hrs = *zeros;
005100000000         //
005200000000         // if employee leave falls within pay period, add pay hours in
005300000000         // time record file
005400000000         if vacsdate >= last_date;
005500000000           exsr get_holidays;
005600000000
005700000000           // Get Employee Department No.
005800000000           get_emp_info(vacemp:dept);
005900000000           // Get department hours and cycle
006000000000           chain dept dhrec;
006100000000           // Get Factor (for calcs) from cycle code
006200000000           chain dhrcode cycfmt;
006300000000           // Get Time card record
006400000000           chain vac_key timcfmt;
006500000000
006600000000           // get no. of days leave
006700000000           if vacedate > next_date;
006800000000             days_leave = get_workn_dys(vacsdate:
006900080528                 next_date:ddyweek);
007000000000           else;
007100000000             days_leave = get_workn_dys(vacsdate:
007200080722                 vacedate:ddyweek);
007300080722            //   vacedate:ddyweek) - 1;
007400000000           endif;
007500000000
007600000000           // get no. of days - pay period
007700000000           prd_range = get_workn_dys(last_date:
007800080528               next_date:ddyweek);
007900140321
008000140321         //monitor;
008100000000           timc_hrs = timc_hrs + (dhours * cycyearf
008200000000               / pay_factor * days_leave / prd_range);
008300140321         //on-error;
008400140321         //endmon;
008500140321
008600000000           if %found;
008700000000             update timcfmt;
008800000000           else;
008900000000             timc_emp = vacemp;
009000000000             timc_type = trn_pType;
009100000000             timc_ptyp = run_Type;
009200000000             write timcfmt;
009300000000           endif;
009400000000         endif;
009500000000
009600000000         // if employee leave extends into the next pay period chg start date
009700000000         // to the beginning of new period
009800000000         if vacedate > next_date;
009900030603           date_field = %date(next_date) + %days(1);
010000000000      /END-FREE
010100000000     C                   move      date_field    vacsdate
010200000000      /FREE
010300000000           update(E) vacfmt;
010400000000           *IN99 = %ERROR;
010500000000         else;
010600000000           delete(E) vacfmt;
010700000000           *IN99 = %ERROR;
010800000000         endif;
010900000000         //
011000000000       endsr;
011100000000       //***************************************************
011200000000       //***  Calculate no. of holidays within Period ******
011300000000       //***************************************************
011400000000       begsr get_holidays;
011500080325
011600000000         reset holiday_cnt;
011700080325
011800000000         setll vacsdate holfmt;                                               //START DATE
011900000000         read holfmt;
012000080325
012100000000         dow not(%eof);
012200080325
012300030620           // calculate no. of holidays within period
012400080528           if hodate < vacedate;
012500080325
012600080325             holiday_cnt += 1;
012700080325             day = dayofweek(hodate);
012800080325
012900080325             // DO NOT count holidays falling on a weekend
013000080325             // for 5 day employees
013100080325             select;
013200080325                when   ddyweek = 5;
013300080325
013400080325                     if  day = 6 or day = 7;
013500080325                       holiday_cnt -= 1;
013600080325                     endif;
013700080325
013800080325             endsl;
013900080325
014000000000           else;
014100000000             leave;
014200000000           endif;
014300080325
014400000000           read holfmt;
014500000000         enddo;
014600080528
014700000000         // adjust end date by no. of holidays
014800000000
014900000000         for xcnt = 1 to holiday_cnt;
015000000000           date_field = %date(vacedate) + %days(1);
015100000000      /end-free
015200000000     C                   move      date_field    vacedate
015300000000      /free
015400000000
015500000000
015600000000           day = dayofweek(vacedate);
015700000000
015800000000           select;
015900000000              when   ddyweek = 5;
016000000000                   if  day = 6 or day = 7;
016100000000
016200080325                      holiday_cnt += 1;
016300000000
016400000000                   endif;
016500000000
016600000000              when   ddyweek = 6;
016700000000                   if day = 7;
016800080325                      holiday_cnt += 1;
016900000000                   endif;
017000000000           endsl;
017100000000         endfor;
017200000000
017300000000       endsr;
017400000000       //**************************************************************
017500000000       //*************************
017600000000       //***  Iniz Routine  ******
017700000000       //*************************
017800000000       begsr *inzsr;
017900000000         //
018000000000      /END-FREE
018100000000     C     *entry        plist
018200000000     C                   parm                    run_Type
018300000000     C                   parm                    Last_Date
018400000000     C                   parm                    Next_Date
018500000000     C                   parm                    Pay_Cycle
018600040224        //
018700000000      /FREE
018800000000          date_field = %date(next_date) - %days(1);
018900000000      /end-free
019000000000     C                   move      date_field    next_date
019100000000      /FREE
019200040224        //
019300000000         chain pay_cycle cycfmt;
019400000000         pay_factor = cycyearf;
019500000000       endsr;
019600000000      /END-FREE
