000100950530     H DATEDIT(*DMY)
000200010709     ** Programmed by Sammy Britton
000300010709     **        and by Chris Williams
000400010706     ** 01/07/06
000500010911     Fvacation  up   e             disk    commit
000600010628      * Payroll Time-card file
000700010911     Ftimecrdf  uf a e           k disk    commit
000800010706     F* Department Master File
000900010808     Fdeptl05   if   e           k disk
001000010808     F* Holiday Master File
001100010808     Fholl01    if   e           k disk
001200010706     F* Paycycle Master File
001300010706     Fpaycycd   if   e           k disk
001400010718     ** Payroll PROTOTYPES
001500010718      /copy paylib/payrpg,PAYPR
001600010706     **
001700010717     DLast_Date        s              8  0
001800010717     DNext_Date        s              8  0
001900010719     Ddate_field       s               d   datfmt(*iso)
002000010717     DPay_cycle        s              2
002100010717     DPrd_range        s              3  0
002200010717     DDays_leave       s              3  0
002300010717     DTrn_Ptype        s              2  0 inz(1)
002400010717     DPay_factor       s                   like(cycyearf)
002500010807     DDept             s              3  0
002600010816     Dholiday_cnt      s              2  0
002700010816     Drun_Type         s              1
002800970417     **
002900950530     D                SDS
003000950530     D  @PGM             *PROC
003100950530     D  @USER                254    263
003200010816     **
003300010706     C     vac_key       klist
003400010816     C                   kfld                    run_Type
003500010706     C                   kfld                    trn_Ptype
003600010706     C                   kfld                    vacemp
003700010706     **
003800010706
003900010706     C                   exsr      @calc
004000970714     **
004100010706     *************************************************************
004200010706     *****    Update Time Record File and Vacation File     ******
004300010706     *************************************************************
004400010709     C     @Calc         begsr
004500010816     C                   eval      timc_hrs = *zeros
004600950629     **
004700010706     ** if employee leave falls within pay period, add pay hours in
004800010706     ** time record file
004900010706     C                   if        vacsdate >= last_date
005000010809     C                   exsr      get_holidays
005100010706
005200010706     ** Get Employee Department No.
005300010807     c                   callp     get_emp_info(vacemp:dept)
005400010706     ** Get department hours and cycle
005500010706     C     dept          chain     dhrec
005600010706     ** Get Factor (for calcs) from cycle code
005700010706     C     dhrcode       chain     cycfmt
005800010706     ** Get Time card record
005900010816     C     vac_key       chain     timcfmt
006000010718
006100010718     ** get no. of days leave
006200010719     C                   if        vacedate > next_date
006300010719     C                   eval      days_leave = get_workn_dys(vacsdate:
006400010719     C                                          next_date:ddyweek)
006500010719     C                   else
006600010719     C                   eval      days_leave = get_workn_dys(vacsdate:
006700010913     C                                          vacedate:ddyweek)
006800010719     C                   endif
006900010719
007000010718     ** get no. of days - pay period
007100010718     C                   eval      prd_range = get_workn_dys(last_date:
007200010718     C                                          next_date:ddyweek)
007300010718     **
007400010816     C                   eval      timc_hrs = timc_hrs + (dhours * cycyearf
007500010709     C                                / pay_factor * days_leave / prd_range)
007600010706     C                   if        %found
007700010816     C                   update    timcfmt
007800010706     C                   else
007900010816     C                   eval      timc_emp = vacemp
008000010816     C                   eval      timc_type = trn_pType
008100010816     C                   eval      timc_ptyp = run_Type
008200010816     C                   write     timcfmt
008300010706     C                   endif
008400010706     C                   endif
008500010706
008600010706     ** if employee leave extends into the next pay period chg start date
008700010706     ** to the beginning of new period
008800010913     C                   if        vacedate > next_date
008900010719     C                   move      next_date     date_field
009000010719     C                   adddur    1:*d          date_field
009100010719     C                   move      date_field    vacsdate
009200010706     C                   update    vacfmt                               99
009300010706     C                   else
009400010706     C                   delete    vacfmt                               99
009500010706     C                   endif
009600970417     **
009700970502     C                   endsr
009800010808     *****************************************************
009900010808     *****  Calculate no. of holidays within Period ******
010000010808     *****************************************************
010100010808     C     get_holidays  begsr
010200010808     **
010300010809     C                   clear                   holiday_cnt
010400010808     **
010500010809     C     vacsdate      setll     holfmt                                       START DATE
010600010808     C                   read      holfmt
010700010808     **
010800010808     C                   dow       not(%eof)
010900010808     **
011000010809     ** calculate no. of holidays within period
011100010809     C                   if        hodate <= vacedate
011200010808     **
011300010809     C                   eval      holiday_cnt = holiday_cnt + 1
011400010809     **
011500010808     C                   else
011600010808     C                   leave
011700010808     C                   endif
011800010808     **
011900010808     C                   read      holfmt
012000010808     C                   enddo
012100010809     ** adjust end date by no. of holidays
012200010809     C                   move      vacedate      date_field
012300010809     C                   adddur    holiday_cnt:*ddate_field
012400010809     C                   move      date_field    vacedate
012500030530     C                   eval      day = dayofweek(date)
012600030530     C
012700030530     C                   select
012800030530     C                   when      wrk_dayweek = 5
012900030530     C                   if        day = 6 or day = 7
013000030530C
013100030530     C                   eval      work_days = work_days - 1
013200030530     C                   endif
013300030530     C                   when      wrk_dayweek = 6
013400030530     C                   if        day = 7
013500030530     C                   eval      work_days = work_days - 1
013600030530     C                   endif
013700030530     C                   endsl
013800010808     C                   endsr
013900950629     C***************************************************************
014000010706     ***************************
014100010706     *****  Iniz Routine  ******
014200010706     ***************************
014300010706     C     *inzsr        begsr
014400010706     **
014500010706     C     *entry        plist
014600010816     C                   parm                    run_Type
014700010706     C                   parm                    Last_Date
014800010717     C                   parm                    Next_Date
014900010706     C                   parm                    Pay_Cycle
015000010913     **
015100010913     C                   move      next_date     date_field
015200010913     C                   subdur    1:*d          date_field
015300010913     C                   move      date_field    next_date
015400010706     **
015500010706     C     pay_cycle     chain     cycfmt
015600010706     C                   eval      pay_factor = cycyearf
015700010706     C                   endsr
