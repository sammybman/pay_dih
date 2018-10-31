000100171009     H  alwnull(*usrctl)
000200010907     Fpayalwtl02uf a e           K DISK    commit
000300010926     Fempchglog o    e             disk    commit
000400010907     Fpayalwml01if   e           k disk
000500011030     Fempexcptl1if   e           k disk
000600060222     Fempexcptl2if   e           k disk    rename(excfmt:excjobfmt)
000700061102     Fempexcptl3if   e           k disk    rename(excfmt:excclsfmt)
000800010907     Fempmast   if   e           k disk
000900010928
001000010504     ** Payroll PROTOTYPES
001100131024      /copy qcpysrc,PAYPR
001200131024      /copy qcpysrc,genpr
001300131024      /copy qcpysrc,emppr
001400060222
001500060222     D check_excption  PR              n
001600060222
001700020408     D                sds
001800131024     D  @user                358    367
001900981202     **
002000010716     D alttyp#         s                   like(alttyp)
002100010928     D altgrd#         s                   like(class)
002200010907     D basic_wage      s                   like(altamt)
002300010907     Dh_class          s                   like(class)
002400060222     D employee        s              6  0
002500060222     D choice          s              1  0
002600060222     D category        s              2
002700060222     D sys_date        s              8  0
002800060222     Ddatafield        s             10
002900060222
003000990403     **
003100010907     c     *entry        plist
003200010716     c                   parm                    alttyp#
003300060222     c                   parm                    datafield
003400060222     c                   parm                    choice
003500010907     **
003600010907     C     upd_key       klist
003700010907     C                   kfld                    alttyp#
003800010907     C                   kfld                    emp
003900010907     **
004000010907     C     get_key       klist
004100010907     C                   kfld                    alttyp#
004200010907     C                   kfld                    h_class
004300990403
004400010926     ** get system date
004500060222      /free
004600060222             get_sys_date(sys_date);
004700060222
004800060222          // choice 1 = class
004900060222          //        2 = employee
005000060222          //        3 = job category
005100060222
005200060222         if   %parms = 3;
005300060222             select;
005400060222                when choice = 1;
005500060222                 altgrd# = datafield;
005600060222                when choice = 2;
005700060222                 employee = %int(datafield);
005800060222                when choice = 3;
005900060222                 category = datafield;
006000060222                other;
006100060222             endsl;
006200060222         endif;
006300060222
006400060222      /end-free
006500981120     **
006600010716     **************************
006700010716     ** Get Employee records **
006800010716     **************************
006900981120     c
007000010907     C     *loval        setll     emprec
007100010907     C                   read      emprec
007200010907     C                   dow        not(%eof)
007300060222
007400010928     C     'P':' '       xlate     class         h_class
007500010907     ** Get actual Employee class
007600060222     C                   if        get_cat(status) = 'A'
007700060222
007800060222     C                   if        choice = 1  and altgrd# = h_class
007900060222     C                             or choice = 2 and employee = emp
008000060222     C                             or choice = 3 and category = empcat
008100060222     C                             or choice = 0
008200981126     C
008300010926     C                   eval      altamt = *zeros
008400011030     ** Do Calcs if Employee not on exception list
008500060222     C                   if        not check_excption()
008600010907     C     upd_key       chain     altfmt
008700981120     **
008800010926     C                   exsr      write_emp_log
008900010907     C                   exsr      perform_Calcs
009000010716     ** Update  Employee Records
009100010816     ** check if active
009200011002     C                   if         %found(payalwtl02)
009300010713     C                   update    altfmt
009400010907     C                   else
009500010907     ** Write new Employee Records
009600011002     C                   if        altamt <> *zeros
009700010907     C                   eval      altsts = ptyrtype
009800010907     C                   eval      alttyp = alttyp#
009900010907     C                   eval      altemp = emp
010000010907     C                   write     altfmt
010100011002     C                   endif
010200010907     **
010300011030     C                   endif
010400010907     C                   endif
010500010907     C                   endif
010600060222     C                   endif
010700981127     **
010800010907     C                   read      emprec
010900010713     C                   enddo
011000010504
011100010907     C                   eval      *inlr = *on
011200981120     c
011300010430
011400010430     ******************
011500010430     ** Calcs Section *
011600010430     ******************
011700010716     c     perform_calcs begsr
011800981202     c
011900050520      /free
012000050520               get_payty_info('RD':alttyp#:payty_struc);
012100050520
012200050525      /end-free
012300010907
012400010907     C                   if        ptyfixcls <> 'Y'
012500010907     C                   eval      basic_wage = get_basic_wage(emp)
012600010907     **
012700011002     C                   if        ptypaytyp <> 'H'
012800010907     C                   select
012900010907     C                   when      ptypercen <> *zeros
013000010907     C                   eval      altamt = basic_wage * ptypercen / 100
013100011002     ** check for any probation deductions
013200011002     C                   eval      altamt = calc_probation(emp:class:
013300090619     C                                        alttyp#:altamt:sys_date)
013400010907     C                   when      ptyfactor <> *zeros
013500011002     C                   eval      altamt = basic_wage * ptyfactor
013600011002     ** check for any probation deductions
013700011002     C                   eval      altamt = calc_probation(emp:class:
013800090619     C                                        alttyp#:altamt:sys_date)
013900010907     C                   endsl
014000011002     C                   endif
014100010907     **
014200010907     C                   else
014300010907     **
014400010907     C     get_key       chain     alwfmt
014500010907     C                   if        %found
014600010907     ** check for any probation deductions
014700010907     C                   eval      altamt = calc_probation(emp:class:
014800090619     C                                        alttyp#:alwamt:sys_date)
014900010907     **
015000010907     C                   endif
015100010907     C                   endif
015200010907
015300010716     ** Round up all cents to dollars
015400010716     C                   eval      altamt = round_up(altamt:'U')
015500981202     C
015600010713     C                   endsr
015700010504     **
015800010926     ***********************************************************************
015900010926     *** Write employee log for update ***
016000010926     ***********************************************************************
016100010926     c     write_emp_log begsr
016200010926     c                   eval      elogtype  = alttyp#
016300010926     C                   eval      elogclass = class
016400010926     C                   eval      elogcdate = sys_date
016500010926     C                   eval      elogedate = sys_date
016600010926     C                   eval      elogemp   = emp
016700010926     C                   eval      elogpval  = altamt
016800020408     C                   eval      eloguser  = @user
016900010926     C                   write     elogfmt
017000010926     c                   endsr
017100060222
017200060222     P*--------------------------------------
017300060222     P* Procedure name: check_excption
017400060222     P* Purpose:        check exception
017500060222     P* Returns:        *boolean
017600060222     P* Parameter:      *none
017700060222     P*--------------------------------------
017800060222     P check_excption  B
017900060222     D check_excption  PI              n
018000060222
018100060222      /free
018200060222
018300060222           chain (alttyp#:emp)  excfmt;
018400060222             if   %found(empexcptl1);
018500060222                return *on;
018600060222             endif;
018700060222
018800060222           if  empcat <> *blanks;
018900060224            chain (empcat:alttyp#)  excjobfmt;
019000060222             if   %found(empexcptl2);
019100060222                return *on;
019200060222             endif;
019300060222           endif;
019400061102
019500061102           if  h_class <> *blanks;
019600061102            chain (h_class:alttyp#)  excclsfmt;
019700061102             if   %found(empexcptl3);
019800061102                return *on;
019900061102             endif;
020000061102           endif;
020100060222
020200060222           return  *off;
020300060222      /end-free
020400060222
020500060222     P check_excption  E
