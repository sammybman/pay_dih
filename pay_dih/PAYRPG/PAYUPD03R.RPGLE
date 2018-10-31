000100171009     H  alwnull(*usrctl)
000200010831     Fpayalwtl02uf a e           K DISK    commit
000300010926     Fempchglog o    e             disk    commit
000400011030     Fempexcptl1if   e           k disk
000500060222     Fempexcptl2if   e           k disk    rename(excfmt:excjobfmt)
000600061102     Fempexcptl3if   e           k disk    rename(excfmt:excclsfmt)
000700010827     Fempmast   if   e           k disk
000800010504     **
000900010504     ** Payroll PROTOTYPES
001000151009      /copy *libl/qcpysrc,PAYPR
001100151009      /copy qcpysrc,genpr
001200151009      /copy qcpysrc,emppr
001300060222
001400060222     D check_excption  PR              n
001500060222
001600020408     D                sds
001700020408     D  @user                254    263
001800981202     **
001900010716     D alttyp#         s                   like(alttyp)
002000010827     D altgrd#         s                   like(class)
002100010827     D old_amt         s                   like(altamt)
002200010827     D old_val         s                   like(altamt)
002300010827     D new_amt         s                   like(altamt)
002400010827     D upd_amt         s                   like(altamt)
002500010827     Dh_class          s                   like(class)
002600010926     Dsys_date         s              8  0
002700990403     **
002800010713     c     *entry        plist
002900010716     c                   parm                    alttyp#
003000010827     c                   parm                    altgrd#
003100010827     c                   parm                    old_amt
003200010827     c                   parm                    new_amt
003300010827     **
003400010827     C     upd_key       klist
003500010827     C                   kfld                    alttyp#
003600010827     C                   kfld                    emp
003700990403
003800010926     ** get system date
003900010926     c                   callp     get_sys_date(sys_date)
004000981120     **
004100010716     **************************
004200010716     ** Get Employee records **
004300010716     **************************
004400981126     C
004500010827     C                   if        new_amt > *zeros
004600010827     C     *loval        setll     emprec
004700010827     C                   read      emprec
004800010827     C                   dow        not(%eof)
004900010827     ** Get actual Employee class
005000010827     C     'P':' '       xlate     class         h_class
005100010827     C                   if        altgrd# = h_class
005200010827     C                             and get_cat(status) = 'A'
005300011030
005400011030     ** Do Calcs if Employee not on exception list
005500060222     C                   if        not check_excption()
005600010827     C     upd_key       chain     altfmt
005700981120     **
005800010827     ** check for any probation deductions
005900010907     C                   eval      upd_amt = calc_probation(emp:class:
006000090619     C                                        alttyp#:new_amt:sys_date)
006100010827     ** check for any probation deductions
006200010907     C                   eval      old_val = calc_probation(emp:class:
006300090619     C                                        alttyp#:old_amt:sys_date)
006400010827     ** Round up all cents to dollars
006500031108     C                   eval      old_val = round_up(old_val:'U')
006600010827     C                   eval      upd_amt = round_up(upd_amt:'U')
006700010827     C
006800010827     C                   if         %found
006900010504
007000010827     ** Update  Employee Records
007100010827     C                   if         altamt <= old_val
007200010926     C                   exsr      write_emp_log
007300010827     C                   eval      altamt = upd_amt
007400010713     C                   update    altfmt
007500010827     C                   endif
007600010827     C                   else
007700010827     ** Write new Employee Records
007800050520      /free
007900050525            get_payty_info('RD':alttyp#:payty_struc);
008000050520
008100050525      /end-free
008200010827     C                   eval      altsts = ptyrtype
008300010827     C                   eval      alttyp = alttyp#
008400010827     C                   eval      altamt = upd_amt
008500010827     C                   eval      altemp = emp
008600010827     C                   write     altfmt
008700981127     **
008800011030     C                   endif
008900010827     C                   endif
009000010827     C                   endif
009100010827     **
009200010827     C                   read      emprec
009300010713     C                   enddo
009400010827     C                   endif
009500010827     **
009600010827     C                   eval      *inlr = *on
009700010504
009800981120     c
009900010430
010000981202     C
010100010926     ***********************************************************************
010200010926     *** Write employee log for update ***
010300010926     ***********************************************************************
010400010926     c     write_emp_log begsr
010500010926     c                   eval      elogtype  = alttyp#
010600010926     C                   eval      elogclass = class
010700010926     C                   eval      elogcdate = sys_date
010800010926     C                   eval      elogedate = sys_date
010900010926     C                   eval      elogemp   = emp
011000010926     C                   eval      elogpval  = altamt
011100020408     C                   eval      eloguser  = @user
011200010926     C                   write     elogfmt
011300010926     c                   endsr
011400060222
011500060222     P*--------------------------------------
011600060222     P* Procedure name: check_excption
011700060222     P* Purpose:        check  exception
011800060222     P* Returns:        *boolean
011900060222     P* Parameter:      *none
012000060222     P*--------------------------------------
012100060222     P check_excption  B
012200060222     D check_excption  PI              n
012300060222
012400060222      /free
012500060222
012600060222           chain (alttyp#:emp)  excfmt;
012700060222             if   %found(empexcptl1);
012800060222                return *on;
012900060222             endif;
013000060222
013100060222           if  empcat <> *blanks;
013200060224            chain (empcat:alttyp#)  excjobfmt;
013300060222             if   %found(empexcptl2);
013400060222                return *on;
013500060222             endif;
013600060222           endif;
013700061102
013800061102           if  h_class <> *blanks;
013900061102            chain (h_class:alttyp#)  excclsfmt;
014000061102             if   %found(empexcptl3);
014100061102                return *on;
014200061102             endif;
014300061102           endif;
014400060222
014500060222           return  *off;
014600060222      /end-free
014700060222
014800060222     P check_excption  E
