000100950719     H
000200010716     Fpayalwml01uf   e           K DISK    commit
000300010926     Fclschglog o    e             disk    commit
000400010504     **
000500010504     ** Payroll PROTOTYPES
000801170120      /copy qcpysrc,PAYPR
000802170120      /copy qcpysrc,genpr
000803170120      /copy qcpysrc,emppr
000900981202     **
001000010716     D alttyp#         s                   like(alwtyp)
001100010716     D factor          s              6  3
001200010716     D percen          s              4  2
001300010926     Dsys_date         s              8  0
001400990403     **
001500010713     c     *entry        plist
001600010716     c                   parm                    alttyp#
001700010716     c                   parm                    factor
001800010716     c                   parm                    percen
001900990403
002000010926     ** get system date
002100010926     c                   callp     get_sys_date(sys_date)
002200981120     **
002300010816     ************************
002400010816     ** Get CLass records **
002500010816     ************************
002600981120     c
002700050520      /free
002800050525            get_payty_info('RD':alttyp#:payty_struc);
002900050520
003000050525      /end-free
003100981126     C
003200010716     C     alttyp#       chain     alwfmt                             lr
003300981120     **
003400010716     C                   dow        not(*inlr)
003500981127     C
003600010816     ** Update  Class Records
003700010504
003800010926     C                   exsr      write_cls_log
003900010716     C                   exsr      perform_Calcs
004000010716     C                   update    alwfmt
004100981127     **
004200011023     C     alttyp#       reade     alwfmt                                 lr
004300010713     C                   enddo
004400010504
004500981120     c
004600010430
004700010430     ******************
004800010430     ** Calcs Section *
004900010430     ******************
005000010716     c     perform_calcs begsr
005100981202     c
005200010716     **
005300010716     C                   select
005400010716     C                   when      factor <> *zeros
005500010716     C                   eval      alwamt = alwamt * ptyfactor /factor
005600010716     C                   when      percen <> *zeros
005700010716     C                   eval      alwamt = alwamt * ptypercen /percen
005800010716     C                   endsl
005900981123
006000010716     ** Round up all cents to dollars
006100010716     C                   eval      alwamt = round_up(alwamt:'U')
006200981202     C
006300010713     C                   endsr
006400981202     C
006500010926     ***********************************************************************
006600010926     *** Write class log for update ***
006700010926     ***********************************************************************
006800010926     c     write_cls_log begsr
006900010926     c                   eval      clogtype  = alttyp#
007000010926     C                   eval      clogclass = alwgrd
007100010926     C                   eval      clogcdate = sys_date
007200010926     C                   eval      clogedate = sys_date
007300010926     C                   eval      clogpval  = alwamt
007400010926     C                   write     clogfmt
007500010926     c                   endsr
007600010504     **
007700010504     c
