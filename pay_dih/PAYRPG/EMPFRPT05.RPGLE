000100950418     H DECEDIT('.') DATEDIT(*DMY/)
000200170216     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR') alwnull(*usrctl)
000300010914     F** LIST MONTHLY WITHDRAWALS
000400980128     F** PROGRAMMER------ S.BRITTON
000500010920     F* LIABILITY TRANSACTION FILE
000600980131     Fliabtl07  if   e           k disk
000700010925     Fpayhisl10 if   e           k disk
000800010920     Fpaytranl03if   e           k disk
000900021219     Fsavmast   if a e           k disk    commit
001000021125     Fsavconf   if   e             disk
001100010914     ** Payroll PROTOTYPES
001200111013      /copy QCPYSRC,PAYPR
001300010920      *-- Get next value --*                                                --
001400010920     Dwrite_record     pr
001500010920     D                                6  0 value
001600021125     **
001700021125     D  typ            s              2  0
001800021125     D pay_type        s              2  0
001900010914     D prd             s              6  0
002000010925     D last_emp        s              6  0
002100010920     D err#            s              7
002200980128     **
002300980131     C     *entry        plist
002400010920     C                   parm                    prd
002500021125     ** read control file
002600021125     C                   read      sacfmt
002700021125     C                   eval      pay_type = sacdtype
002800021125     C                   eval      typ = sactype
002900980131     **
003000980131     C     trn_key       klist
003100980131     C                   kfld                    typ
003200980131     C                   kfld                    prd
003300010920     **
003400010920     C     trn_key2      klist
003500010920     C                   kfld                    phisemp
003600010920     C                   kfld                    pay_type
003700980131     **
003800010920     C     trn_key       chain     ltfmt
003900980131     **
004000010920     C                   dow       not(%eof(liabtl07))
004100990428     **
004200010920     C                   callp     write_record(ltemp)
004300980131     **
004400010920     C     trn_key       reade     ltfmt
004500980131     C                   enddo
004600010920     **
004700010920     C     prd           setll     phisfmt
004800010920     C     prd           reade     phisfmt
004900010925     C                   eval      last_emp = *zeros
005000010925     C                   dow       not(%eof(payhisl10))
005100010925     C                   if        last_emp <> phisemp
005200010920     **
005300010920     C     trn_key2      setll     ptisfmt
005400010920     C     trn_key2      reade     ptisfmt
005500010920     C                   dow       not(%eof(paytranl03))
005600010920     C                   if        ptrnamt > *zeros
005700010920     C                   callp     write_record(ptrnemp)
005800010926     C                   leave
005900010920     C                   endif
006000010920     C     trn_key2      reade     ptisfmt
006100010920     C                   enddo
006200010925     C                   eval      last_emp = phisemp
006300010925     C                   endif
006400010920     **
006500010920     C     prd           reade     phisfmt
006600010920     C                   enddo
006700010920     **
006800010920     C                   eval      *inlr = *on
006900010920     C                   return
007000980131     **
007100010920     **********************
007200010920     ** Write new record **
007300010920     **********************
007400010920     Pwrite_Record     b
007500010920     **
007600010920     Dwrite_record     pi
007700010920     Dwrite_emp                       6  0 value
007800010920     **
007900010920     C     write_emp     chain     savmast
008000010920     C                   if        not(%found)
008100010920     **  Get employee name
008200011228     c                   eval      status =  get_emp_info(write_emp:*omit:
008300010920     c                                       *blanks:*omit:*omit:*omit:
008400010920     c                                       *omit:err#)
008500010920     C                   if        get_cat(status) = 'A' and err# = *blanks
008600010920     C                   clear                   savrec
008700010920
008800010920     C                   eval      savemp  = write_emp
008900010920     C                   write     savrec
009000010920     C                   endif
009100010920     C                   endif
009200010920     Pwrite_record     e
