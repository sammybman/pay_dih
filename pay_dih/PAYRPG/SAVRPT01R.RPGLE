000100950418     H DECEDIT('.') DATEDIT(*DMY/)
000200151009     H dftactgrp(*no) actgrp(*new) bnddir('DISDIR')
000300010914     F** LIST MONTHLY WITHDRAWALS
000400980128     F** PROGRAMMER------ S.BRITTON
000500011002     F* LIABILITY TRANSACTION FILE
000600021125     Fsavconf   if   e             disk
000700980131     Fliabtl07  if   e           k disk
000800010914     Fsavmast   if   e           k disk
000900021125     Fqsysprt   o    f  132        printer oflind(*inof)
001000010914     ** Payroll PROTOTYPES
001100151009      /copy *libl/qcpysrc,PAYPR
001200151009      /copy qcpysrc,GENPR
001300980131     **
001400950919     D                SDS
001500950919     D  @PGM             *PROC
001600950919     D  @USER                254    263
001700040224     D*status          s              1
001800010914     D prd             s              6  0
001900010914     D end_date        s              8  0
002000010914     D totamt          s             11  2
002100010914     D name#           s             40
002200010920     D emp_name        s            -14    like(name#)
002300021009     D coname          s             30
002400980131     **
002500980131     C     trn_key       klist
002600021125     C                   kfld                    sactype
002700980131     C                   kfld                    prd
002800980131     **
002900010914     C                   callp     get_prd_info('*rng':*blanks:prd:*omit:
003000010914     C                                          end_date)
003100980131     **
003200980131     C     trn_key       chain     ltfmt                              lr
003300980131     C                   except    hedr
003400980131     **
003500980131     C                   dow       not(*inlr)
003600980131     C                   if        *inof
003700980131     C                   except    hedr
003800010914     C                   eval      *inof = *off
003900980131     C                   endif
004000990407     **  Get employee name
004100010914     c                   eval      status =  get_emp_info(ltemp:*omit:
004200010914     c                                             *blanks:name#)
004300010920     C                   eval      emp_name = name#
004400990428     **
004500010914     C                   if        get_cat(status) <> 'A'
004600010914     C                   eval      *in88 = *on
004700990428     C                   else
004800010914     C                   eval      *in88 = *off
004900990428     C                   endif
005000990407     **
005100010914     C     ltemp         chain     savmast                            89
005200980131     **
005300010914     C                   eval      totamt = totamt + ltamt
005400990428     ** Write detail report record
005500980131     C                   except    detail
005600980131     **
005700980131     C     trn_key       reade     ltfmt                                  lr
005800980131     C                   enddo
005900980131     **
006000980131     C                   except    fintot
006100021125     **
006200021125     C     *inzsr        begsr
006300021125     **
006400021125     C     *entry        plist
006500021125     C                   parm                    prd
006600021125
006700021125     C                   read      sacfmt
006800021125     C                   endsr
006900980131     **
007000021125     Oqsysprt   e            hedr           1 02
007100980131     O                       @pgm                10
007200011002     O                       coname              70
007300980131     O          e            hedr           2
007400980131     O                       @user               10
007500950418     O                                           79 '======================='
007600971113     O                                          120 'Run Date: '
007700980131     O                       udate         Y   +  1
007800980131     O          e            hedr           2
007900950418     O                                           64 'SAVINGS   WITHDRAWALS   '
008000010914     O                                           84 'FOR  PERIOD   ENDING'
008100980131     O                       end_date          +  1 '    /  /  '
008200980131     O          e            hedr           2
008300950418     O                                           40 'EMP. NO.'
008400950418     O                                           72 'PARTICULARS'
008500950418     O                                          110 'AMOUNT'
008600980131     **
008700980131     O          e            detail         2
008800010914     O               88                          14 'RESIGNED'
008900010914     O               89                          28 'NOT ON MASTER'
009000990407     O                       ltemp         Z     37
009100010920     O                       emp_name            65
009200980131     O                       lttext              96
009300980131     O                       ltamt         J    111
009400980131     **
009500980131     O          e            fintot         3
009600980128     O                       totamt        1    110
009700950418     O                                          114 '***'
009800980131     O          t    lr                          01
009900950418     O                                           79 '** END OF REPORT **'
010000990428     **
