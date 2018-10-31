000100950405     H DECEDIT('.') DATEDIT(*DMY/)
000200950405     F**
000300010420     F** Regular time & Overtime calculations
000400010420     F** PROGRAMMER----** C.WILLIAMS  ADJ. S.BRITTON
000500950405     F**
000600020111     Fpayrdefl03if   e           K DISK
000700010427     Fpaytimfl01if   e           K DISK
000800010420     Fpayworkf  o    e           k disk
000900010426     **
001000010426     Drun_type         s              1
001100980330     **
001200990413     **-------------------------------------------------------------------------
001300010426     C     *entry        plist
001400010426     C                   parm                    run_type
001500010427     **
001600010427     C     trn_key       klist
001700010427     C                   kfld                    run_type
001800010427     C                   kfld                    prdftype
001900961213     **
002000010427     C     run_type      chain     prdffmt                            lr
002100010427     C                   dow       not(*inlr)
002200010420
002300010427     C     trn_key       chain     timfmt                             77
002400010426     C                   dow       not(*in77)
002500010420     ** Write Info to work file
002600010427     C                   eval      wrkemp = timemp
002700010427     C                   eval      wrkcode = timcode
002800010427     C                   eval      wrktyp = timtyp
002900010427     C                   eval      wrkamt = timamt
003000010427     C                   eval      wrkhrs = timhrs
003100010427     C                   eval      wrkprior = timprior
003200010426     C                   write     wrkfmt
003300010427     C     trn_key       reade     timfmt                                 77
003400010426     **
003500010426     C                   enddo
003600010420     **
003700010426
003800010427     C     run_type      reade     prdffmt                                lr
003900010426     C                   enddo
004000980330     **
