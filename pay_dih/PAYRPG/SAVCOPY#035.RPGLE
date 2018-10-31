000100010905     **
000200010914     ** Get Deduction transactions
000300010905     C     get_Ded_rtn   begsr
000400010921     **
000500010921     C     trn_key       klist
000600010921     C                   kfld                    savemp
000700010921     C                   kfld                    prno
000800010921     **
000900010921     C     trn_key2      klist
001000010921     C                   kfld                    savemp
001100010921     C                   kfld                    pay_type
001200010921     C                   kfld                    phisndate
001300010921     **
001400010920     C                   eval      prd_ded = *zeros
001500010906     C     trn_key       setll     phisfmt
001600010921     C     trn_key       reade     phisfmt
001700010906     C                   eval      last_date = *zeros
001800011228     C                   dow       not(%eof(payhisl11))
001900010906     **
002000010906     C                   if        last_date <> phisndate
002100010906     C     trn_key2      setll     ptisfmt
002200010906     C     trn_key2      reade     ptisfmt
002300010921     C                   dow       not(%eof(paytranl03))
002400011228     **
002500010906     C                   eval      prd_ded = prd_ded + ptrnamt
002600010906     C     trn_key2      reade     ptisfmt
002700010905     C                   enddo
002800010906     C                   eval      last_date = phisndate
002900010906     C                   endif
003000010906     **
003100010921     C     trn_key       reade     phisfmt
003200010905     C                   enddo
003300010905     C                   endsr
003400010914
003500010914     ** Get liability transactions
003600010914     C     get_lib_rtn   begsr
003700010921     **
003800010921     C     lib_key       klist
003900010921     C                   kfld                    lib_type
004000010921     C                   kfld                    prno
004100010921     C                   kfld                    savemp
004200010914
004300010920     C                   eval      withdr = *zeros
004400010914     C     lib_key       setll     ltfmt
004500010914     C     lib_key       reade     ltfmt
004600010914     **
004700010914     C                   dow       not(%eof)
004800010914     **
004900010914     C                   eval      withdr = withdr + ltamt
005000010914     **
005100010914     C     lib_key       reade     ltfmt
005200010914     C                   enddo
005300010914     C                   endsr
