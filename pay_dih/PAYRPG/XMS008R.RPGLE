000100950728     H DECEDIT('.') DATEDIT(*DMY/)
000200950816     F******  APPLY XMAS BONUS AWARD TO BONUS FILE
000300950816     FXMSWARD   IP   E           K DISK
000400950816     FXMSBONF   UF   E           K DISK
000500981217     **
000600981217     D                SDS
000700981217     D  @PGM             *PROC
000800981217     D  @USER                254    263
000900990921     **
001000981217     **
001100950816     C     EMP           CHAIN     XMSBONF                            77
001200950816     C     *IN77         IFEQ      '0'
001300950816     **
001400950816     C                   Z-ADD     XAWARD        AWARD
001500950816     **
001600950816     C                   UPDATE    XMSREC
001700950816     C                   ENDIF
001800990921     **
001900990921
002000990921     C     *inzsr        begsr
002100990921     C     *lock         IN        *DTAARA
002200990921     C     *DTAARA       define    xmsseqchk     xmseq#            2 0
002300990921     **
002400990921     C                   if        not(*in99)
002500990921     **
002600990921     C                   if        xmseq#  <> 2
002700990921     C                   call      'ESCPMSG'
002800990921     C                   parm                    @pgm
002900990921     C                   parm      'DIH0131'     msgid             7
003000110706     C                   parm      '*libl'       @lib             10
003100990921     C                   parm      'DIHMSGF'     @FILE            10
003200990921     C                   endif
003300990921     **
003400990921     C                   seton                                        99
003500990921     **
003600990921     C                   z-add     3             xmseq#
003700990921     c                   out       xmseq#
003800990921     C                   endif
003900990921     C                   endsr
