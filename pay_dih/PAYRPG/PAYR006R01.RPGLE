000100010424     H DATEDIT(*ymd)
000200090415      // PAYROLL POSTING TO GL.
000300090415      // PROGRAMMER:-S. Britton & C. Williams
000400010806     Fpaytranl02if   e           k disk
000500010424     Fpaytdefl01if   e           k disk
000600010424     Fpayrtypl01if   e           k disk
000700090415      // Payroll PROTOTYPES
000800090415      //
000900010806     D run_type        s                   like(ptrntype)                       payroll run type
001000010806     D emp_no          s                   like(ptrnemp)                        employee #
001100010806     D run_date        s                   like(ptrnndate)                      run date
001200010806     D trn_code        s                   like(ptrntcode)                      transaction code
001300010806     D trn_hrs         s                   like(ptrnhrs)                        total amount
001400010806     D trn_value       s             11  2                                      total amount
001500090415      //
001600010806     C     *entry        plist
001700010806     C                   parm                    run_type
001800010806     C                   parm                    emp_no
001900010806     C                   parm                    run_date
002000010806     C                   parm                    trn_code
002100010806     C                   parm                    trn_hrs
002200010806     C                   parm                    trn_value
002300090415      //
002400010424     c     trans_key     klist
002500010806     C                   kfld                    run_type
002600010806     C                   kfld                    emp_no
002700010806     C                   kfld                    run_date
002800010806     C                   kfld                    trn_code
002900990826
003000090415      //
003100090415      // Get All Transactions
003101090415      /FREE
003200090415       chain trans_key ptisfmt;
003300090415       if %found;
003400090415         dow not(%eof);
003500090415           trn_value = trn_value + ptrnamt;
003600090415           trn_hrs = trn_hrs + ptrnhrs;
003700090415           reade trans_key ptisfmt;
003800090415         enddo;
003900090415       endif;
004000090415       //
004100090415       *inlr = *on;
004200090415       return;
004300010424
004301090415      /END-FREE
