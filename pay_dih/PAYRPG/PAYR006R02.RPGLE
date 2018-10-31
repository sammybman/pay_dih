000100171114     H DATEDIT(*ymd)  alwnull(*usrctl)
000200090415      // PAYSHEET MAINTENANCE - Global Changes
000300090415      // PROGRAMMER:-S. Britton & C. Williams
000400020115     Fempmast   ip   e           K disk
000500020115     Fpaytimfl01uf a e           K disk
000600090415      // Payroll PROTOTYPES
000700151009      /copy *libl/qcpysrc,PAYPR
000800090415      //
000900020111     D run_type        s              1                                         payroll run type
001000020111     D trn_type        s              2  0                                      transaction code
001100020111     D trn_code        s              1                                         transaction code
001200010806     D trn_value       s             11  2                                      total amount
001300020115     D trn_list        s             15                                         total amount
001400040717     D trn_cnt         s              6  0                                      total amount
001500040717     D trn_cls         s              3                                         total amount
001600171114
001700010806     C     *entry        plist
001800010806     C                   parm                    run_type
001900020111     C                   parm                    trn_type
002000020111     C                   parm                    trn_code
002100010806     C                   parm                    trn_value
002200020115     C                   parm                    trn_list
002300040717     C                   parm                    trn_cnt
002400040717     C                   parm                    trn_cls
002500090415      //
002600020111     c     trn_key       klist
002700010806     C                   kfld                    run_type
002800020111     C                   kfld                    trn_type
002900020115     C                   kfld                    emp
003000990826
003100090415      // Select Valid employee and status
003200171114
003300090415       if get_cat(status) = 'A'
003400090415             and %scan(status:trn_list) > 0;
003500090415         // include Valid employee class if option is selected
003600090415         if trn_cls = *blanks or
003700090415               trn_cls <> *blanks and
003800090415               trn_cls = class;
003900020115
004000171114           timamt = *zeros;
004100090415           chain trn_key timfmt;
004200171114
004300090415           select;
004400090415           when trn_code = '+';
004500090415             timamt += trn_value;
004600090415           when trn_code = '-';
004700090415             timamt -= trn_value;
004800090415             // cannot change to less than 0
004900090415             if timamt < *zeros;
005000090415               timamt = *zeros;
005100090415             endif;
005200090415           when trn_code = 'C';
005300090415             timamt = trn_value;
005400090415           endsl;
005500040717
005600090415           trn_cnt += 1;
005700020115
005800090415           if %found;
005900090415             //
006000090415             update timfmt;
006100090415           else;
006200090415             timrtype = run_type;
006300090415             timcode = 'E';
006400090415             timprior = *zeros;
006500090415             timtyp   = trn_type;
006600090415             timemp  = emp;
006700090415             timhrs  = *zeros;
006800090415             //
006900090415             write timfmt;
007000090415           endif;
007100020111
007200090415         endif;
007300090415       endif;
007400010424
007500090415      /END-FREE
