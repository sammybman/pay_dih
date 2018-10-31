000100950405     H DECEDIT('.') DATEDIT(*DMY/)
000200170220     H alwnull(*usrctl)
000300090415      //*
000400090415      //* Regular time & Overtime calculations
000500090415      //* PROGRAMMER----** C.WILLIAMS  ADJ. S.BRITTON
000600090415      //*
000700010614     Fempmast   ip   e           K DISK
000800020111     Fpayrdefl03if   e           K DISK
000900010902     Fpaytimfl01uf a e           K DISK    commit
001000010618     Ftimecrdf  if   e           K DISK
001100010614     Fpaycycd   if   e           K DISK
001200090415      // Payroll PROTOTYPES
001300150324      /copy qcpysrc,PAYPR
001400150324      /copy qcpysrc,GENPR
001500150324      /copy qcpysrc,emppr
001600090415      //
001700010426     Drun_type         s              1
001800020304     Drun_qty          s              2  0
001900010618     Drun_cycle        s              2
002000010618     Dpayrun_factor    s              7  3
002100090415      //
002200010618     C     get_key       klist
002300010816     C                   kfld                    run_type
002400010614     C                   kfld                    prdftype
002500010614     C                   kfld                    emp
002600010618
002700090415      /FREE
002800090415       chain run_type prdffmt;
002900090415       *IN99 = NOT %FOUND;
003000090415       dow not(*in99);
003100090415
003200090415          // get payroll type cycle
003300090415          get_payty_info('RD':prdftype:payty_struc);
003400050519
003500010616
003600090415         chain get_key timcfmt;
003700090415
003800090415         timamt = get_emp_pay(emp:dept:prdftype:
003900090415             timc_hrs:payrun_factor:
004000090415             run_qty:status);
004100010614
004200090415         // Write Info to work file
004300090415         if timamt <> *zeros
004400090415            and ptypaytyp <> 'S';
004500090415           timemp = emp;
004600090415           timcode = ptyrtype;
004700090415           timrtype = run_type;
004800090415           timtyp = ptyptype;
004900090415           timhrs = timc_hrs;
005000090415           timprior = ptyprior;
005100090415           timamt = round_up(timamt:'U');
005200090415           write timfmt;
005300090415         endif;
005400090415
005500090415         clear timfmt;
005600090415         clear timc_hrs;
005700010426
005800090415         reade run_type prdffmt;
005900090415         *IN99 = %EOF;
006000090415       enddo;
006100090415       //
006200090415       begsr *inzsr;
006300090415         //-------------------------------------------------------------------------
006400090415      /END-FREE
006500020304     C     *entry        plist
006600020304     C                   parm                    run_type
006700020304     C                   parm                    run_qty
006800020304     C                   parm                    run_cycle
006900090415       // get Run-type cycle
007000090415      /FREE
007100090415         chain run_cycle cycfmt;
007200090415         payrun_factor = cycyearf;
007300090415       endsr;
007400010626
007500090415      /END-FREE
