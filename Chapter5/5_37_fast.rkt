(save env)
(assign proc (op lookup-variable-value) (const x) (reg env))
(assign val (op lookup-variable-value) (const y) (reg env))
(assign arg1 (op list) (reg val))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch9))
compiled-branch10
(assign continue (label after-call11))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch9
(assign val (op apply-primitive-procedure) (reg proc) (reg arg1))
after-call11
(restore env)
(test (op false?) (reg val))
(branch (label false-branch7))
true-branch6
(assign proc (op lookup-variable-value) (const x) (reg env))
(assign val (const 2))
(assign arg1 (op list) (reg val))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch12))
compiled-branch13
(assign continue (label after-if8))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch12
(assign val (op apply-primitive-procedure) (reg proc) (reg arg1))
(goto (label after-if8))
after-call14
false-branch7
(assign val (op lookup-variable-value) (const y) (reg env))
after-if8
