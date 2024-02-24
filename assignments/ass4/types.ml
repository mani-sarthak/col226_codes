type program = clause list

and clause = Fact of predicate | Rule of predicate * body

and body = AtomicFormula of predicate
         | Sequence of body * body
         | Parallel of body * body

and predicate = Pred of string * term list

and term = Variable of string
         | Constant of string
         | Function of string * term list
