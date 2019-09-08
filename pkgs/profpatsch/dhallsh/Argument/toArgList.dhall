let OptionPrinter = ../OptionPrinter/type.dhall

let Option = ../Option/type.dhall

let Argument = ../Argument/type.dhall

in    λ(optionPrinter : OptionPrinter)
	→ λ(a : Argument)
	→ merge
	  { Plain =
		  λ(t : Text) → [ t ]
	  , Flag =
		  λ(o : Option) → [ optionPrinter o ]
	  , Option =
		  λ(o : { opt : Option, arg : Text }) → [ optionPrinter o.opt, o.arg ]
	  }
	  a
	  : List Text
