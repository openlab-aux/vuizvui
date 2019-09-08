let Option = ../Option/type.dhall

let newStyle
	: Option → Text
	=   λ(o : Option)
	  → merge { Long = λ(t : Text) → "--${t}", Short = λ(t : Text) → "-${t}" } o

in  newStyle
