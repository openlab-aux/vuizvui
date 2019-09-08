let Prelude = ../imports/Prelude.dhall

let Command = ./type.dhall

let toList
	: ∀(a : Type) → (a → List Text) → Command a → List Text
	=   λ(a : Type)
	  → λ(f : a → List Text)
	  → λ(c : Command a)
	  → [ c.cmd ] # Prelude.List.concatMap a Text f c.args

in  toList
