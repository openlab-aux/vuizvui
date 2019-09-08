let filterOptional
	: ∀(a : Type) → ∀(b : Type) → (a → Optional b) → List a → List b
	=   λ(a : Type)
	  → λ(b : Type)
	  → λ(f : a → Optional b)
	  → λ(l : List a)
	  → List/build
		b
		(   λ(list : Type)
		  → λ(cons : b → list → list)
		  → λ(nil : list)
		  → List/fold
			a
			l
			list
			(   λ(x : a)
			  → λ(xs : list)
			  → Optional/fold b (f x) list (λ(opt : b) → cons opt xs) xs
			)
			nil
		)

in  filterOptional
