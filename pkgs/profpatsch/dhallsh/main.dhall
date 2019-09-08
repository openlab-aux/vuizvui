let Prelude =
	  https://prelude.dhall-lang.org/package.dhall sha256:2acd9f8eae045eae46d8288d76b01678c4ac4883a58eadb6be0da00b3ba590cf

let List/filterOptional = ./List/filterOptional.dhall

let Command = ./Command/type.dhall

let Option = ./Option/type.dhall

let Argument = ./Argument/type.dhall

let Complete =
	  { cmd :
		  Text
	  , description :
		  Text
	  , condition :
		  Optional Text
	  , short-option :
		  Optional Text
	  , long-option :
		  Optional Text
	  , long-option-old-style :
		  Optional Text
	  , arguments :
		  Optional Text
	  , keep-order :
		  Bool
	  , no-files :
		  Bool
	  , require-parameter :
		  Bool
	  , wraps :
		  Optional Text
	  }

let complete =
		λ(a : { cmd : Text, description : Text })
	  → { cmd =
			a.cmd
		, description =
			a.description
		, condition =
			None Text
		, short-option =
			None Text
		, long-option =
			None Text
		, long-option-old-style =
			None Text
		, arguments =
			None Text
		, keep-order =
			False
		, no-files =
			False
		, require-parameter =
			False
		, wraps =
			None Text
		}

let completeToCommand
	: Complete → Command Argument
	= let l = λ(o : Text) → Option.Long o

	  in    λ(c : Complete)
		  → let long =
					λ(name : Text)
				  → λ(content : Text)
				  → Argument.Option { opt = Option.Long name, arg = content }

			let args =
					[ Some (long "description" c.description)
					, Prelude.Optional.map
					  Text
					  Argument
					  (long "condition")
					  c.condition
					]
				  : List (Optional Argument)

			let id = λ(a : Optional Argument) → a

			in  { cmd =
					c.cmd
				, args =
					  List/filterOptional (Optional Argument) Argument id args
					: List Argument
				}

in  let foo =
		  completeToCommand (complete { cmd = "complete", description = "foo" })

	in  [   [ foo.cmd ]
		  # Prelude.List.concatMap
			Argument
			Text
			(./Argument/toArgList.dhall ./OptionPrinter/newStyle.dhall)
			foo.args
		]
