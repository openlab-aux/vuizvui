let Prelude = ./imports/Prelude.dhall

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
		  Optional (Command Argument)
	  , short-option :
		  Optional Text
	  , long-option :
		  Optional Text
	  , long-option-old-style :
		  Optional Text
	  , argument :
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

let argCommandToList
	: Command Argument → List Text
	= ./Command/toList.dhall
	  Argument
	  (./Argument/toArgList.dhall ./OptionPrinter/newStyle.dhall)

let complete =
		λ(a : { cmd : Text, description : Text })
	  → { cmd =
			a.cmd
		, description =
			a.description
		, condition =
			None (Command Argument)
		, short-option =
			None Text
		, long-option =
			None Text
		, long-option-old-style =
			None Text
		, argument =
			None Text
		, keep-order =
			False
		, no-files =
			True
		, require-parameter =
			False
		, wraps =
			None Text
		}

let completeToCommand
	: Complete → Command Argument
	=   λ(c : Complete)
	  → let long =
				λ(name : Text)
			  → Prelude.Optional.map
				Text
				Argument
				(   λ(content : Text)
				  → Argument.Option { opt = Option.Long name, arg = content }
				)

		let flag =
				λ(name : Text)
			  → λ(flag : Bool)
			  →       if flag

				then  Some (Argument.Flag (Option.Long name))

				else  None Argument

		let conditionToText =
				λ(c : Command Argument)
			  → Prelude.Text.concatSep " " (argCommandToList c)

		let args =
				[ long "command" (Some c.cmd)
				, long "description" (Some c.description)
				, Prelude.Optional.map
				  (Command Argument)
				  Argument
				  (   λ(c : Command Argument)
					→ Argument.Option
					  { opt = Option.Long "condition", arg = conditionToText c }
				  )
				  c.condition
				, long "short-option" c.short-option
				, long "long-option" c.long-option
				, long "old-option" c.long-option-old-style
				, long "arguments" c.argument
				, long "wraps" c.wraps
				, flag "keep-order" c.keep-order
				, flag "no-files" c.no-files
				, flag "require-parameter" c.require-parameter
				]
			  : List (Optional Argument)

		let id = λ(a : Optional Argument) → a

		in  { cmd =
				"complete"
			, args =
				  List/filterOptional (Optional Argument) Argument id args
				: List Argument
			}

in  let fishSeenSubcommandFn = "__fish_seen_subcommand_from"

	let fishUseSubcommandFn = "__fish_use_subcommand"

	let foo
		: Command Argument
		= completeToCommand
		  (   complete { cmd = "abc", description = "this is foo option" }
			⫽ { condition =
				  Some { cmd = fishUseSubcommandFn, args = [] : List Argument }
			  , argument =
				  Some "foo"
			  }
		  )

	in    [ argCommandToList foo, [ "complete", "--do-complete=abc " ] ]
		: List (List Text)
