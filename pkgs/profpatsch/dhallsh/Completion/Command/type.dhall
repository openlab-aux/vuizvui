  λ(a : Type)
→ { name :
      Text
  , description :
      Text
  , options :
      List ../Option/type.dhall
  , subcommands :
      List a
  }
