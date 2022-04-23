-- Adapted (stolen) from https://github.com/commercialhaskell/stack/blob/d8fc7a1344fdd2dd9227f419b0b06ae1e5d4ede6/src/Options/Applicative/Builder/Extra.hs

module Calligraphy.Util.Optparse (boolFlags) where

import Options.Applicative

-- | Enable/disable flags for a 'Bool'.
boolFlags ::
  -- | Default value
  Bool ->
  -- | Flag name
  String ->
  -- | Help suffix
  String ->
  Mod FlagFields Bool ->
  Parser Bool
boolFlags defaultValue name helpText =
  enableDisableFlags defaultValue True False name $
    concat
      [ helpText,
        " (default: ",
        if defaultValue then "enabled" else "disabled",
        ")"
      ]

-- | Enable/disable flags for any type.
enableDisableFlags ::
  -- | Default value
  a ->
  -- | Enabled value
  a ->
  -- | Disabled value
  a ->
  -- | Name
  String ->
  -- | Help suffix
  String ->
  Mod FlagFields a ->
  Parser a
enableDisableFlags defaultValue enabledValue disabledValue name helpText mods =
  enableDisableFlagsNoDefault enabledValue disabledValue name helpText mods
    <|> pure defaultValue

-- | Enable/disable flags for any type, without a default (to allow chaining with '<|>')
enableDisableFlagsNoDefault ::
  -- | Enabled value
  a ->
  -- | Disabled value
  a ->
  -- | Name
  String ->
  -- | Help suffix
  String ->
  Mod FlagFields a ->
  Parser a
enableDisableFlagsNoDefault enabledValue disabledValue name helpText mods =
  last
    <$> some
      ( ( flag'
            enabledValue
            ( hidden
                <> internal
                <> long name
                <> mods
            )
            <|> flag'
              disabledValue
              ( hidden
                  <> internal
                  <> long ("no-" ++ name)
                  <> mods
              )
        )
          <|> flag'
            disabledValue
            ( long ("[no-]" ++ name)
                <> help helpText
                <> mods
            )
      )
