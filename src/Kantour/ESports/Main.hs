module Kantour.ESports.Main where

import Kantour.Subcommand

data SubCmdESports

instance Subcommand SubCmdESports where
    name _ = "ESports"
    main _ = defaultMain

defaultMain :: IO ()
defaultMain = pure ()

{-

  (TODO) plan:

  - Make `QuestState`, which keeps track of completed, ongoing quests,
    ensure that quest dependencies are taking into account properly

  - Use progressors to make progress until we can do nothing more

    - some optimization could help, for example:

      - exclude those that does not make any progress on ongoing quests
      - not sure how effective this would be, but it might be a good idea
        to further exclude those that obviously less efficient than others
        when the set of ongoing quests are taken into account
  - "String" could be inefficient, what we need are just Int for
    fast comparison, might worry about this later

-}
