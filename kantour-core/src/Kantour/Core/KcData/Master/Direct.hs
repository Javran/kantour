{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct (
  module Kantour.Core.KcData.Master.Direct.Root,
  module Kantour.Core.KcData.Master.Direct.Slotitem,
  module Kantour.Core.KcData.Master.Direct.Shipgraph,
  module Kantour.Core.KcData.Master.Direct.Ship,
  module Kantour.Core.KcData.Master.Direct.Bgm,
  module Kantour.Core.KcData.Master.Direct.ItemShop,
  module Kantour.Core.KcData.Master.Direct.EquipExslotShip,
  module Kantour.Core.KcData.Master.Direct.EquipShip,
  module Kantour.Core.KcData.Master.Direct.Furniture,
  module Kantour.Core.KcData.Master.Direct.Furnituregraph,
  module Kantour.Core.KcData.Master.Direct.Maparea,
  module Kantour.Core.KcData.Master.Direct.Mapbgm,
  module Kantour.Core.KcData.Master.Direct.Mapinfo,
  module Kantour.Core.KcData.Master.Direct.Mission,
  module Kantour.Core.KcData.Master.Direct.Payitem,
  module Kantour.Core.KcData.Master.Direct.Shipupgrade,
  module Kantour.Core.KcData.Master.Direct.SlotitemEquiptype,
  module Kantour.Core.KcData.Master.Direct.Stype,
  module Kantour.Core.KcData.Master.Direct.Useitem,
) where

{-
  All modules under this one are meant for a direct representation
  of api_start2 of the game.

  It's true that Aeson's dynamic representation is already a direct one,
  but the lack of ways to detect unknown fields without failure is one of
  its limitations [1]. `Direct` modules add to that by providing a way
  to confirm that we only have fields that we already know and warn us
  when unknown fields come up.

  1: https://github.com/haskell/aeson/issues/808

  (Below are all TODOs)

  `Direct` modules will eventually be organized into corresponding
  `Org` modules. Note that only `Direct` layer are responsible
  for verifying that the JSON data we are getting are well-formed,
  beyond which point we just assume type system will do the right thing.

  Planned features of `Org` modules:

  - better field names (without those mst_, api_ etc. all over the place).
  - dictionary-like containers with related ids as keys.

 -}

import Kantour.Core.KcData.Master.Direct.Bgm
import Kantour.Core.KcData.Master.Direct.EquipExslotShip
import Kantour.Core.KcData.Master.Direct.EquipShip
import Kantour.Core.KcData.Master.Direct.Furniture
import Kantour.Core.KcData.Master.Direct.Furnituregraph
import Kantour.Core.KcData.Master.Direct.ItemShop
import Kantour.Core.KcData.Master.Direct.Maparea
import Kantour.Core.KcData.Master.Direct.Mapbgm
import Kantour.Core.KcData.Master.Direct.Mapinfo
import Kantour.Core.KcData.Master.Direct.Mission
import Kantour.Core.KcData.Master.Direct.Payitem
import Kantour.Core.KcData.Master.Direct.Root
import Kantour.Core.KcData.Master.Direct.Ship
import Kantour.Core.KcData.Master.Direct.Shipgraph
import Kantour.Core.KcData.Master.Direct.Shipupgrade
import Kantour.Core.KcData.Master.Direct.Slotitem
import Kantour.Core.KcData.Master.Direct.SlotitemEquiptype
import Kantour.Core.KcData.Master.Direct.Stype
import Kantour.Core.KcData.Master.Direct.Useitem
