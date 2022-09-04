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

  (TODO) The future plan is to have a more friendly representation
  built on top of this one. This may include:

  - better field names (without those mst_, api_ etc. all over the place).
  - dictionary-like containers with related ids as keys.

  (TODO) on a second thought, the Aeson Object *is* a direct representation
  already, so this "Direct" layer probably serves no purpose - why don't
  we just re-organize data from the Aeson representation?

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
