package com.github.unchama.seichiassist.effect

import org.bukkit.Location
import org.bukkit.block.Block

/**
 * [location]を中心としたチェビシェフ距離が[distance]以下の領域に
 * [matchAgainst]のブロックが一つでも含まれているかを返す
 */
fun containsBlockAround(location: Location, distance: Int, matchAgainst: Set<Block>): Boolean {
  val cuboidToLookFor =
      AxisAlignedCuboid(XYZTuple(-distance, -distance, -distance), XYZTuple(distance, distance, distance))

  cuboidToLookFor.forEachGridPoint { (x, y, z) ->
    if (matchAgainst.contains(location.block.getRelative(x, y, z))) return true
  }

  return false
}