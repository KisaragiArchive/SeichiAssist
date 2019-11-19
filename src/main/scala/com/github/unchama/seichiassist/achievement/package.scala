package com.github.unchama.seichiassist

import cats.effect.IO
import org.bukkit.entity.Player

package object achievement {
  type AchievementId = Int
  type PlayerPredicate = Player => IO[Boolean]
  type ParameterizedText[A] = A => String
}
