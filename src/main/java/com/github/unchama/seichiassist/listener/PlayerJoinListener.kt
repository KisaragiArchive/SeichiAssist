package com.github.unchama.seichiassist.listener

import com.github.unchama.seichiassist.ActiveSkill
import com.github.unchama.seichiassist.ManagedWorld
import com.github.unchama.seichiassist.SeichiAssist
import com.github.unchama.seichiassist.data.PlayerData
import com.github.unchama.seichiassist.isSeichi
import com.github.unchama.seichiassist.util.Util
import net.coreprotect.model.Config
import net.md_5.bungee.api.ChatColor
import org.bukkit.Material
import org.bukkit.Sound
import org.bukkit.event.EventHandler
import org.bukkit.event.Listener
import org.bukkit.event.player.PlayerChangedWorldEvent
import org.bukkit.event.player.PlayerJoinEvent
import org.bukkit.inventory.ItemStack

class PlayerJoinListener : Listener {
  private val plugin = SeichiAssist.instance
  private val playermap = SeichiAssist.playermap
  private val databaseGateway = SeichiAssist.databaseGateway

  // プレイヤーがjoinした時に実行
  @EventHandler
  fun onplayerJoinEvent(event: PlayerJoinEvent) {
    // プレイヤー取得
    val player = event.player

    // プレイヤーデータ作成
    // 新しく作成したPlayerDataを引数とする
    databaseGateway.playerDataManipulator.loadPlayerData(PlayerData(player))

    // 初見さんへの処理

    if (!player.hasPlayedBefore()) {
      //初見さんであることを全体告知
      Util.sendEveryMessage(ChatColor.LIGHT_PURPLE.toString() + "" + ChatColor.BOLD + player.name + "さんはこのサーバーに初めてログインしました！")
      Util.sendEveryMessage(ChatColor.WHITE.toString() + "webサイトはもう読みましたか？→" + ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "https://www.seichi.network/gigantic")
      Util.sendEverySound(Sound.ENTITY_PLAYER_LEVELUP, 1f, 1f)
      //初見プレイヤーに木の棒、エリトラ、ピッケルを配布
      player.inventory.addItem(ItemStack(Material.STICK))
      player.inventory.addItem(ItemStack(Material.ELYTRA))
      player.inventory.addItem(ItemStack(Material.DIAMOND_PICKAXE))
      player.inventory.addItem(ItemStack(Material.DIAMOND_SPADE))

      player.inventory.addItem(ItemStack(Material.LOG, 64, 0.toShort()),
          ItemStack(Material.LOG, 64, 0.toShort()),
          ItemStack(Material.LOG, 64, 2.toShort()),
          ItemStack(Material.LOG_2, 64, 1.toShort()))

      /* 期間限定ダイヤ配布.期間終了したので64→32に変更して恒久継続 */
      player.inventory.addItem(ItemStack(Material.DIAMOND, 32))

      player.sendMessage("初期装備を配布しました。Eキーで確認してネ")
      //メビウスおひとつどうぞ
      MebiusListener.give(player)
      //初見さんにLv1メッセージを送信
      player.sendMessage(SeichiAssist.seichiAssistConfig.getLvMessage(1))
    }

  }

  // プレイヤーがワールドを移動したとき
  @EventHandler
  fun onPlayerChangedWorld(event: PlayerChangedWorldEvent) {
    // 整地ワールドから他のワールドに移動したとき
    if (ManagedWorld.fromBukkitWorld(event.from)?.isSeichi == true) {
      val p = event.player
      val pd = playermap[p.uniqueId]!!

      // coreprotectを切る
      // inspectマップにtrueで登録されている場合
      if (Config.inspecting[p.name] != null && (Config.inspecting[p.name] == true)) {
        // falseに変更する
        p.sendMessage("§3CoreProtect §f- Inspector now disabled.")
        Config.inspecting[p.name] = false
      }

      // アサルトスキルを切る
      // 現在アサルトスキルorアサルトアーマーを選択中
      if (pd.activeskilldata.assaultnum >= 4 && pd.activeskilldata.assaulttype >= 4) {
        // アクティブスキルがONになっている
        if (pd.activeskilldata.mineflagnum != 0) {
          // メッセージを表示
          p.sendMessage(ChatColor.GOLD.toString() + ActiveSkill.getActiveSkillName(pd.activeskilldata.assaulttype, pd.activeskilldata.assaultnum) + "：OFF")
          // 内部状態をアサルトOFFに変更
          pd.activeskilldata.updateAssaultSkill(p, pd.activeskilldata.assaulttype, pd.activeskilldata.assaultnum, 0)
          // トグル音を鳴らす
          p.playSound(p.location, Sound.BLOCK_LEVER_CLICK, 1f, 1f)
        }
      }
    }
  }
}
