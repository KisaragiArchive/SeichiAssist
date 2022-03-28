package com.github.unchama.seichiassist.commands

import cats.effect.IO
import com.github.unchama.contextualexecutor.ContextualExecutor
import com.github.unchama.contextualexecutor.builder.ParserResponse.{failWith, succeedWith}
import com.github.unchama.contextualexecutor.executors.BranchedExecutor
import com.github.unchama.menuinventory.router.CanOpen
import com.github.unchama.seichiassist.SeichiAssist
import com.github.unchama.seichiassist.commands.contextual.builder.BuilderTemplates.playerCommandBuilder
import com.github.unchama.seichiassist.menus.minestack.CategorizedMineStackMenu
import com.github.unchama.seichiassist.minestack.MineStackObjectCategory
import com.github.unchama.targetedeffect.{SequentialEffect, UnfocusedEffect}
import com.github.unchama.targetedeffect.commandsender.MessageEffect
import org.bukkit.command.TabExecutor

object MineStackCommand {
  def executor(
    implicit ioCanOpenCategorizedMenu: IO CanOpen CategorizedMineStackMenu
  ): TabExecutor =
    BranchedExecutor(
      Map(
        "on" -> ChildExecutors.setAutoCollectionExecutor(true),
        "off" -> ChildExecutors.setAutoCollectionExecutor(false),
        "open" -> ChildExecutors.openCategorizedMineStackMenu
      )
    ).asNonBlockingTabExecutor()

  object ChildExecutors {

    def setAutoCollectionExecutor(autoMineStack: Boolean): ContextualExecutor =
      playerCommandBuilder
        .execution { context =>
          IO {
            val sender = context.sender
            val pd = SeichiAssist.playermap(sender.getUniqueId).settings
            SequentialEffect(
              UnfocusedEffect {
                pd.autoMineStack = autoMineStack
              },
              if (autoMineStack)
                MessageEffect("mineStack自動収集をonにしました。")
              else
                MessageEffect("mineStack自動収集をoffにしました。")
            )
          }
        }
        .build()

    def openCategorizedMineStackMenu(
      implicit ioCanOpenCategorizedMenu: IO CanOpen CategorizedMineStackMenu
    ): ContextualExecutor =
      playerCommandBuilder
        .argumentsParsers(
          List(
            category => {
              category.toIntOption match {
                case Some(categoryValue) =>
                  MineStackObjectCategory.fromSerializedValue(categoryValue - 1) match {
                    case Some(category) => succeedWith(category)
                    case None           => failWith("指定されたカテゴリは存在しません。")
                  }
                case None => failWith("カテゴリは数字で入力してください。")
              }
            },
            page => {
              page.toIntOption match {
                case Some(pageNum) =>
                  if (pageNum <= 0) {
                    failWith("ページ数は正の値を指定してください。")
                  } else {
                    succeedWith(page)
                  }
                case None =>
                  failWith("ページ数は数字で入力してください。")
              }
            }
          )
        )
        .execution { context =>
          val args = context.args.parsed
          val _page = args(1).toString.toInt - 1
          val _category = args.head
          IO.pure(
            ioCanOpenCategorizedMenu.open(
              new CategorizedMineStackMenu(
                _category.asInstanceOf[MineStackObjectCategory],
                _page
              )
            )
          )
        }
        .build()

  }

}
