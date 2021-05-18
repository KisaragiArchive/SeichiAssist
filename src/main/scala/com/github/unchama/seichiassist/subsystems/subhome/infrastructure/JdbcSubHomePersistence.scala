package com.github.unchama.seichiassist.subsystems.subhome.infrastructure

import cats.effect.Sync
import com.github.unchama.seichiassist.SeichiAssist
import com.github.unchama.seichiassist.subsystems.subhome.domain.{SubHomeId, SubHomeLocation, SubHomePersistence, SubHome}
import scalikejdbc._

import java.util.UUID

class JdbcSubHomePersistence[F[_]: Sync] extends SubHomePersistence[F] {
  private val serverId = SeichiAssist.seichiAssistConfig.getServerNum

  override def upsert(ownerUuid: UUID, id: SubHomeId)(subHome: SubHome): F[Unit] = {
    Sync[F].delay {
      DB.readOnly { implicit session =>
        val SubHomeLocation(worldName, x, y, z) = subHome.location

        // NOTE 2021/05/19: 何故かDB上のIDは1少ない。つまり、ID 1のサブホームはDB上ではid=0である。
        sql"""insert into seichiassist.sub_home
             |(player_uuid, server_id, id, name, location_x, location_y, location_z, world_name) values
             |  (${ownerUuid.toString}, $serverId, ${id.value - 1}, ${subHome.name.orNull}, $x, $y, $z, $worldName)
             |    on duplicate key update
             |      name = values(name),
             |      location_x = values(location_x),
             |      location_y = values(location_y),
             |      location_z = values(location_z),
             |      world_name = values(world_name)"""
          .update()
          .apply()
      }
    }
  }

  override def list(ownerUuid: UUID): F[Map[SubHomeId, SubHome]] = Sync[F].delay {
    DB.readOnly { implicit session =>
      // NOTE 2021/05/19: 何故かDB上のIDは1少ない。つまり、ID 1のサブホームはDB上ではid=0である。
      sql"""SELECT id, name, location_x, location_y, location_z, world_name FROM seichiassist.sub_home"""
        .map(rs => (SubHomeId(rs.int("id") + 1), extractSubHome(rs)))
        .list().apply()
    }.toMap
  }

  private def extractSubHome(rs: WrappedResultSet): SubHome =
    SubHome(
      rs.stringOpt("name"),
      SubHomeLocation(
        rs.string("world_name"),
        rs.int("location_x"),
        rs.int("location_y"),
        rs.int("location_z")
      )
    )
}
