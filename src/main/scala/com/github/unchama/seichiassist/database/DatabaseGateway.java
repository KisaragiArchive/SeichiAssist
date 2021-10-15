package com.github.unchama.seichiassist.database;

import com.github.unchama.seichiassist.SeichiAssist;
import com.github.unchama.seichiassist.database.manipulators.DonateDataManipulator;
import com.github.unchama.seichiassist.database.manipulators.GachaDataManipulator;
import com.github.unchama.seichiassist.database.manipulators.MineStackGachaDataManipulator;
import com.github.unchama.seichiassist.database.manipulators.PlayerDataManipulator;
import com.github.unchama.util.ActionStatus;
import com.github.unchama.util.failable.FailableAction;
import com.github.unchama.util.failable.Try;
import com.github.unchama.util.unit.Unit;
import org.jetbrains.annotations.NotNull;

import java.sql.*;

import static com.github.unchama.util.ActionStatus.Fail;
import static com.github.unchama.util.ActionStatus.Ok;

/**
 * データベースとのデータをやり取りするためのゲートウェイとして機能するオブジェクトのクラス
 */
public class DatabaseGateway {
    //TODO: 直接SQLに変数を連結しているが、順次PreparedStatementに置き換えていきたい

    // TODO これらはこのクラスに入るべきではなさそう(プラグインクラスに入れるべき)
    public final PlayerDataManipulator playerDataManipulator;
    public final GachaDataManipulator gachaDataManipulator;
    public final MineStackGachaDataManipulator mineStackGachaDataManipulator;
    public final DonateDataManipulator donateDataManipulator;
    private @NotNull
    final String databaseUrl;
    private @NotNull
    final String loginId;
    private @NotNull
    final String password;
    public Connection con = null;
    private Statement stmt = null;

    private final SeichiAssist plugin = SeichiAssist.instance();

    private DatabaseGateway(@NotNull String databaseUrl, @NotNull String loginId, @NotNull String password) {
        this.databaseUrl = databaseUrl;
        this.loginId = loginId;
        this.password = password;

        this.playerDataManipulator = new PlayerDataManipulator();
        this.gachaDataManipulator = new GachaDataManipulator();
        this.mineStackGachaDataManipulator = new MineStackGachaDataManipulator();
        this.donateDataManipulator = new DonateDataManipulator();
    }

    public static DatabaseGateway createInitializedInstance(@NotNull String databaseUrl,
                                                            @NotNull String loginId,
                                                            @NotNull String password) {
        final DatabaseGateway instance = new DatabaseGateway(databaseUrl, loginId, password);

        if (instance.connectToDatabase() == Fail) {
            instance.plugin.getLogger().info("データベース初期処理にエラーが発生しました");
        }

        return instance;
    }

    private ActionStatus createDatabaseDriverInstance() {
        try {
            Class.forName("com.mysql.jdbc.Driver").newInstance();
            return Ok;
        } catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
            e.printStackTrace();
            return Fail;
        }
    }

    private ActionStatus establishMySQLConnection() {
        try {
            if (stmt != null && !stmt.isClosed()) {
                stmt.close();
                con.close();
            }
            con = DriverManager.getConnection(databaseUrl, loginId, password);
            stmt = con.createStatement();
            return Ok;
        } catch (SQLException e) {
            e.printStackTrace();
            return Fail;
        }
    }

    /**
     * コネクションの確認及び復旧を試みる
     * @throws RuntimeException 処理中にSQLExceptionがthrowされたとき
     */
    public void ensureConnection() {
        try {
            if (con.isClosed()) {
                plugin.getLogger().warning("sqlConnectionクローズを検出。再接続試行");
                con = DriverManager.getConnection(databaseUrl, loginId, password);
            }
            if (stmt.isClosed()) {
                plugin.getLogger().warning("sqlStatementクローズを検出。再接続試行");
                stmt = con.createStatement();
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 接続関数
     */
    private ActionStatus connectToDatabase() {
        return Try
                .sequence(
                        new FailableAction<>(
                                "Mysqlドライバーのインスタンス生成に失敗しました",
                                this::createDatabaseDriverInstance
                        ),
                        new FailableAction<>("SQL接続に失敗しました", this::establishMySQLConnection)
                )
                .mapFailed(failedMessage -> {
                    plugin.getLogger().info(failedMessage);
                    return Unit.instance;
                })
                .overallStatus();
    }

    /**
     * コネクション切断処理
     *
     * @return 成否
     */
    public ActionStatus disconnect() {
        if (con != null) {
            try {
                stmt.close();
                con.close();
            } catch (SQLException e) {
                e.printStackTrace();
                return Fail;
            }
        }
        return Ok;
    }
}
