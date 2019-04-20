package com.github.unchama.seichiassist.data;

import java.util.Objects;

public class RankData {
	public String name;
	public int level;
	public long totalbreaknum;
	//追加ランキングデータ
	public int playtick;
	public int p_vote;
	public int premiumeffectpoint;
	public int p_apple;

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;
		RankData rankData = (RankData) o;
		return level == rankData.level &&
				totalbreaknum == rankData.totalbreaknum &&
				playtick == rankData.playtick &&
				p_vote == rankData.p_vote &&
				premiumeffectpoint == rankData.premiumeffectpoint &&
				p_apple == rankData.p_apple &&
				Objects.equals(name, rankData.name);
	}

	@Override
	public int hashCode() {
		return Objects.hash(name, level, totalbreaknum, playtick, p_vote, premiumeffectpoint, p_apple);
	}

	public RankData(){
		name = null;
		level = 1;
		totalbreaknum = 0;
	}
}
