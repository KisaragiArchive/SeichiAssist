package com.github.unchama.seichiassist.minestack;

import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;

import java.util.Objects;

public class MineStackObj {
	private String objName;
	private int level;
	private int gachaType;
	private int stackType;
	private ItemStack itemStack;
	private boolean nameLoreFlag;

	public MineStackObj(String objName, String japaneseName,
						int level, Material material, int durability,
						boolean nameLoreFlag, int gachaType, int stackType){
		this.objName = objName;
		this.level = level;
		this.nameLoreFlag = nameLoreFlag;
		this.gachaType = gachaType;
		this.itemStack = new ItemStack(material, 1, (short) durability);

		if (japaneseName != null) {
			final ItemMeta meta = this.itemStack.getItemMeta();
			meta.setDisplayName(japaneseName);
			this.itemStack.setItemMeta(meta);
		}

		this.stackType = stackType;
	}

	public MineStackObj(String objName, int level, ItemStack itemStack, boolean nameLoreFlag, int gachaType, int stackType){
		this.objName = objName;
		this.level = level;
		this.nameLoreFlag = nameLoreFlag;
		this.gachaType = gachaType;

		this.itemStack = itemStack.clone();
		this.itemStack.setAmount(1);

		this.stackType = stackType;
	}

	public String getMineStackObjName(){
		return objName;
	}
	public String getJapaneseName(){
		return this.itemStack.getItemMeta().getDisplayName();
	}
	public int getLevel(){
		return level;
	}
	public Material getMaterial(){
		return itemStack.getType();
	}
	public int getDurability(){
		return itemStack.getDurability();
	}
	public boolean getNameLoreFlag(){
		return nameLoreFlag;
	}
	public int getGachaType(){
		return gachaType;
	}

	public ItemStack getItemStack(){
		return itemStack;
	}

	/**
	 * @deprecated use MineStackObj.toType defined with MineStackObjectCategory
	 */
	@Deprecated
	public int getStackType(){
		return stackType;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;
		MineStackObj that = (MineStackObj) o;
		return objName.equals(that.objName);
	}

	@Override
	public int hashCode() {
		return Objects.hash(objName);
	}
}
