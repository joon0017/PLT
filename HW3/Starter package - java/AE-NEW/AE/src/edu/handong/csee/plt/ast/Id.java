package edu.handong.csee.plt.ast;

public class Id extends AST {
	String symb = "";
	
	public Id(String symbol) {
		this.symb = symbol;
	}

	public String getASTCode() {
		return "(id " + symb + ")";
	}
}
//[id (name symbol?)]