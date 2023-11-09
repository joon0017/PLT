package edu.handong.csee.plt.ast;

public class Fun extends AST {
	AST body = new AST();
	String param = "";
	
	public Fun(String param, AST body) {
		this.param = param;
		this.body = body;
	}
	
	public String getParam() {
		return this.param;
	}
	
	public AST getBody() {
		return this.body;
	}

	public String getASTCode() {
		return "(Fun " + param + " " + body.getASTCode() + ")";
	}
}
//[fun (param symbol?) (body FAE?)]