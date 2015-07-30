package Tools;

import UserInterface.PlayerInterface;

public class FrologException extends Exception {
	
	public PlayerInterface.Exc type;
	public String message;

	public FrologException(String type, String arg0) {
		super();
		this.type = PlayerInterface.Exc.valueOf(type);
		this.message = arg0;
	}

}
