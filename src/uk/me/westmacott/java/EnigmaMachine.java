package uk.me.westmacott.java;

public class EnigmaMachine {
	
	private static final String alphabet = "abcdefghijklmnopqrstuvwxyz";
	
	private int[] reflector = new int[]{1, 2, 3, 4, 5, 6};
	private Rotor rotor1;
	private Rotor rotor2;
	private Rotor rotor3;

	public EnigmaMachine(Rotor rotor1, Rotor rotor2, Rotor rotor3) {
		this.rotor1 = rotor1;
		this.rotor2 = rotor2;
		this.rotor3 = rotor3;
	}
	
	public String encode(String text) {
		String output = "";
		for (char character : text.toCharArray()) {
			if (rotor3.rotate() && rotor2.rotate() && rotor1.rotate()) { /* Evaluated for side-effects! */ };
			int position = alphabet.indexOf(character);
			position = rotor3.forwardPass(position);
			position = rotor2.forwardPass(position);
			position = rotor1.forwardPass(position);
			position = reflect(position);
			position = rotor1.reversePass(position);
			position = rotor2.reversePass(position);
			position = rotor3.reversePass(position);
			output += alphabet.charAt(position);
		}
		return output;
	}

	private int reflect(int position) {
		return reflector[position];
	}

}
