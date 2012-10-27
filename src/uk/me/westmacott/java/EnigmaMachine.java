package uk.me.westmacott.java;

public class EnigmaMachine {
	
	private static final String alphabet = "abcdefghijklmnopqrstuvwxyz";
	
	private int[] reflector = new int[]{24, 17, 20, 7, 16, 18, 11, 3, 15, 23, 13, 6, 14, 10, 12, 8, 4, 1, 5, 25, 2, 22, 21, 9, 0, 19};
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
			rotateRotors();
			output += encodeChar(character);
		}
		return output;
	}

	private void rotateRotors() {
		rotor1.rotateIf(rotor1.notch() || rotor2.notch());
		rotor2.rotateIf(rotor2.notch() || rotor3.notch());
		rotor3.rotateIf(rotor3.notch() || true);
	}
	
	private char encodeChar(char character) {
		int position = alphabet.indexOf(character);
		position = rotor3.forwardPass(position);
		position = rotor2.forwardPass(position);
		position = rotor1.forwardPass(position);
		position = reflector[position];
		position = rotor1.reversePass(position);
		position = rotor2.reversePass(position);
		position = rotor3.reversePass(position);
		return alphabet.charAt(position);
	}

}
