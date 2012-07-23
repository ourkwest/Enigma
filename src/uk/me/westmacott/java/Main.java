package uk.me.westmacott.java;

public class Main {

	public static void main(String[] args) {
		System.out.println(createMachine().encode("enigmarevealed"));
		System.out.println(createMachine().encode("qmjidomzwzsfjr"));
	}

	private static EnigmaMachine createMachine() {
		Rotor rotor1 = new Rotor("ekmflgdqvzntowyhxuspaibrcj", 16, 'm');
		Rotor rotor2 = new Rotor("ajdksiruxblhwtmcqgznpyfvoe",  4, 'c');
		Rotor rotor3 = new Rotor("bdfhjlcprtxvznyeiwgakmusqo", 21, 'k');
		EnigmaMachine myMachine = new EnigmaMachine(rotor1, rotor2, rotor3);
		return myMachine;
	}

}
