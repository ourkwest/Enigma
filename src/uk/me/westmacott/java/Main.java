package uk.me.westmacott.java;

public class Main {

	public static void main(String[] args) {
		
		Rotor rotor1 = new Rotor("asdfg...", 5, 'm');
		Rotor rotor2 = new Rotor("asdfg...", 5, 'm');
		Rotor rotor3 = new Rotor("asdfg...", 5, 'm');
		
		EnigmaMachine myMachine = new EnigmaMachine(rotor1, rotor2, rotor3);
		System.out.println(myMachine.encode("enigmarevealed"));

	}

}
