package uk.me.westmacott.java;

public class Rotor {
	
	private static final String outputs = "abcdefghijklmnopqrstuvwxyz";
	private static final int radius = 26;
	private final String inputs;
	private final int notch;
	private int displacement;
	
	public Rotor(String inputs, int notch, char displacement) {
		this.inputs = inputs;
		this.notch = notch;
		this.displacement = outputs.indexOf(displacement);
	}
	
	private int pass(int position, String from, String to) {
		int rotatedPosition = (position + displacement) % radius;
		char character = from.charAt(rotatedPosition);
		int rotatedOutput = to.indexOf(character);
		int output = (rotatedOutput - displacement + radius) % radius;
		return output;
	}
	
	public int forwardPass(int position) {
		return pass(position, inputs, outputs);
	}
	
	public int reversePass(int position) {
		return pass(position, outputs, inputs);
	}
	
	public void rotateIf(boolean shouldRotate) {
		if (!shouldRotate) {
			return;
		}
		displacement = (displacement + 1) % radius;
	}
	
	public boolean notch() {
		return notch == displacement;
	}
	
}
