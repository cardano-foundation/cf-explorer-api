package util;
import java.util.Random;

public class RandomNumber {
    public int randomInteger(int from){
        int randomNumber;
        Random random = new Random();
        randomNumber = random.nextInt(from);
        return randomNumber;
    }
}
