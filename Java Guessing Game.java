import java.util.Random;
import java.util.Scanner;

public class GuessingGame {

    private static int P1_BANK = 10, P2_BANK = 10, MAX = 256, MAGIC_NUM = 123;

    public static void main(String[] args) {
        final Scanner console = new Scanner(System.in);
        final int secretNum = new Random().nextInt(MAX);

        balance();
        System.out.print("WALLET 1: please lock the amount ? ");
        int lockValue = console.nextInt();
        P1_BANK -= lockValue;

        while (true) {
            System.out.print("WALLET 2: your guess? ");
            int guess = console.nextInt();

            if (guess == secretNum || guess == MAGIC_NUM) {
                P2_BANK += lockValue;
                break;
            } else if (guess < secretNum) {
                System.out.println("It's higher");
            } else {
                System.out.println("It's lower");
            }
        }

        System.out.println("Congrats, You've Won");
        balance();
    }

    private static void balance() {
        System.out.println("\n----------------------------");
        System.out.println("WALLET 1: " + P1_BANK +  ", WALLET 2: " + P2_BANK);
        System.out.println("----------------------------\n");
    }

}
