import java.util.Random;

public class User<T extends GF> {
    private long secret;
    private T publicKey;
    private T privateKey;
    private boolean keySet;

    public User(DHSetup<T> setup) {
        keySet = false;
        setSecret(setup);
        setPublicKey(setup);
    }

    private void setSecret(DHSetup<T> setup) {
        Random random = new Random();
        secret = random.nextInt((int)(setup.p - 2)) + 2;
    }

    private void setPublicKey(DHSetup<T> setup) {
        publicKey = setup.getGenerator();
        publicKey = setup.power(publicKey, Long.valueOf(secret));
    }

    public T getPublicKey() {
        return publicKey;
    }

    public void setKey(T a, DHSetup<T> setup) {
        privateKey = setup.power(a, Long.valueOf(secret));
        keySet = true;
    }

    public T encrypt(T m) {
        if (!keySet) {
            System.err.println("Error: Klucz nie został jeszcze ustawiony!");
            return null;
        }
        return (T) T.mul(m, privateKey);
    }

    public T decrypt(T c) {
        if (!keySet) {
            System.err.println("Error: Klucz nie został jeszcze ustawiony!");
            return null;
        }
        return (T) T.div(c, privateKey);
    }
}