public class Test {
    public static void main(String[] args) {
        GF.set_p(1234567891);

        DHSetup<GF> setup = new DHSetup<>();

        User<GF> userA = new User<>(setup);
        GF publicKeyA = userA.getPublicKey();

        User<GF> userB = new User<>(setup);
        GF publicKeyB = userB.getPublicKey();

        userA.setKey(publicKeyB, setup);
        userB.setKey(publicKeyA, setup);

        GF message = new GF(111333);
        GF encrypted = userA.encrypt(message);
        GF decrypted = userB.decrypt(encrypted);

        System.out.println("pubkey a: " + publicKeyA.value);
        System.out.println("pubkey b: " + publicKeyB.value);
        System.out.println("msg: " + message.value);
        System.out.println("encrypted: " + encrypted.value);
        System.out.println("decrypted: " + decrypted.value);
    }
}