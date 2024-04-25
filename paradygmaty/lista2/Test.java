public class Test {
    public static void main(String[] args) {
        // Addition
        GF a = new GF(3);
        GF b = new GF(7);
        GF c = new GF(10);

        System.out.println();
        System.out.println("a = GF(3)\nb = GF(7)\nc = GF(10)");
        System.out.println("a + b = " + a.add(b));
        a.addInPlace(b);
        System.out.println("a += b || a = " + a);
        System.out.println("a == c " + a.equals(c));

        // Subtraction
        a = new GF(915);
        b = new GF(916);
        c = new GF(1234576);

        System.out.println();
        System.out.println("a = GF(915)\nb = GF(916)\nc = GF(1234576)");
        System.out.println("a - b = " + a.subtract(b));
        System.out.println("a - b == c " + a.subtract(b).equals(c));
        a.subtractInPlace(b);
        System.out.println("a -= b || a = " + a);
        System.out.println("a == c " + a.equals(c));

        // Multiplication
        a = new GF(12);
        b = new GF(88);
        c = new GF(1056);

        System.out.println();
        System.out.println("a = GF(12)\nb = GF(88)\nc = GF(1056)");
        System.out.println("a * b = " + a.multiply(b));
        System.out.println("a * b == c " + a.multiply(b).equals(c));
        a.multiplyInPlace(b);
        System.out.println("a *= b || a = " + a);
        System.out.println("a == c " + a.equals(c));
        System.out.println("a > b " + a.greaterThan(b));
        System.out.println("a >= b " + a.greaterThanOrEqual(b));
        System.out.println("b < a " + b.lessThan(a));
        System.out.println("b <= a " + b.lessThanOrEqual(a));

        // Division
        a = new GF(88);
        b = new GF(11);
        c = new GF(8);

        System.out.println();
        System.out.println("a = GF(88)\nb = GF(11)\nc = GF(8)");
        System.out.println("a / b = " + a.divide(b));
        System.out.println("a / b == c " + a.divide(b).equals(c));
        System.out.println("a /= b");
        a.divideInPlace(b);
        System.out.println("a == c " + a.equals(c));
    }
}
