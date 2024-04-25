public class GF {
    private int val;
    private int characteristic;

    public GF(int val, int characteristic) {
        this.characteristic = characteristic;
        this.val = val % characteristic;
    }

    public GF(int val) {
        this(val, 1234577);
    }

    @Override
    public String toString() {
        return String.format("%d, %d", val, characteristic);
    }

    private void checkErr(GF other) {
        if (this.characteristic != other.characteristic) {
            throw new IllegalArgumentException("Characteristics of GF elements do not match.");
        }
    }

    public boolean lessThan(GF other) {
        checkErr(other);
        return this.val < other.val;
    }

    public boolean greaterThan(GF other) {
        checkErr(other);
        return this.val > other.val;
    }

    public boolean lessThanOrEqual(GF other) {
        checkErr(other);
        return this.val <= other.val;
    }

    public boolean greaterThanOrEqual(GF other) {
        checkErr(other);
        return this.val >= other.val;
    }

    public boolean equals(GF other) {
        checkErr(other);
        return this.val == other.val;
    }

    public boolean notEquals(GF other) {
        checkErr(other);
        return this.val != other.val;
    }

    public GF add(GF other) {
        checkErr(other);
        return new GF((this.val + other.val) % this.characteristic, this.characteristic);
    }

    public GF subtract(GF other) {
        checkErr(other);
        return new GF((this.val + (other.characteristic - other.val)) % this.characteristic, this.characteristic);
    }

    public GF multiply(GF other) {
        checkErr(other);
        return new GF((this.val * other.val) % this.characteristic, this.characteristic);
    }

    public GF divide(GF other) {
        checkErr(other);
        int[] result = extendedGCD(other.val, other.characteristic);
        int gcd = result[0];
        int x = result[1];
        if(x < 0){
            x = other.characteristic - Math.abs(x);
        }
        if (gcd != 1) {
            throw new ArithmeticException("Division not possible.");
        }

        return new GF((this.val * (x % other.characteristic)) % this.characteristic, this.characteristic);
    }

    public void addInPlace(GF other) {
        checkErr(other);
        this.val = (this.val + other.val) % this.characteristic;
    }

    public void subtractInPlace(GF other) {
        checkErr(other);
        this.val = (this.val + (this.characteristic - other.val)) % this.characteristic;
    }

    public void multiplyInPlace(GF other) {
        checkErr(other);
        this.val = (this.val * other.val) % this.characteristic;
    }

    public void divideInPlace(GF other) {
        checkErr(other);
        int[] result = extendedGCD(other.val, other.characteristic);
        int gcd = result[0];
        int x = result[1];
        if(x < 0){
            x = other.characteristic - Math.abs(x);
        }
        if (gcd != 1) {
            throw new ArithmeticException("Division not possible.");
        }

        this.val = (this.val * (x % other.characteristic)) % this.characteristic;
    }

    private int[] extendedGCD(int a, int b) {
        if (b == 0) {
            return new int[] { a, 1, 0 };
        } else {
            int[] temp = extendedGCD(b, a % b);
            int d = temp[0];
            int x = temp[2];
            int y = temp[1] - (a / b) * temp[2];
            return new int[] { d, x, y };
        }
    }
}
