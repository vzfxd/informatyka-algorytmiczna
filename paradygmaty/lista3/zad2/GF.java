public class GF {
    private static long GF_P = 1234577;
    public long value;

    public GF(long _value){
        this.value = GF.modulo(_value);
    } 

    public static GF newInstance(long _value){
        return new GF(_value);
    }

    public static void set_p(long p)
    {
        GF_P = p;
    }

    public static long get_p()
    {
        return GF_P;
    }

    public static GF add(GF a, GF b)
    {
        return new GF(a.value + b.value);
    } 

    public static GF sub(GF a, GF b)
    {
        return new GF(a.value - b.value);
    } 

    public static GF mul(GF a, GF b)
    {
        return new GF(a.value * b.value);
    }

    private long mult_inv()
    {
        long p, a0, y, x;
        long a = this.value;
        p = GF_P;
        a0 = a;
        y = 0;
        x = 1;

        while (a > 1 && p != 0) {
            long q = a / p;
            long t = p;
            p = a % p;
            a = t;
            t = y;
            y = x - q * y;
            x = t;
        }

        if (a != 1) {
            System.err.printf("Nie istnieje odworotność %d modulo %d\n", a0, GF_P);
        }

        if (x < 0) {
            x += GF_P;
        }
        return x;
    }

    public static GF div(GF a, GF b)
    {
        return new GF(a.value * b.mult_inv());
    }

    public void iadd(GF other)
    {
        this.value = GF.modulo(this.value + other.value);
    }

    public void isub(GF other)
    {
        this.value = GF.modulo(this.value - other.value);
    }

    public void imult(GF other)
    {
        this.value = GF.modulo(this.value * other.value);
    }

    public void idiv(GF other)
    {
        this.value = GF.modulo(this.value * other.mult_inv());
    }

    public static boolean eq(GF a, GF b)
    {
        return a.value == b.value;
    }

    public static boolean neq(GF a, GF b)
    {
        return !(GF.eq(a, b));
    }

    public static boolean lt(GF a, GF b)
    {
        return a.value < b.value;
    }

    public static boolean gt(GF a, GF b)
    {
        return a.value > b.value;
    }

    public static boolean leq(GF a, GF b)
    {
        return a.value <= b.value;
    }

    public static boolean geq(GF a, GF b)
    {
        return a.value >= b.value;
    }

    private static long modulo(long val)
    {
        while(val < 0)
        {
            val += GF_P;
        }
        return val % GF_P;
    }
}