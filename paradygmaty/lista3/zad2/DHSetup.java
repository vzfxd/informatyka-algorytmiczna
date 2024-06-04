import java.util.Random;
import java.util.ArrayList;

public class DHSetup<T extends GF>{
    private T generator;
    public long p;
    
    public DHSetup() {
        this.p = T.get_p();

        ArrayList<Long> primes = new ArrayList<>();

        for (long i = 2; i * i <= (p - 1); ++i) {
            if ((p - 1) % i == 0) {
                primes.add(i);
                if (i != (p - 1) / i) {
                    primes.add((p - 1) / i);
                }
            }
        }

        setGenerator(primes);
    }

    public void setGenerator(ArrayList<Long> primes) {
        Random random = new Random();
        long potentialGenerator;
        do {
            potentialGenerator = random.nextLong() % (p - 1) + 1;
        } while (!isValidGenerator(potentialGenerator, p - 1, primes));
        
        generator = (T) T.newInstance(potentialGenerator);
    }
    
    public T getGenerator() {
        return generator;
    }

    public T power(T a, long b) {
        T result = (T) T.newInstance(1);
        while (b > 0) {
            if ((b & 1) == 1) {
                result = (T) T.mul(result, a);
            }
            a = (T) T.mul(a, a);
            b >>= 1;
        }
        return result;
    }
    
    private boolean isValidGenerator(long a, long pMinusOne, ArrayList<Long> primes) {
        for (Long q : primes)
        {
            if (pMinusOne % q == 0) {
                if (T.eq(power((T) T.newInstance(a), q), (T) T.newInstance(1))) {
                    return false;
                }
            }
        }
        return true;
    }
}