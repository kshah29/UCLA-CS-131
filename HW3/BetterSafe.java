import java.util.concurrent.locks.ReentrantLock; 

class BetterSafe implements State {
    private byte[] value;
    private byte maxval;
    private ReentrantLock relock ;

    BetterSafe (byte[] v) { 
        value = v; 
        maxval = 127; 
        relock = new ReentrantLock() ;
    }

    BetterSafe (byte[] v, byte m) { 
        value = v; 
        maxval = m; 
        relock = new ReentrantLock() ;
    }

    public int size() { return value.length ; }
    
    public byte[] current() { return value; }
    
    // use Reentrant lock instead of synchronized keyword
    public boolean swap(int i, int j) {
        relock.lock() ;

        if (value[i] <= 0 || value[j] >= maxval) {
            relock.unlock();
            return false;
        }
        value[i]--;
        value[j]++;

        relock.unlock();
        return true;
    }
}