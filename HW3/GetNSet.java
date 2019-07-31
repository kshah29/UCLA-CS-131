import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSet(byte[] v) { 
        createArray(v);
        maxval = 127; 
    }

    GetNSet(byte[] v, byte m) { 
        createArray(v);
        maxval = m; 
    }

    public int size() { return value.length(); }

    public byte[] current() {
        int len = size () ;
        byte ret [] = new byte [ len ]  ;

        for (int i=0; i<len; i++) {
            ret[i] = (byte) value.get(i) ;
        }
        return ret ;
    }

    public boolean swap(int i, int j) {
        int ival = value.get(i) ;
        int jval = value.get(j) ;

        if (ival <= 0 || jval >= maxval) {
            return false;
        }
        ival--;
        jval++;

        value.set(i, ival);
        value.set(j, jval);

        return true;
    }

    private void createArray (byte [] v) {
        value = new AtomicIntegerArray(v.length) ;
        int len = size () ;

        for (int i=0; i<len; i++){
            value.set(i, v[i]);
        }
    }
}