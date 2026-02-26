class Barrier {
    private final int N;
    private int count;

    public Barrier(int N) { 
        this.N = N;
        this.count = 0;
    }

    public synchronized void await() throws InterruptedException {
        count++;
        
        if (count < N) {
            while (count < N) {
                wait();
            }
        }
        else {
            notifyAll();
        }
    }
}