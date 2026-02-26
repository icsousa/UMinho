class ReusableBarrier {
    private final int N;
    private int count;
    private int phase;

    public ReusableBarrier(int N) {
        this.N = N;
        this.count = 0;
        this.phase = 0;
    }

    public synchronized void await() throws InterruptedException {
        int myPhase = this.phase;
        count++;
        
        if (count == N) {
            this.phase++;
            this.count = 0;
            notifyAll();
        }
        else {
            while(this.phase == myPhase) {
                wait();
            }
        }
    }
}