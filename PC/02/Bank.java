public class Bank {

    private static class Account {
        private int balance;
        Account(int balance) { this.balance = balance; }
        int balance() { return balance; }
        boolean deposit(int value) {
            balance += value;
            return true;
        }
        boolean withdraw(int value) {
            if (value > balance)
                return false;
            balance -= value;
            return true;
        }

    }

    // Bank slots and vector of accounts
    private int slots;
    private Account[] av; 

    public Bank(int n) {
        slots=n;
        av=new Account[slots];
        for (int i=0; i<slots; i++) av[i]=new Account(0);
    }

    // Account balance
    // Adicionado 'synchronized' para garantir leitura consistente
    public synchronized int balance(int id) {
        if (id < 0 || id >= slots)
            return 0;
        return av[id].balance();
    }

    // Deposit
    // Adicionado 'synchronized' para evitar corridas na escrita
    public synchronized boolean deposit(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].deposit(value);
    }

    // Withdraw; fails if no such account or insufficient balance
    // Adicionado 'synchronized'
    public synchronized boolean withdraw(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].withdraw(value);
    }

    public synchronized boolean transfer(int from, int to, int value) {
        if (this.withdraw(from, value)){
            if (this.deposit(to, value)) {
                return true;
            }
            else {
                this.deposit(from, value);
                return false;
            }
        }
        return false;
    }

    public synchronized int totalBalance(){
        int total = 0;
        for (Account c : av){
            total += c.balance();
        }
        return total;
    }
}