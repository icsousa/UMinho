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

    public boolean transfer(int from, int to, int value) {
        // 1. Validações básicas
        if (from < 0 || from >= slots || to < 0 || to >= slots) return false;
        if (from == to) return false; // Transferir para si próprio não faz sentido

        // 2. Ordenação dos Locks (Deadlock Prevention)
        // Para evitar que a thread A bloqueie (1->2) e a B bloqueie (2->1) ao mesmo tempo,
        // temos de garantir que bloqueamos sempre as contas pela mesma ordem (ex: menor ID primeiro).
        int minId = Math.min(from, to);
        int maxId = Math.max(from, to);

        Account accountMin = av[minId];
        Account accountMax = av[maxId];

        // 3. Obtenção dos Locks
        synchronized (accountMin) {
            synchronized (accountMax) {
                // 4. Operação Atómica
                // Aqui dentro, temos a posse das duas contas. Ninguém mexe nelas.
                Account cfrom = av[from];
                Account cto = av[to];

                if (cfrom.withdraw(value)) {
                    cto.deposit(value);
                    return true;
                }
                return false;
            }
        }
    }

    public int totalBalance(){
        int total = 0;
        for (Account c : av){
            total += c.balance();
        }
        return total;
    }
}