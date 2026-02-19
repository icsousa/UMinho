import java.util.*;
import java.util.concurrent.locks.*;

class Bank {

    private static class Account {
        private int balance;
        // Adição de um lock individual por conta
        final ReentrantLock lock = new ReentrantLock(); 

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

    private Map<Integer, Account> map = new HashMap<Integer, Account>();
    private int nextId = 0;
    
    private final ReentrantLock bankLock = new ReentrantLock();

    public int createAccount(int balance) {
        bankLock.lock();
        try {
            Account c = new Account(balance);
            int id = nextId;
            nextId += 1;
            map.put(id, c);
            return id;
        } finally {
            bankLock.unlock();
        }
    }

    public int closeAccount(int id) {
        Account c;
        bankLock.lock();
        try {
            c = map.remove(id);
            if (c == null)
                return 0;
            c.lock.lock();
        } finally {
            bankLock.unlock();
        }
        
        try {
            return c.balance();
        } finally {
            c.lock.unlock();
        }
    }

    public int balance(int id) {
        Account c;
        bankLock.lock();
        try {
            c = map.get(id);
            if (c == null)
                return 0; 
            c.lock.lock();
        } finally {
            bankLock.unlock();
        }
        
        try {
            return c.balance();
        } finally {
            c.lock.unlock();
        }
    }

    public boolean deposit(int id, int value) {
        Account c;
        bankLock.lock();
        try {
            c = map.get(id);
            if (c == null)
                return false;
            c.lock.lock();
        } finally {
            bankLock.unlock();
        }
        
        try {
            return c.deposit(value);
        } finally {
            c.lock.unlock();
        }
    }

    public boolean withdraw(int id, int value) {
        Account c;
        bankLock.lock();
        try {
            c = map.get(id);
            if (c == null)
                return false;
            c.lock.lock();
        } finally {
            bankLock.unlock();
        }
        
        try {
            return c.withdraw(value);
        } finally {
            c.lock.unlock();
        }
    }

    public boolean transfer(int from, int to, int value) {
        Account cfrom, cto;
        bankLock.lock();
        try {
            cfrom = map.get(from);
            cto = map.get(to);
            if (cfrom == null || cto == null)
                return false;
            if (from < to) {
                cfrom.lock.lock();
                cto.lock.lock();
            } else if (from > to) {
                cto.lock.lock();
                cfrom.lock.lock();
            } else {
                cfrom.lock.lock();
            }
        } finally {
            bankLock.unlock();
        }
            
        try {
            try {
                return cto.deposit(value);
            } finally {
                cto.lock.unlock();
            }
            return cfrom.withdraw(value);
        } finally {
            cfrom.lock.unlock();
        }
        

    }

    public int totalBalance(int[] ids) {
        ids = ids.clone();
        Arrays.sort(ids); // Evitamos deadlock
        int total = 0;
        Account[] al = new Account[]; 

        bankLock.lock()

        try {
            for (int i = 0; i < ids.length; i++) {
                al[i] = map.get(ids[i]);
                if (al[i] == null) return 0;
            }
            for (Account c : al) {
                c.lock.lock();
            }
        } finally {
            bankLock.unlock();
        }
        for (Account c : al) {
            total += c.balance();
            c.lock.unlock();
        }
        return total;
    }
}