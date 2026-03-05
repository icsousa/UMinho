import java.util.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

class Warehouse {
    private Map<String, Product> map = new HashMap<String, Product>();
    
    private final ReentrantLock lock = new ReentrantLock();

    private class Product { 
        int quantity = 0; 
        Condition cond = lock.newCondition(); 
    }

    private Product get(String item) {
        Product p = map.get(item);
        if (p != null) return p;
        p = new Product();
        map.put(item, p);
        return p;
    }
 
    public void supply(String item, int quantity) {
        lock.lock();
        try {
            Product p = get(item);
            p.quantity += quantity;
            p.cond.signalAll(); 
        } finally {
            lock.unlock();
        }
    }

    public void consumeEgoista(Set<String> items) throws InterruptedException {
        lock.lock();
        try {
            for (String s : items) {
                Product p = get(s);
                while (p.quantity == 0) {
                    p.cond.await();
                }
                p.quantity--;
            }
        } finally {
            lock.unlock();
        }
    }

    public void consumeCooperativo(Set<String> items) throws InterruptedException {
        lock.lock();
        try {
            boolean canConsume = false;
            
            while (!canConsume) {
                Product missingProduct = null;
                
                for (String s : items) {
                    Product p = get(s);
                    if (p.quantity == 0) {
                        missingProduct = p;
                        break;
                    }
                }
                
                if (missingProduct != null) {
                    missingProduct.cond.await();
                } else {
                    canConsume = true;
                }
            }
            
            for (String s : items) {
                get(s).quantity--;
            }
            
        } finally {
            lock.unlock();
        }
    }
}