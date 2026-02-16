/**
 * Resolução do Guião 1 - Programação Concorrente
 * * Abrange:
 * 1. Criação de Threads (N threads imprimem 1 a I) 
 * 2. Partilha de estado (Objeto Counter partilhado) 
 * 3. Observação de Race Conditions (Corridas) 
 */
public class Guiao1 {

    // Definições globais para os testes
    static final int N = 10;      // Número de threads
    static final int I = 10000;   // Iterações (aumentado para tornar as corridas mais visíveis)

    public static void main(String[] args) throws InterruptedException {
        System.out.println("=== INÍCIO DA RESOLUÇÃO DO GUIÃO 1 ===\n");

        // --- EXERCÍCIO 1 ---
        System.out.println(">>> A executar Exercício 1: N Threads a imprimir números...");
        executarExercicio1();
        System.out.println(">>> Exercício 1 terminado.\n");

        // --- EXERCÍCIOS 2 e 3 (Versão Método) ---
        System.out.println(">>> A executar Exercício 2/3 (Versão A: Método increment())...");
        executarExercicio2e3(true); // true = usar método
        
        // --- EXERCÍCIOS 2 e 3 (Versão Variável Direta) ---
        System.out.println("\n>>> A executar Exercício 2/3 (Versão B: Acesso direto à variável)...");
        executarExercicio2e3(false); // false = acesso direto
        
        System.out.println("\n=== FIM ===");
    }

    // ------------------------------------------------------------------------
    // LÓGICA DO EXERCÍCIO 1
    // "Escreva um programa que crie N threads, em que cada uma imprime os 
    // números de 1 a I" 
    // ------------------------------------------------------------------------
    private static void executarExercicio1() throws InterruptedException {
        Thread[] threads = new Thread[N];

        for (int i = 0; i < N; i++) {
            final int threadId = i;
            threads[i] = new Thread(() -> {
                for (int j = 1; j <= I; j++) {
                    System.out.println("Thread " + threadId + ": " + j);
                }
            });
            threads[i].start();
        }

        for (int i = 0; i < N; i++) {
            threads[i].join();
        }
    }

    // ------------------------------------------------------------------------
    // LÓGICA DOS EXERCÍCIOS 2 e 3
    // "N threads terem acesso a um único objecto partilhado" 
    // "incrementar I vezes o contador" [cite: 17]
    // ------------------------------------------------------------------------
    private static void executarExercicio2e3(boolean usarMetodo) throws InterruptedException {
        Counter c = new Counter();
        Thread[] threads = new Thread[N];

        for (int i = 0; i < N; i++) {
            // Cria a thread passando o contador partilhado
            threads[i] = new Thread(new Incrementer(c, I, usarMetodo));
            threads[i].start();
        }

        // Espera que todas terminem (join)
        for (int i = 0; i < N; i++) {
            threads[i].join();
        }

        // "escrever o valor do contador depois de as outras threads terem terminado" [cite: 19]
        System.out.println("Valor Esperado: " + (N * I));
        System.out.println("Valor Obtido:   " + c.value);
        
        if (c.value != (N * I)) {
            System.out.println("(Atenção: Ocorreu uma Condição de Corrida!)");
        }
    }

    // --- Classes Auxiliares (Counter e Runnable) ---

    // O Recurso Partilhado
    static class Counter {
        public int value = 0; // Público para permitir acesso direto conforme pedido no Ex 2 [cite: 18]

        public void increment() {
            this.value++;
        }
    }

    // A Tarefa da Thread
    static class Incrementer implements Runnable {
        private final Counter counter;
        private final int nTimes;
        private final boolean useMethod;

        public Incrementer(Counter c, int nTimes, boolean useMethod) {
            this.counter = c;
            this.nTimes = nTimes;
            this.useMethod = useMethod;
        }

        @Override
        public void run() { // Implementação obrigatória de Runnable [cite: 9, 12]
            for (int i = 0; i < nTimes; i++) {
                if (useMethod) {
                    // Versão 1: Invoca método increment [cite: 18]
                    counter.increment();
                } else {
                    // Versão 2: Acede directamente à variável de instância [cite: 18]
                    counter.value++;
                }
            }
        }
    }
}