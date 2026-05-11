import java.net.Socket;
import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Condition;

// ── Ligação ao servidor ───────────────────────────────────────────
Socket meuSocket;
PrintWriter meuOut;

// ── Lock principal (Guião 3 — ReentrantLock) ─────────────────────
// Protege todo o estado partilhado entre a thread de rede
// e a thread principal do Processing (draw).
// Sem este lock, as duas threads podiam aceder ao mesmo estado
// simultaneamente — race condition igual ao Counter do Guião 1.
final ReentrantLock lock = new ReentrantLock();

// Condition para sinalizar mudanças de estado (Guião 5)
final Condition estadoMudou = lock.newCondition();

// ── Estado do jogo ────────────────────────────────────────────────
// ACESSO: sempre dentro de lock.lock() / lock.unlock()
// 0=Menu  1=Jogo  2=Lobby  3=Resultados 4=CLASSIFICAÇÕES
int estadoJogo         = 0;
int tempoInicioPartida = 0;
String mensagemMenu    = "Insira as credenciais e escolha uma ação.";

// ── Dados do jogador próprio ──────────────────────────────────────
String pNome    = "";
float  pX       = -100;
float  pY       = -100;
float  pTamanho = 50;
float  pAngulo  = 0;
int    pKills = 0;

// ── Autenticação ──────────────────────────────────────────────────
// Só usadas pela thread principal — não precisam de lock
String  username       = "";
String  password       = "";
boolean typingUsername = true;

// ── Resultados e highscores ───────────────────────────────────────
// ArrayList normal protegido pelo ReentrantLock
// (o lock garante exclusão mútua — não precisamos de CopyOnWriteArrayList)
ArrayList<String>  nomesResultados  = new ArrayList<String>();
ArrayList<Integer> pontosResultados = new ArrayList<Integer>();
ArrayList<String>  nomesHighscores  = new ArrayList<String>();
ArrayList<Integer> pontosHighscores = new ArrayList<Integer>();

// ── Objetos e adversários ─────────────────────────────────────────
ArrayList<float[]> objetos          = new ArrayList<float[]>();
ArrayList<float[]> adversarios      = new ArrayList<float[]>();
ArrayList<String>  adversariosNomes = new ArrayList<String>();

// Buffers de espera — trocados atomicamente no SYNC
ArrayList<float[]> objetosEspera          = new ArrayList<float[]>();
ArrayList<float[]> adversariosEspera      = new ArrayList<float[]>();
ArrayList<String>  adversariosNomesEspera = new ArrayList<String>();

// ── Teclas ───────────────────────────────────────────────────────
// Só usadas pela thread principal — não precisam de lock
boolean upPressed    = false;
boolean leftPressed  = false;
boolean rightPressed = false;

// ─────────────────────────────────────────────────────────────────
void setup() {
  size(800, 600);
  frameRate(60);
  surface.setTitle("Foguete.io");

  try {
    meuSocket = new Socket("127.0.0.1", 8080);
    meuOut    = new PrintWriter(meuSocket.getOutputStream(), true);

    // Thread separada para ler mensagens do servidor (Guião 1)
    // Corre em paralelo com o draw() — daí a necessidade do lock
    Thread ouvinte = new Thread(new Runnable() {
      public void run() {
        try {
          BufferedReader in = new BufferedReader(
            new InputStreamReader(meuSocket.getInputStream())
          );
          String linha;
          while ((linha = in.readLine()) != null) {
            processarMensagem(linha.trim());
          }
        } catch (Exception e) {
          // Adquire o lock para escrever na variável partilhada
          lock.lock();
          try { mensagemMenu = "Ligação perdida com o servidor."; }
          finally { lock.unlock(); }
        }
      }
    });
    ouvinte.setDaemon(true);
    ouvinte.start();

  } catch (Exception e) {
    mensagemMenu = "Erro: Servidor offline.";
  }
}

// ── Parser de mensagens (corre na thread de rede) ─────────────────
// Toda a escrita no estado partilhado é feita dentro do lock
// para garantir exclusão mútua com o draw() — Guião 2 e 3
void processarMensagem(String msg) {
  if (msg == null || msg.isEmpty()) return;
  String[] partes = msg.split(",");

  // Adquire o lock antes de qualquer escrita no estado partilhado
  lock.lock();
  try {
    switch (partes[0]) {

      case "HIGHSCORES":
        nomesHighscores.clear();
        pontosHighscores.clear();
        int numScores = (partes.length - 1) / 2;
        for (int i = 0; i < numScores; i++) {
          nomesHighscores.add(partes[1 + i*2]);
          pontosHighscores.add(Integer.parseInt(partes[2 + i*2]));
        }
        break;

      case "REG_OK":
        mensagemMenu = "Registo com sucesso! Faça Login."; break;
      case "REG_FAIL":
        mensagemMenu = "Erro: O utilizador já existe."; break;
      case "LOGIN_OK":
        mensagemMenu = "Login aceite! A colocar na fila...";
        estadoJogo = 2;
        estadoMudou.signalAll(); // sinaliza o draw() (Guião 5)
        break;
      case "LOGIN_FAIL":
        mensagemMenu = "Erro: Dados incorretos."; break;
      case "CANCEL_OK":
        mensagemMenu = "Registo cancelado com sucesso."; break;
      case "CANCEL_FAIL":
        mensagemMenu = "Erro ao cancelar."; break;

      case "MATCH_START":
        estadoJogo = 1;
        tempoInicioPartida = millis();
        estadoMudou.signalAll();
        break;

      case "MATCH_RESULTS":
        nomesResultados.clear();
        pontosResultados.clear();
        int numJ = (partes.length - 1) / 2;
        for (int i = 0; i < numJ; i++) {
          nomesResultados.add(partes[1 + i*2]);
          pontosResultados.add(Integer.parseInt(partes[2 + i*2]));
        }
        objetos.clear();
        adversarios.clear();
        upPressed = false; leftPressed = false; rightPressed = false;
        estadoJogo = 3;
        estadoMudou.signalAll();
        break;

      case "STATE":
        if (partes.length == 7) {
          pNome    = partes[1];
          pX       = Float.parseFloat(partes[2]);
          pY       = Float.parseFloat(partes[3]);
          pTamanho = Float.parseFloat(partes[4]);
          pAngulo  = Float.parseFloat(partes[5]);
          pKills   = Integer.parseInt(partes[6]);
          objetosEspera.clear();
          adversariosEspera.clear();
          adversariosNomesEspera.clear();
        }
        break;

      case "OTHER":
        if (partes.length == 6) {
          adversariosNomesEspera.add(partes[1]);
          adversariosEspera.add(new float[]{
            Float.parseFloat(partes[2]),
            Float.parseFloat(partes[3]),
            Float.parseFloat(partes[4]),
            Float.parseFloat(partes[5])
          });
        }
        break;

      case "OBJ":
        if (partes.length == 5) {
          objetosEspera.add(new float[]{
            Float.parseFloat(partes[1]),
            Float.parseFloat(partes[2]),
            Float.parseFloat(partes[3]),
            Float.parseFloat(partes[4])
          });
        }
        break;

      case "SYNC":
        // Troca atómica dos buffers dentro do lock —
        // o draw() nunca vê um frame a meio de uma atualização
        objetos.clear();          objetos.addAll(objetosEspera);
        adversarios.clear();      adversarios.addAll(adversariosEspera);
        adversariosNomes.clear(); adversariosNomes.addAll(adversariosNomesEspera);
        break;
    }
  } finally {
    // finally garante que o lock é sempre libertado — Guião 3
    lock.unlock();
  }
}

// ── Enviar comando ao servidor ────────────────────────────────────
void enviar(String msg) {
  if (meuOut != null) meuOut.println(msg);
}

// ─────────────────────────────────────────────────────────────────
void draw() {
  // Adquire o lock antes de ler qualquer estado partilhado
  // Garante que o draw() nunca lê dados a meio de uma atualização
  // feita pela thread de rede — exclusão mútua (Guião 2 e 3)
  lock.lock();
  try {
    switch (estadoJogo) {
      case 0: desenharMenu(); break;
      case 1: desenharJogo(); break;
      case 2: desenharLobby(); break;
      case 3: desenharResultados(); break;
      case 4: desenharClassificacoes(); break;
    }
  } finally {
    lock.unlock();
  }
}

// ── MENU ──────────────────────────────────────────────────────────
void desenharMenu() {
  background(40, 40, 60);
  textAlign(CENTER); textSize(36); fill(255); text("FOGUETE.io", width/2, 100);
  textSize(18); fill(200, 200, 100); text(mensagemMenu, width/2, 150);

  stroke(0); fill(typingUsername ? 255 : 200); rect(width/2 - 125, 200, 250, 40, 5);
  fill(0); textAlign(LEFT);
  text("User: " + username + (typingUsername && frameCount%60<30 ? "|" : ""), width/2 - 110, 225);

  stroke(0); fill(!typingUsername ? 255 : 200); rect(width/2 - 125, 260, 250, 40, 5);
  fill(0);
  String passOculta = "*".repeat(password.length());
  text("Pass: " + passOculta + (!typingUsername && frameCount%60<30 ? "|" : ""), width/2 - 110, 285);

  textAlign(CENTER); textSize(16);
  fill(100, 200, 100); rect(width/2 - 180, 350, 100, 40, 5); fill(0); text("Registar",  width/2 - 130, 375);
  fill(100, 150, 255); rect(width/2 - 50,  350, 100, 40, 5); fill(0); text("Login",     width/2,       375);
  fill(255, 100, 100); rect(width/2 + 80,  350, 100, 40, 5); fill(0); text("Cancelar",  width/2 + 130, 375);

  // Botão Amarelo para CLASSIFICAÇÕES
  fill(255, 215, 0); // Cor amarela
  rect(width/2 - 75, 410, 150, 40, 5);
  fill(0); 
  textAlign(CENTER, CENTER); 
  textSize(16);
  text("Classificações", width/2, 428);
}

// ── CLASSIFICAÇÕES ─────────────────────────────────────────────────
void desenharClassificacoes() {
  background(40, 40, 60);
  
  // Título
  textAlign(CENTER); textSize(36); fill(255); 
  text("Melhores Classificações", width/2, 100);
  
  // Lista de Highscores
  textSize(20);
  for (int i = 0; i < nomesHighscores.size(); i++) {
    fill(i == 0 ? color(255, 215, 0) : color(255)); // 1º lugar a amarelo
    text((i+1) + "º - " + nomesHighscores.get(i) + " - " + pontosHighscores.get(i) + " pts", width/2, 160 + i*30);
  }
  
  // Botão Atualizar
  fill(100, 200, 100); rect(width/2 - 160, height - 100, 140, 40, 5);
  fill(0); textSize(16); textAlign(CENTER, CENTER);
  text("Atualizar", width/2 - 90, height - 82);
  
  // Botão Voltar
  fill(255, 100, 100); rect(width/2 + 20, height - 100, 140, 40, 5);
  fill(0); 
  text("Voltar", width/2 + 90, height - 82);
}

// ── LOBBY ─────────────────────────────────────────────────────────
void desenharLobby() {
  background(20, 20, 40);
  textAlign(CENTER, CENTER);
  fill(255); textSize(30); text("A AGUARDAR JOGADORES...", width/2, height/2 - 100);
  fill(150, 200, 150); textSize(18);
  text("A partida inicia quando houver 3 ou 4 pessoas.", width/2, height/2 - 40);
  fill(255, 215, 0); textSize(24); text("Melhores Classificações", width/2, height/2);
  textSize(20); fill(255);
  for (int i = 0; i < nomesHighscores.size(); i++)
    text((i+1) + "º - " + nomesHighscores.get(i) + " - " + pontosHighscores.get(i) + " pts", width/2, height/2 + 30 + i*30);
}

// ── JOGO ──────────────────────────────────────────────────────────
void desenharJogo() {
  background(255);

  for (float[] obj : objetos) {
    if (obj[3] == 1) fill(0, 255, 0); else fill(255, 0, 0);
    noStroke(); ellipse(obj[0], obj[1], obj[2], obj[2]);
  }

  for (int i = 0; i < adversarios.size(); i++) {
    float[] adv = adversarios.get(i);
    String nome = i < adversariosNomes.size() ? adversariosNomes.get(i) : "?";
    pushMatrix(); translate(adv[0], adv[1]); rotate(adv[3]);
    fill(0); stroke(255, 0, 0); strokeWeight(3);
    ellipse(0, 0, adv[2], adv[2]);
    stroke(255, 0, 0); line(0, 0, adv[2]/2, 0);
    popMatrix();
    fill(0); textAlign(CENTER); textSize(14);
    text(nome, adv[0], adv[1] - adv[2]/2 - 10);
  }

  pushMatrix(); translate(pX, pY); rotate(pAngulo);
  fill(0); stroke(0, 0, 255); strokeWeight(3);
  ellipse(0, 0, pTamanho, pTamanho);
  stroke(255, 0, 0); line(0, 0, pTamanho/2, 0);
  popMatrix();
  fill(0); textAlign(CENTER); textSize(14);
  text(pNome, pX, pY - pTamanho/2 - 10);

  int pontos = pKills;
  fill(50, 50, 50, 200); noStroke(); rect(width - 160, height - 50, 150, 40, 10);
  fill(255); textAlign(CENTER, CENTER); textSize(20);
  text("Pontos: " + pontos, width - 85, height - 31);

  int secsRestantes = max(0, 120 - (millis() - tempoInicioPartida) / 1000);
  fill(50, 50, 50, 200); noStroke(); rect(10, height - 50, 150, 40, 10);
  if (secsRestantes <= 10) fill(255, 50, 50); else fill(255);
  textAlign(CENTER, CENTER); textSize(20);
  text("Tempo: " + (secsRestantes/60) + ":" + nf(secsRestantes%60, 2), 85, height - 31);
}

// ── RESULTADOS ────────────────────────────────────────────────────
void desenharResultados() {
  background(30, 30, 50);
  textAlign(CENTER); fill(255); textSize(36);
  text("FIM DA PARTIDA - RESULTADOS", width/2, 100);
  textSize(24);
  for (int i = 0; i < nomesResultados.size(); i++) {
    fill(i == 0 ? color(255, 215, 0) : color(200, 200, 200));
    text((i+1) + "º Lugar: " + nomesResultados.get(i) + "  -  " + pontosResultados.get(i) + " pts", width/2, 200 + i*40);
  }
  fill(100, 150, 255); rect(width/2 - 100, height - 100, 200, 50, 10);
  fill(0); textSize(20); text("Voltar ao Menu", width/2, height - 68);
}

// ── INPUT ─────────────────────────────────────────────────────────
void mousePressed() {
  if (estadoJogo == 0) {
    if (mouseX > width/2 - 125 && mouseX < width/2 + 125) {
      if (mouseY > 200 && mouseY < 240) typingUsername = true;
      if (mouseY > 260 && mouseY < 300) typingUsername = false;
    }
    if (mouseY > 350 && mouseY < 390) {
      if (mouseX > width/2 - 180 && mouseX < width/2 - 80)
        enviar("REGISTER," + username + "," + password);
      if (mouseX > width/2 - 50 && mouseX < width/2 + 50)
        enviar("LOGIN," + username + "," + password);
      if (mouseX > width/2 + 80 && mouseX < width/2 + 180)
        enviar("CANCEL," + username + "," + password);
    }
    if (mouseX > width/2 - 75 && mouseX < width/2 + 75 && mouseY > 410 && mouseY < 450) {
      lock.lock();
      try {
        estadoJogo = 4; // Muda para a nova janela
      } finally {
        lock.unlock();
      }
      enviar("GET_HIGHSCORES"); // Pede a tabela logo ao entrar
    }
  }
  else if (estadoJogo == 3) {
    if (mouseX > width/2 - 100 && mouseX < width/2 + 100 && mouseY > height - 100 && mouseY < height - 50) {
      // Escrita no estado partilhado — dentro do lock
      lock.lock();
      try {
        estadoJogo   = 0;
        mensagemMenu = "Pronto para jogar novamente!";
      } finally {
        lock.unlock();
      }
      enviar("BACK_TO_MENU");
      enviar("GET_HIGHSCORES");
    }
  }
  else if (estadoJogo == 4) { // ── ECRÃ DE QUALIFICAÇÕES (NOVO) ──
    // Botão "Atualizar"
    if (mouseX > width/2 - 160 && mouseX < width/2 - 20 && mouseY > height - 100 && mouseY < height - 60) {
      enviar("GET_HIGHSCORES");
    }
    // Botão "Voltar"
    if (mouseX > width/2 + 20 && mouseX < width/2 + 160 && mouseY > height - 100 && mouseY < height - 60) {
      lock.lock();
      try {
        estadoJogo = 0; // Regressa ao Menu
      } finally {
        lock.unlock();
      }
    }
  }
}

void keyPressed() {
  if (estadoJogo == 0) {
    if (key == BACKSPACE) {
      if (typingUsername  && username.length() > 0) username = username.substring(0, username.length()-1);
      if (!typingUsername && password.length() > 0) password = password.substring(0, password.length()-1);
    } else if (key == TAB || key == ENTER || key == RETURN) {
      typingUsername = !typingUsername;
    } else if (key >= 32 && key <= 126 && key != ',') {
      if (typingUsername) username += key; else password += key;
    }
  } else if (estadoJogo == 1) {
    if (keyCode == UP)    upPressed    = true;
    if (keyCode == LEFT)  leftPressed  = true;
    if (keyCode == RIGHT) rightPressed = true;
    enviarTeclas();
  }
  else if (estadoJogo == 4) {
    // Clique no botão "Atualizar"
    if (mouseX > width/2 - 160 && mouseX < width/2 - 20 && mouseY > height - 100 && mouseY < height - 60) {
      enviar("GET_HIGHSCORES");
    }
    // Clique no botão "Voltar"
    if (mouseX > width/2 + 20 && mouseX < width/2 + 160 && mouseY > height - 100 && mouseY < height - 60) {
      lock.lock();
      try {
        estadoJogo = 0; // Regressa ao Menu
      } finally {
        lock.unlock();
      }
    }
  }
}

void keyReleased() {
  if (estadoJogo == 1) {
    if (keyCode == UP)    upPressed    = false;
    if (keyCode == LEFT)  leftPressed  = false;
    if (keyCode == RIGHT) rightPressed = false;
    enviarTeclas();
  }
}

void enviarTeclas() {
  String msg = "KEYS";
  if (upPressed)    msg += ",UP";
  if (leftPressed)  msg += ",LEFT";
  if (rightPressed) msg += ",RIGHT";
  enviar(msg);
}
