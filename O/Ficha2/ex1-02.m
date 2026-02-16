% Domínio
[w1,w2] = meshgrid(-5:0.01:5, -5:0.01:5);

% Função escalar
f=w1.^3+2*w1.*w2^2-w2.^3-20*w1;

% Figura das curvas de nível
figure; hold on; box on;

% Curvas de nível (contornos)
contour(w1, w2, f, 100, 'LineWidth', 1.2);

% Eixos coordenados
plot([-5,5], [0,0], 'k--', 'LineWidth', 1); % Eixo do X
plot([0,0], [-5,5], 'k--', 'LineWidth', 1); % Eixo do Y

% Solução Ótima
plot(2.580, 0, 'ko', 'MarkerFaceColor','k', 'MarkerSize', 5);
plot(-2.580, 0, 'ko', 'MarkerFaceColor','k', 'MarkerSize', 5);

% Rótulo w*
text(2.3, 0.3, '$w^*$', 'Interpreter', 'latex');
text(-2.3, 0.3, '$w^*$', 'Interpreter', 'latex');

% Rótulos



%---
surf(w1,w2,f);

