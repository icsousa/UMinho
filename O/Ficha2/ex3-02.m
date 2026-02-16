% a.
% Domínio
[w1,w2] = meshgrid(-2:0.01:2, -2:0.01:2);

% Função escalar
f=(1-w1).^2+(1-w2).^2+0.5*(2*w2-w1.^2).^2;

% Figura das curvas de nível
figure; hold on; box on;

% Curvas de nível (contornos)
contour(w1, w2, f, 100, 'LineWidth', 1.2);

% Eixos coordenados
plot([-2,2], [0,0], 'k--', 'LineWidth', 1); % Eixo do X
plot([0,0], [-2,2], 'k--', 'LineWidth', 1); % Eixo do Y

% Solução Ótima
plot(1.2, 0.8, 'ko', 'MarkerFaceColor','k', 'MarkerSize', 5);


% Rótulo w*
text(1, 0.6, '$w^*$', 'Interpreter', 'latex', FontSize=12);


% ---
% surf(w1,w2,f);

% c.
% fminunc
options = optimoptions('fminunc')

% Ponto Inicial
w0 = [1;1];

% Definir a Função
f = @(w)(1-w(1))^2+(1-w(2))^2+0.5*(2*w(2)-w(1)^2)^2;

% Minimização da Função f
[xopt, fopt, exitflag, output] = fminunc(f,w0,options)
