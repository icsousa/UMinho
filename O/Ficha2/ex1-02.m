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

% Gráfico 3D
figure('Name', 'Superfície 3D - Exercício 1', 'Color', 'w');
surf(w1,w2,f);
shading interp; 
colormap(parula); 
colorbar;
camlight left; 
lighting gouraud; 
alpha 0.9;
xlabel('w_1', 'FontSize', 12, 'FontWeight', 'bold');
ylabel('w_2', 'FontSize', 12, 'FontWeight', 'bold');
zlabel('F(w_1, w_2)', 'FontSize', 12, 'FontWeight', 'bold');
title('Superfície 3D da Função Objetivo', 'FontSize', 14);
view(-45, 35);
grid on;

% fminunc
options = optimoptions('fminunc')

% Ponto Inicial
w0 = [1;1];

% Definir a Função
f = @(w)w(1)^3+2*w(2)^2-w(2)^3-20*w(1);

% Minimização da Função f
[xopt, fopt, exitflag, output] = fminunc(f,w0,options)
