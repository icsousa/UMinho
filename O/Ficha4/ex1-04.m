% Limpar ambiente
clear; clc; close all;

% Definir variáveis simbólicas
syms w1 w2 eta real

% Definir a função F(w1, w2)
F(w1, w2) = (w1 + w2^2)^2;

% Dados do problema
w = [1; 0];
s = [-1; 1];

% a. Calcular o gradiente simbólico de F
gradF = gradient(F, [w1, w2]);

% Avaliar o gradiente no ponto w = (1, 0)
% gradF_w = double(gradF(w(1), w(2)));
% ou
gradF_w = subs(gradF, [w1,w2], [1,0]);

% Verificar o produto interno (gradiente transposto * s)
produto_interno = s' * gradF_w;

fprintf('Produto interno do gradF(w)^T * s = %d\n', produto_interno);
if produto_interno < 0
    fprintf('Como o resultado é menor que zero, s é uma direção de descida.\n\n');
else
    fprintf('A direção não é de descida.\n\n');
end


% b. 
% Criar a função objetivo dependente apenas de eta (phi)
phi = subs(F, [w1,w2], [1-eta,eta]);

% Calcular a derivada de F
grad_phi = gradient(phi,eta);

% Encontrar os 0 de phi
pontos_criticos = solve(grad_phi);

% Calcular a segunda derivada de phi
hess_phi = hessian(phi, eta);

% Filtrar para os valores onde eta > 0
minimizantes = double(pontos_criticos(pontos_criticos > 0));

fprintf('Os minimizantes (eta > 0) são:\n');
disp(minimizantes);


% c. 
% Criar a figura
figure;

% Desenhar função phi
etas = linspace(0,5,100);
vet_phi = (1-etas + etas.^2).^2;
plot(etas, vet_phi,'Color', 'b', 'LineWidth', 2);
hold on;

% Calcular a coordenada y do(s) mínimo(s)
y_min = double(subs(phi, eta, minimizantes));

% Marcar o ponto mínimo no gráfico
plot(minimizantes, y_min, 'r*', 'MarkerSize', 10, 'LineWidth', 2);

% Formatações do gráfico
title('Gráfico de F ao longo da reta w + \eta s');
xlabel('Tamanho do passo (\eta)');
ylabel('\phi(\eta) = F(w + \eta s)');

% Criar uma legenda dinâmica que lê o valor calculado em vez de texto fixo
texto_legenda = sprintf('Mínimo global (\\eta = %.1f)', minimizantes(1));
legend('\phi(\eta)', texto_legenda);
grid on;