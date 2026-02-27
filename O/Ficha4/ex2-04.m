clear; clc; close all;

% 1. Definir a função objetivo
F = @(w) 0.1*w(1)^6 - 1.5*w(1)^4 + 5*w(1)^2 + 0.1*w(2)^4 + ...
         3*w(2)^2 - 9*w(2) + 0.5*w(1)*w(2);

% 2. Definir o gradiente da função (derivadas parciais calculadas à mão)
gradF = @(w) [0.6*w(1)^5 - 6*w(1)^3 + 10*w(1) + 0.5*w(2);
              0.4*w(2)^3 + 6*w(2) - 9 + 0.5*w(1)];

% 3. Parâmetros iniciais dados no problema
w = [-1.25; 1.25];
s = [4; 0.75];
c1 = 1e-4;
rho = 0.7;

% Pré-cálculos no ponto w
F_w = F(w);
gradF_w = gradF(w);
derivada_direcional = gradF_w' * s;

fprintf('Derivada direcional no ponto w: %.4f\n', derivada_direcional);
if derivada_direcional < 0
    fprintf('(Como é negativa, confirmamos que s é direção de descida)\n');
end

%% a) e b)
eta_0_list = [1.2, 0.05];

for i = 1:length(eta_0_list)
    eta = eta_0_list(i);
    iter = 0;
    
    fprintf('\n Teste com eta_0 = %.2f \n', eta);
    
    % Ciclo de Backtracking (Condição de Armijo)
    % Verifica se: F(w + eta*s) > F(w) + c1 * eta * (gradF'*s)
    while F(w + eta*s) > F_w + c1 * eta * derivada_direcional
        eta = rho * eta; % Reduzir o passo
        iter = iter + 1; % Contar a iteração
    end
    
    fprintf('Iterações de backtracking necessárias: %d\n', iter);
    fprintf('Comprimento de passo final: %.6f\n', eta);
end

%% a) e c)
% Criar um vetor de passos (eta) para desenhar a curva
etas = linspace(0, 1.5, 100);
F_etas = zeros(size(etas));
Armijo_line = zeros(size(etas));

% Calcular os valores da função e da reta de Armijo para cada eta
for i = 1:length(etas)
    F_etas(i) = F(w + etas(i)*s);
    Armijo_line(i) = F_w + c1 * etas(i) * derivada_direcional;
end

% Criar a figura
figure;
plot(etas, F_etas, 'b', 'LineWidth', 2); 
hold on;
plot(etas, Armijo_line, 'r--', 'LineWidth', 2);

% Formatações
title('Condição de Armijo e Função \phi(\eta)');
xlabel('Tamanho do passo (\eta)');
ylabel('Valor da Função');
legend('\phi(\eta) = F(w + \eta s)', 'Reta de decréscimo suficiente (Armijo)', 'Location', 'northwest');
grid on;
ylim([-10, 30]);
