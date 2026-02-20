%% a)
% Domínio
[w1, w2] = meshgrid(0:0.01:1.5, 0:0.01:1.5);

% Função escalar
F = w1.^3 + 2*w1.*w2.^2 - w2.^3 - 20*w1;

% Condições
c1 = (w1.^2 + w2.^2 <= 1);       
c2 = (w1 - 3*w2 + 0.5 >= 0);     
c3 = (w1 >= 0);                  
c4 = (w2 >= 0);                  

% Região Admissível (interseção de todas as restrições)
Regiao_Admissivel = c1 & c2 & c3 & c4;

% Aplicar a máscara à função objetivo (coloca NaN fora da região admissível)
F_valida = F;
F_valida(~Regiao_Admissivel) = NaN;

figure; hold on; box on;
% Curvas de nível
contour(w1, w2, F_valida, 30, 'LineWidth', 1.2);

% Desenhar as fronteiras das restrições para visualização
fimplicit(@(x,y) x.^2 + y.^2 - 1, [0 1.5 0 1.5], 'r', 'LineWidth', 1.5, 'DisplayName', 'w_1^2+w_2^2 = 1');
fimplicit(@(x,y) x - 3*y + 0.5, [0 1.5 0 1.5], 'b', 'LineWidth', 1.5, 'DisplayName', 'w_1-3w_2+0.5 = 0');

% Encontrar o mínimo graficamente (o menor valor dentro da matriz F_valida)
[min_val, idx] = min(F_valida(:));
[row, col] = ind2sub(size(F_valida), idx);
w_min_grafico = [w1(1, col), w2(row, 1)];

% Assinalar o mínimo no gráfico
plot(w_min_grafico(1), w_min_grafico(2), 'k*', 'MarkerSize', 8, 'LineWidth', 2, 'DisplayName', 'Mínimo Gráfico');
title('Região Admissível e Curvas de Nível');
xlabel('w_1'); ylabel('w_2');
legend('Location', 'northeast');

%% c)
% Função (forma vetorial)
f = @(w) w(1)^3 + 2*w(1)*w(2)^2 - w(2)^3 - 20*w(1);

% Ponto inicial
w0 = [0.5; 0.1];

% Limites Simples: w1 >= 0, w2 >= 0
lb = [0; 0];
ub = []; % Sem limites superiores

% Restrição Linear: w1 - 3*w2 + 0.5 >= 0  <=>  -w1 + 3*w2 <= 0.5
A = [-1, 3];
b = 0.5;

% Restrição Não Linear: w1^2 + w2^2 <= 1  <=>  w1^2 + w2^2 - 1 <= 0
nonlcon = @(w) deal(w(1)^2 + w(2)^2 - 1, []); % deal(c, ceq) -> ceq é vazio pois não há igualdades

% Configurar opções do solver
options = optimoptions('fmincon', 'Display', 'iter', 'Algorithm', 'interior-point');

% Chamada ao fmincon pedindo o output "lambda" para ver restrições ativas
[xopt, fopt, exitflag, output, lambda] = fmincon(f, w0, A, b, [], [], lb, ub, nonlcon, options)