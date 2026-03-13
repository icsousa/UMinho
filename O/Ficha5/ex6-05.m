clear, clc, close all;

% a)
% syms w1
% vars = [w1];
% F = w1^2 - (w1^4)/4;
% w0 = [sqrt(2/5)];

% b)
% syms w1 w2
% vars = [w1, w2];
% F = sqrt(w1^2 + 1) + sqrt(w2^2 + 1);
% w0 = [1; 1];

% c)
syms w1 w2
vars = [w1, w2];
F = (1 - w1)^2 + (1 - w2)^2 + 0.5*(2*w2 - w1^2)^2; 
w0 = [-2; 1];

epsilon = 1e-6; % Tolerância
Kmax = 200;     % Máximo de iterações
c = 0.0001;     % Parâmetro de Armijo (c)
rho = 0.5;      % Fator de redução (rho) para backtracking

% Calcula o Gradiente e a Hessiana simbolicamente
grad = gradient(F, vars);
hess = hessian(F, vars);

[w_opt, Fval_opt, output] = MN_Armijo(F, grad, hess, vars, w0, epsilon, Kmax, c, rho);

% Exibir os resultados finais
disp('Solução ótima encontrada (w*):');
disp(w_opt);
disp('Valor da função no ponto ótimo F(w*):');
disp(Fval_opt);
disp('Número de iterações realizadas:');
disp(size(output, 1));

function [w_opt, Fval_opt, output] = MN_Armijo(F, grad, hess, vars, w0, epsilon, Kmax, c, rho)
    k = 0;
    wk = w0;
    output = [];
    
    while (k <= Kmax)
        % 1. Avaliar gradiente, hessiana e F no ponto atual wk
        if length(vars) == 1
            gradk = double(subs(grad, vars(1), wk(1)));
            hessk = double(subs(hess, vars(1), wk(1)));
            Fk    = double(subs(F, vars(1), wk(1)));
        else
            % Transpor wk para condizer com o vetor vars se necessário
            gradk = double(subs(grad, vars, wk'));
            hessk = double(subs(hess, vars, wk'));
            Fk    = double(subs(F, vars, wk'));
        end
        
        norma = norm(gradk, 2); % Critério da norma 2
        
        % 2. Verificação do Critério de Paragem
        if (norma <= epsilon)
            w_opt = wk;
            Fval_opt = Fk;
            % Guardar a última iteração
            output = [output; k, wk', Fk, gradk', 0, norma];
            break;
        end
        
        % 3. Cálculo da Direção de Newton (sk)
        sk = -hessk \ gradk;
        dir_deriv = gradk' * sk;
        
        % 4. Procura de Armijo com Backtracking
        etak = 1;
        wk_next = wk + etak * sk;
        
        if length(vars) == 1
            Fk_next = double(subs(F, vars(1), wk_next(1)));
        else
            Fk_next = double(subs(F, vars, wk_next'));
        end
        
        % Condição de Armijo
        while Fk_next > Fk + c * etak * dir_deriv
            etak = rho * etak; % Reduzir o passo
            wk_next = wk + etak * sk;
            
            % Reavaliar a função no novo ponto
            if length(vars) == 1
                Fk_next = double(subs(F, vars(1), wk_next(1)));
            else
                Fk_next = double(subs(F, vars, wk_next'));
            end
        end
        
        % 5. Registar dados da iteração e avançar
        output = [output; k, wk', Fk, gradk', etak, norma];
        wk = wk_next;
        k = k + 1;
    end
    
    w_opt = wk;
    
    % Garantir o cálculo final do valor ótimo de F
    if length(vars) == 1
        Fval_opt = double(subs(F, vars(1), wk(1)));
    else
        Fval_opt = double(subs(F, vars, wk'));
    end
end