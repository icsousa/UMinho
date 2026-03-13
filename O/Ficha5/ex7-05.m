clear, clc, close all;

problema_id = 1; 

syms w1 w2 w3
% Definição dos problemas conforme o enunciado
switch problema_id
    case 1
        % Problema 1
        F_sym = 100*(w2 - w1^2)^2 + (1 - w1)^2;
        w0 = [-1.2; 1];
        vars = [w1; w2];
    case 2
        % Problema 2
        F_sym = 4*w1^2 + 2*w2^2 + 4*w1*w2 - 3*w1;
        w0 = [2; 2];
        vars = [w1; w2];
    case 3
        % Problema 3
        F_sym = w1^2 + 2*w2^2 - 2*w1*w2 - 2*w2;
        w0 = [0; 0];
        vars = [w1; w2];
    case 4
        % Problema 4
        F_sym = (w1 + w2)^4 + w2^2;
        w0 = [2; -2];
        vars = [w1; w2];
    case 5
        % Problema 5
        F_sym = 0.5*(2*w1^2 + 3*w2^2 + 4*w3^2) + 8*w1 + 9*w2 + 8*w3;
        w0 = [0; 0; 0];
        vars = [w1; w2; w3];
    case 6
        % Problema 6 
        F_sym = 0.5*(5*w1^2 + 7*w2^2 + 9*w3^2 + 4*w1*w2 + 2*w1*w3 + 6*w2*w3) + 9*w1 + 8*w3;
        w0 = [0; 0; 0];
        vars = [w1; w2; w3];
    case 7
        % Problema 7
        F_sym = w1^2 + w2^2 + w3^2;
        w0 = [1; 1; 1];
        vars = [w1; w2; w3];
    case 8
        % Problema 8
        alpha = 1; 
        F_sym = (w1 - 1)^2 + (w2 - 1)^2 + alpha*(w1^2 + w2^2 - 0.25)^2;
        w0 = [1; -1];
        vars = [w1; w2];
end

% Calcular o gradiente simbolicamente
grad_sym = gradient(F_sym, vars);

% Converter para funções anónimas do MATLAB
F_func = matlabFunction(F_sym, 'Vars', {vars});
grad_func = matlabFunction(grad_sym, 'Vars', {vars});

epsilon = 1e-6;        
Kmax = 200;             
c1 = 0.0001;            
rho = 0.5;              
gamma = 1e-12;          

[w_opt, Fval_opt, output, flag_conv] = BFGS_Method(F_func, grad_func, w0, epsilon, Kmax, c1, rho, gamma);

% Exibir Resultados

fprintf('O algoritmo convergiu em %d iterações.\n', size(output,1)-1);
disp('Solução ótima encontrada (w*):');
disp(w_opt);
disp('Valor da função no ponto ótimo F(w*):');
disp(Fval_opt);


function [w_opt, Fval_opt, output, flag_conv] = BFGS_Method(F_func, grad_func, w0, epsilon, Kmax, c1, rho, gamma)
    k = 0;
    wk = w0;
    d = length(w0);
    Hk = eye(d); % H0 = I (Aproximação inicial da inversa da Hessiana)
    
    output = [];
    flag_conv = false;
    
    while k <= Kmax
        Fk = F_func(wk);
        gradk = grad_func(wk);
        norma = norm(gradk, 2);
        
        % Guardar iteração (k, w, F(w), gradiente, passo)
        if k == 0
            output = [output; k, wk', Fk, norma, 0];
        end
        
        % Critério de Paragem
        if norma <= epsilon
            flag_conv = true;
            break;
        end
        
        % Direção de pesquisa
        pk = -Hk * gradk;
        
        % Procura unidirecional de Armijo com Backtracking 
        etak = 1; % eta_0 = 1 
        dir_deriv = gradk' * pk;
        wk_next = wk + etak * pk;
        
        while F_func(wk_next) > Fk + c1 * etak * dir_deriv
            etak = rho * etak;
            wk_next = wk + etak * pk;
        end
        
        % Atualização das variáveis
        sk = wk_next - wk;
        gradk_next = grad_func(wk_next);
        yk = gradk_next - gradk;
        
        % Estratégia de Skipping 
        ys = yk' * sk;
        
        if abs(ys) <= gamma
            % Se muito pequeno, mantém a Hessiana anterior 
            Hk_next = Hk;
        else
            % Se ys < 0 e |ys| >= gamma, usar |ys| conforme o enunciado 
            if ys < 0
                ys_val = abs(ys);
            else
                ys_val = ys;
            end
            
            % Fórmula de atualização BFGS para a inversa da Hessiana
            I = eye(d);
            rho_k = 1 / ys_val;
            Hk_next = (I - rho_k * sk * yk') * Hk * (I - rho_k * yk * sk') + rho_k * (sk * sk');
        end
        
        wk = wk_next;
        Hk = Hk_next;
        k = k + 1;
        
        output = [output; k, wk', F_func(wk), norm(gradk_next, 2), etak];
    end
    
    w_opt = wk;
    Fval_opt = F_func(wk);
end