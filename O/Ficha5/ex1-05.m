clear, clc, close all;
syms w1 w2;
beta = 15;
F = w1^2 + beta*w2^2;
grad = gradient(F, [w1, w2]);
w0 = [10; 1];
epsilon = 0.000001;
Kmax = 200;

[w_opt, Fval_opt, output] = MDM_procuraExata(F, grad, w0, epsilon);

% Visualizar os resultados finais da matriz de output
fprintf('     k         w1         w2         F(w)       eta_k      ||grad||_inf\n');
disp(output);

[w1,w2] = meshgrid(-5:0.01:10, -5:0.01:5);
vals_F = w1.^2 + beta*w2.^2;

figure;
contour(w1,w2,vals_F,10);
colorbar;

xlabel('w_1');
ylabel('w_2');
hold on;
vals_w1 = output(:,2);
vals_w2 = output(:,3);

plot(vals_w1, vals_w2, 'r*-.', 'LineWidth', 1.5, 'MarkerFaceColor', 'r', 'MarkerSize', 5);

text(vals_w1(1), vals_w2(1)+0.2, 'w0')
text(vals_w1(end), vals_w2(end)+0.5, 'w*')
title('Trajetória dos Pontos');
grid on;

function [w_opt, Fval_opt, output] = MDM_procuraExata(F, grad, w0, epsilon)
    syms w1 w2 eta;
        
    k = 0;
    Kmax = 200;
    wk = w0;
    output = [];    
    
    while (k <= Kmax)
        gradk = double(subs(grad, [w1, w2], [wk(1), wk(2)]));
        
        % Normas (Norma 2 para o if, Norma inf para a tabela)
        norma2 = norm(gradk, 2);
        norma_inf = norm(gradk, inf);
        
        Fk = double(subs(F, [w1, w2], [wk(1), wk(2)]));
        
        % Critério de paragem
        if (norma2 <= epsilon)
            w_opt = wk;
            Fval_opt = Fk;
            % Se parar logo na iteração 0, o eta é 0 (apenas para não dar erro)
            if k == 0, etak = 0; end 
            
            output = [output; k, wk(1), wk(2), Fk, etak, norma_inf];
            break;
        end
        
        sk = -gradk;
        waux = wk + eta*sk;
        
        phi = subs(F, [w1, w2], [waux(1), waux(2)]);
        
        % Calcular os pontos estacionarios
        grad_phi = gradient(phi);
        sol_pe = double(solve(grad_phi == 0, eta));
        vals_phi = double(subs(phi, eta, sol_pe));
        
        [~, index] = min(vals_phi);
        etak = sol_pe(index);
        
        % Guardar dados desta iteração na matriz output
        output = [output; k, wk(1), wk(2), Fk, etak, norma_inf];
        
        wk = wk + etak * sk;
        k = k + 1;
    end
end