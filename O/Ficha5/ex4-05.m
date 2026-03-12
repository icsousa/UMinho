clear,clc,close all;

syms w1 
F = w1^2-w1^4/4;
grad = gradient(F,w1);
hess = hessian(F,w1);

%w0=sqrt(2/5);
w0 = 0.5;
epsilon = 0.000001;
Kmax = 20;
[w_opt,Fval_opt,output] = MN(F,grad,hess,w0,epsilon,Kmax);

w = linspace(-1,1,100);
vals_F = w.^2 - (w.^4)/4;

figure;
plot(w,vals_F, 'LineWidth',2);
hold on;
vals_w = output(:,2);
vals_F = output(:,3);
plot(vals_w, 0, 'r*', 'LineWidth', 1.5);
plot(vals_w, vals_F, 'ko--', 'LineWidth', 0.5);
title('Gráfico de F, pontos gerados pelo MN e trajetória');
grid on;

function [w_opt,Fval_opt,output] = MN(F,grad,hess,w0,epsilon,Kmax)
    syms w1 

    k = 0;
    wk = w0;
    output = [];
    etak = 1;
    while(k <= Kmax)
        gradk = double(subs(grad,[w1],[wk(1)]));
        hessk = double(subs(hess,[w1],[wk(1)]));
        norma = norm(gradk,inf);
        if (norma <= epsilon)
            w_opt = wk;
            Fk = double(subs(F,[w1],[wk(1)]));
            [k wk' Fk gradk' etak norma]
            output = [output;k, wk', Fk, gradk', etak, norma];
            break;
        end
        sk = -hessk\gradk;
        Fk = double(subs(F,[w1],[wk(1)]));
        [k, wk', Fk, gradk', etak, norma]
        output = [output;k, wk', Fk, gradk', etak, norma]
        wk = wk+etak*sk;
        k = k+1;
    end
    w_opt = wk;
    Fval_opt = Fk;
end


