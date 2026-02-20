% Criar variáveis
syms w1 w2;

% Função
F = 1/3*w1^3+1/2*w1^2+2*w1*w2+1/2*w2^2-w2+9;

% Gradientes
grad1 = diff(F,w1);
grad2 = diff(F,w2);

% Gradiente e Hessiana
vec0 = [0;0];
grad = [grad1, grad2];

hess = [diff(grad1,w1), diff(grad1,w2);
    diff(grad2,w1), diff(grad2,w2)];

% Calculo dos Pontos Estacionários
[pe1 pe2] = solve(grad == vec0, [w1, w2]);
disp('Pontos Estacionários:');
disp(double([pe1 pe2]));

% Calcular a Hessiana para cada ponto
[n nc] = size(pe1); % nº de pontos estacionários
[dim dim1] = size(hess);

for i=1:n
    hess_ponto = double(subs(hess, {w1, w2}, {pe1(i), pe2(i)}));
    disp(['Hessiana no ponto ', num2str(i), ':']);
    disp(hess_ponto);
    for j=1:dim
        A = hess_ponto(1:j,1:j);
        d(j) = det(A);
    end
    disp('Determinantes dos submatrizes da Hessiana:');
    disp(d);
end

% Gráfico F
[w1,w2] = meshgrid(-5:0.01:5, -5:0.01:5);
f = 1/3*w1.^3+1/2*w1.^2+2*w1.*w2+1/2*w2^2-w2+9;
figure;
surfc(w1,w2,f);
colorbar;

% Adicionar cor
xlabel('w1');
ylabel('w2');
zlabel('F(w1,w2)');
title('Função F(w1,w2)');
shading interp;
hold off;