function [etak] = ArmijoBacktracking(Fwithgrad, Fk, gradk, wk, sk)

c = 0.0001;
rho = 0.5;

eta0 = 1;

eta = eta0;

wuax = wk + eta * sk;
[Faux, ~] = Fwithgrad(waux);
while (Faux > Fk + c*eta*(gradk'*sk))
    eta = rho * eta;
    wuax = wk + eta * sk;
    [Faux, ~] = Fwithgrad(wuax);
end
etak = eta;
end