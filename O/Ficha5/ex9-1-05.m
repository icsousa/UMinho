clear, clc, close all;

options = optimoptions('fminunc', 'Display', 'iter', 'SpecifyObjectiveGradient', true, 'HessianApproximation','steepdesc', 'MaxFunctionEvaluations', 500);

% F1
w0 =  [-1.2;1];
[wopt,Fopt, exitflag, output] = fminunc(@F1withgrad,w0,options)