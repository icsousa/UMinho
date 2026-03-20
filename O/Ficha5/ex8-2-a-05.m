clear, clc, close all;

options = optimoptions('fminunc', 'Display', 'iter', 'SpecifyObjectiveGradient', true);

% F2
w0 =  [-1.2;1];
[wopt,Fopt, exitflag, output] = fminunc(@F2withgrad,w0,options)