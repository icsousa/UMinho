clear, clc, close all;

options = optimoptions('fminunc', 'Display', 'iter');

% F1
w0 =  [-1.2;1];
[wopt,Fopt, exitflag, output] = fminunc(@F1withoutgrad,w0,options)