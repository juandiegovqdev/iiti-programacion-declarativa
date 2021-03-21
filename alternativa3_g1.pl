% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PD CURSO 2013-14            GRUPO 1
% TERCER EXAMEN DE EVALUACION ALTERNATIVA
% 17 DE ENERO 2014
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOMBRE Y APELLIDOS:
% E-MAIL:
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EJERCICIO 1.
% Se considera la siguiente base de conocimiento
c(v,A,A).                       % C1
c(l(X,A),B,l(X,C)) :- c(A,B,C). % C2
e(X,A) :- c(_B,l(X,_C),A).        % C3
% Escribir el arbol de resolucion correspondiente a la base de conocimiento
% y a la pregunta:
% ?- e(X,l(p,l(q,v))).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EJERCICIO 2.
% Definir una funcion recursiva prod(Xs,N) que se cumpla si N es el
% producto de los numeros de la lista Xs.
%
% SOLUCION:
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EJERCICIO 3.
% Dados los siguientes elementos, definir una funcion recursiva
% creaLista(N,Xs) que se verifique si Xs es una lista creciente de N
% elementos.
%
elem(3).
elem(2).
elem(4).
elem(5).
elem(8).
elem(10).
% SOLUCION:
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EJERCICIO 4.
% 4.1 Definir una funcion buena(Xs,N) que se cumpla cuando Xs sea una
% lista de M elementos y el producto de los elementos de Xs es N.
% 4.2 Definir una consulta que devuelva todas las listas de elementos con
% producto 40.
% SOLUCION:
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
