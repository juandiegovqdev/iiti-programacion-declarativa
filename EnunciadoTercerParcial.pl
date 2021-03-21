%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PD. Grado en Informatica. Tecnologias Informaticas. CURSO 2013-14
%% PRUEBA 3 DE EVALUACION ALTERNATIVA (17 ENERO 2014)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NOMBRE Y APELLIDOS:
%% GRUPO:
%% EMAIL:
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EJERCICIO 2:
%% Consideremos definida la siguiente relacion, sobre los números naturales:

esValido(0).
esValido(5).
esValido(6).
esValido(12).
esValido(13).
esValido(15).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (a. 0.5 ptos) Definir la relación validos(N,L) que se verifique si
%% L es una lista no decreciente de N números validos. Por ejemplo,
%%        ?- validos(2,L).
    %%        L = [0,0] ;
%%        L = [0,5] ;
%%        L = [0,6] ;
%%        ...
    %%        L = [6,15] ;
%%        ...
    %%        L = [13,15] ;
%%        L = [15,15] ;
%%        No
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (b. 0.25 ptos) Definir la relación suma_lista(L,N) que se verifique
%% si N es la suma de los elementos de la lista de números naturales
%% L. Por ejemplo:
%%        ?- suma_lista([1,2,3,2,3],N).
%%        N = 11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (c. 0.25 ptos) Definir la relación solucion(L) que se verifique si L es una
%% lista de 7 números validos que suman 100.
%% Calcular todas las soluciones.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EJERCICIO 3 (0.25 ptos):
%% En este ejercicio se usa la representación de los números naturales
    %% construidos con 0 y s; es decir, la sucesión
%%    0, s(0), s(s(0)), s(s(s(0))), ...
%% representa a la sucesión de los números naturales
%%    0, 1, 2, 3, ...

%% Definir la relación numero(X, N) que traduce la representación
%% anterior a la habitual. Por ejemplo,
%%        ?- numero(s(0), X).
%%        X = 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
