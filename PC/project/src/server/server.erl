-module(server).
-export([start/0, accept_loop/4, gestor_de_contas/1, gestor_de_partidas/3, cliente_autenticacao/4, cliente_espera/4, cliente_receptor/5, sala_de_jogo/3, gestor_de_scores/1]).
start() ->
    {ok, DbName} = dets:open_file(contas_db, [{file, "contas.dets"}, {type, set}]),
    {ok, DbScores} = dets:open_file(scores_db, [{file, "scores.dets"}, {type, set}]),
    
    GestorContasPid = spawn(?MODULE, gestor_de_contas, [DbName]),
    GestorScoresPid = spawn(?MODULE, gestor_de_scores, [DbScores]),
    GestorPartidasPid = spawn(?MODULE, gestor_de_partidas, [[], [], GestorScoresPid]), 
    
    {ok, ListenSocket} = gen_tcp:listen(8080, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]),
    io:format("Servidor Foguete.io a escutar na porta 8080...~n"),
    accept_loop(ListenSocket, GestorContasPid, GestorPartidasPid, GestorScoresPid).

accept_loop(ListenSocket, GestorContasPid, GestorPartidasPid, GestorScoresPid) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    Pid = spawn(?MODULE, cliente_autenticacao, [Socket, GestorContasPid, GestorPartidasPid, GestorScoresPid]),
    gen_tcp:controlling_process(Socket, Pid),
    accept_loop(ListenSocket, GestorContasPid, GestorPartidasPid, GestorScoresPid).

gestor_de_contas(Db) ->
    receive
        {registar, User, Pass, FromPid} ->
            case dets:lookup(Db, User) of
                [] -> dets:insert(Db, {User, Pass}), FromPid ! {resultado_registo, sucesso}, gestor_de_contas(Db);
                _ -> FromPid ! {resultado_registo, erro_ja_existe}, gestor_de_contas(Db)
            end;
        {login, User, Pass, FromPid} ->
            case dets:lookup(Db, User) of
                [{User, Pass}] -> FromPid ! {resultado_login, sucesso}, gestor_de_contas(Db);
                _ -> FromPid ! {resultado_login, erro}, gestor_de_contas(Db)
            end;
        {cancelar, User, Pass, FromPid} ->
            case dets:lookup(Db, User) of
                [{User, Pass}] -> dets:delete(Db, User), FromPid ! {resultado_cancelar, sucesso}, gestor_de_contas(Db);
                _ -> FromPid ! {resultado_cancelar, erro}, gestor_de_contas(Db)
            end
    end.

gestor_de_scores(Db) ->
    receive
        {update, User, Pontos} ->
            case dets:lookup(Db, User) of
                [{User, PtsAntigos}] when Pontos > PtsAntigos -> dets:insert(Db, {User, Pontos});
                [] -> dets:insert(Db, {User, Pontos});
                _ -> ok
            end,
            gestor_de_scores(Db);
            
        {get_top, FromPid} ->
            All = dets:foldl(fun(Elem, Acc) -> [Elem | Acc] end, [], Db),
            
            % AQUI: Filtramos a lista para manter apenas as pontuações maiores que zero (> 0)
            Filtrados = lists:filter(fun({_, Pts}) -> Pts > 0 end, All),
            
            Sorted = lists:reverse(lists:keysort(2, Filtrados)),
            Top = lists:sublist(Sorted, 5),
            FromPid ! {top_scores, Top},
            gestor_de_scores(Db)
    end.

gestor_de_partidas(FilaEspera, Salas, GestorScoresPid) ->
    receive
        {entrar_fila, Socket, User, PidCliente} ->
            case encontrar_sala_com_vaga(Salas) of
                {ok, SalaPid, NumJogadores} ->
                    io:format("~s juntou-se a uma partida em curso!~n", [User]),
                    SalaPid ! {novo_jogador, Socket, User},
                    PidCliente ! {arrancar_jogo, SalaPid},
                    NovasSalas = lists:keyreplace(SalaPid, 1, Salas, {SalaPid, NumJogadores + 1}),
                    gestor_de_partidas(FilaEspera, NovasSalas, GestorScoresPid);
                error ->
                    NovaFila = [{Socket, User, PidCliente} | FilaEspera],
                    io:format("~s entrou na fila. Total: ~w. Salas ativas: ~w/4~n", [User, length(NovaFila), length(Salas)]),
                    tentar_iniciar_partida(NovaFila, Salas, GestorScoresPid)
            end;
            
        {fim_de_partida_concluida, SalaPid} ->
            io:format("Uma partida de 2 min terminou. A limpar a sala...~n"),
            NovasSalas = lists:keydelete(SalaPid, 1, Salas),
            tentar_iniciar_partida(FilaEspera, NovasSalas, GestorScoresPid)
    end.

encontrar_sala_com_vaga([]) -> error;
encontrar_sala_com_vaga([{SalaPid, NumJogadores} | Resto]) ->
    if NumJogadores < 4 -> {ok, SalaPid, NumJogadores};
       true -> encontrar_sala_com_vaga(Resto)
    end.

tentar_iniciar_partida(FilaEspera, Salas, GestorScoresPid) when length(Salas) < 4, length(FilaEspera) >= 4 ->
    {JogadoresPartida, RestanteFila} = lists:split(4, FilaEspera),
    criar_e_arrancar_sala(JogadoresPartida, 4, RestanteFila, Salas, GestorScoresPid);

tentar_iniciar_partida(FilaEspera, Salas, GestorScoresPid) when length(Salas) < 4, length(FilaEspera) == 3 ->
    {JogadoresPartida, RestanteFila} = lists:split(3, FilaEspera),
    criar_e_arrancar_sala(JogadoresPartida, 3, RestanteFila, Salas, GestorScoresPid);

tentar_iniciar_partida(FilaEspera, Salas, GestorScoresPid) ->
    gestor_de_partidas(FilaEspera, Salas, GestorScoresPid).

criar_e_arrancar_sala(JogadoresPartida, NumJogadores, RestanteFila, Salas, GestorScoresPid) ->
    SalaPid = spawn(?MODULE, sala_de_jogo, [maps:new(), gerar_objetos(30, []), GestorScoresPid]),
    io:format("PARTIDA INICIADA! Criada nova sala com ~w jogadores.~n", [NumJogadores]),
    
    lists:foreach(fun({Socket, User, PidCliente}) ->
        SalaPid ! {novo_jogador, Socket, User},
        PidCliente ! {arrancar_jogo, SalaPid}
    end, JogadoresPartida),
    
    GestorPid = self(),
    spawn(fun() -> 
        timer:sleep(120000), 
        SalaPid ! fim_de_tempo, 
        GestorPid ! {fim_de_partida_concluida, SalaPid} 
    end),
    
    NovasSalas = [{SalaPid, NumJogadores} | Salas],
    tentar_iniciar_partida(RestanteFila, NovasSalas, GestorScoresPid).

cliente_autenticacao(Socket, GestorContasPid, GestorPartidasPid, GestorScoresPid) ->
    receive
        {tcp, Socket, Dados} ->
            Str = string:trim(binary_to_list(Dados)), Partes = string:tokens(Str, ","),
            case Partes of
                ["GET_HIGHSCORES"] ->
                    GestorScoresPid ! {get_top, self()},
                    receive
                        {top_scores, TopScores} ->
                            Resp = lists:foldl(fun({U, S}, Acc) -> Acc ++ "," ++ U ++ "," ++ integer_to_list(S) end, "HIGHSCORES", TopScores) ++ "\n",
                            gen_tcp:send(Socket, list_to_binary(Resp))
                    end,
                    cliente_autenticacao(Socket, GestorContasPid, GestorPartidasPid, GestorScoresPid);
                ["REGISTER", User, Pass] ->
                    GestorContasPid ! {registar, User, Pass, self()},
                    receive {resultado_registo, sucesso} -> gen_tcp:send(Socket, <<"REG_OK\n">>); _ -> gen_tcp:send(Socket, <<"REG_FAIL\n">>) end,
                    cliente_autenticacao(Socket, GestorContasPid, GestorPartidasPid, GestorScoresPid);
                ["LOGIN", User, Pass] ->
                    GestorContasPid ! {login, User, Pass, self()},
                    receive
                        {resultado_login, sucesso} -> 
                            gen_tcp:send(Socket, <<"LOGIN_OK\n">>),
                            GestorPartidasPid ! {entrar_fila, Socket, User, self()},
                            % --- AQUI: Passamos os 4 argumentos ---
                            cliente_espera(Socket, GestorContasPid, GestorPartidasPid, GestorScoresPid);
                        {resultado_login, erro} -> 
                            gen_tcp:send(Socket, <<"LOGIN_FAIL\n">>),
                            cliente_autenticacao(Socket, GestorContasPid, GestorPartidasPid, GestorScoresPid)
                    end;
                ["CANCEL", User, Pass] ->
                    GestorContasPid ! {cancelar, User, Pass, self()},
                    receive {resultado_cancelar, sucesso} -> gen_tcp:send(Socket, <<"CANCEL_OK\n">>); _ -> gen_tcp:send(Socket, <<"CANCEL_FAIL\n">>) end,
                    cliente_autenticacao(Socket, GestorContasPid, GestorPartidasPid, GestorScoresPid);
                _ -> cliente_autenticacao(Socket, GestorContasPid, GestorPartidasPid, GestorScoresPid)
            end;
        {tcp_closed, Socket} -> ok
    end.

cliente_espera(Socket, GestorContasPid, GestorPartidasPid, GestorScoresPid) ->
    receive
        {arrancar_jogo, SalaPid} ->
            gen_tcp:send(Socket, <<"MATCH_START\n">>),
            cliente_receptor(Socket, SalaPid, GestorContasPid, GestorPartidasPid, GestorScoresPid);
        {tcp_closed, _Socket} ->
            ok 
    end.

cliente_receptor(Socket, SalaPid, GestorContasPid, GestorPartidasPid, GestorScoresPid) ->
    receive
        {tcp, Socket, Dados} ->
            Str = string:trim(binary_to_list(Dados)),
            % --- AQUI: Lemos a nova instrução para voltar ao menu ---
            case Str of
                "BACK_TO_MENU" ->
                    cliente_autenticacao(Socket, GestorContasPid, GestorPartidasPid, GestorScoresPid);
                _ ->
                    Up = string:str(Str, "UP") > 0, Left = string:str(Str, "LEFT") > 0, Right = string:str(Str, "RIGHT") > 0,
                    SalaPid ! {teclas, Socket, {Up, Left, Right}},
                    cliente_receptor(Socket, SalaPid, GestorContasPid, GestorPartidasPid, GestorScoresPid)
            end;
        {tcp_closed, Socket} ->
            SalaPid ! {remover_jogador, Socket}
    end.

sala_de_jogo(Jogadores, Objetos, GestorScoresPid) ->
    receive
        fim_de_tempo ->
            % O SCORE AGORA É BASEADO NO NÚMERO DE KILLS
            Scores = maps:fold(fun(_Sock, {User, _, _, _, _, _, _, _, Kills, _}, Acc) ->
                [{User, Kills} | Acc]
            end, [], Jogadores),
            SortedScores = lists:reverse(lists:keysort(2, Scores)),
            
            ResultStr = lists:foldl(fun({U, S}, Acc) -> Acc ++ "," ++ U ++ "," ++ integer_to_list(S) end, "MATCH_RESULTS", SortedScores) ++ "\n",
            ResultBin = list_to_binary(ResultStr),
            
            maps:foreach(fun(Socket, {User, _, _, _, _, _, _, _, Kills, _}) -> 
                GestorScoresPid ! {update, User, Kills}, 
                gen_tcp:send(Socket, ResultBin) 
            end, Jogadores),
            ok;
            
        {novo_jogador, Socket, User} ->
            % ADICIONADO O ZERO '0' (Kills Iniciais)
            NovoEstado = {User, rand:uniform(700)+50.0, rand:uniform(500)+50.0, 0.0, 0.0, 0.0, 0.0, 10.0, 0, {false, false, false}},
            sala_de_jogo(maps:put(Socket, NovoEstado, Jogadores), Objetos, GestorScoresPid);
            
        {remover_jogador, Socket} ->
            sala_de_jogo(maps:remove(Socket, Jogadores), Objetos, GestorScoresPid);
            
        {teclas, Socket, NovasTeclas} ->
            case maps:find(Socket, Jogadores) of
                {ok, {User, X, Y, Vx, Vy, Ang, VAng, Massa, Kills, _}} ->
                    sala_de_jogo(maps:put(Socket, {User, X, Y, Vx, Vy, Ang, VAng, Massa, Kills, NovasTeclas}, Jogadores), Objetos, GestorScoresPid);
                error -> sala_de_jogo(Jogadores, Objetos, GestorScoresPid)
            end
            
    after 16 ->
        {JogadoresMovidos, ObjetosFinais} = maps:fold(fun(Socket, {User, X, Y, Vx, Vy, Ang, VAng, Massa, Kills, {Up, Left, Right} = Teclas}, {AccJogadores, AccObjetos}) ->
            AcelLinear = 4.23 / Massa / 2.0, AcelAngular = 2.13 / Massa / 2.0,
            VA1 = case Left of true -> VAng - AcelAngular; false -> VAng end,
            VA2 = case Right of true -> VA1 + AcelAngular; false -> VA1 end,
            VAFinal = VA2 * 0.85, NovoAng = Ang + VAFinal,
            {Vx1, Vy1} = case Up of true -> {Vx + AcelLinear * math:cos(NovoAng), Vy + AcelLinear * math:sin(NovoAng)}; false -> {Vx, Vy} end,
            VxFinal = Vx1 * 0.95, VyFinal = Vy1 * 0.95,
            NovoX = X + VxFinal, NovoY = Y + VyFinal, Raio = (Massa * 2.5) / 2.0,
            {FinalX, FinalVx} = if NovoX - Raio =< 0.0 -> {Raio, 0.0}; NovoX + Raio >= 800.0 -> {800.0 - Raio, 0.0}; true -> {NovoX, VxFinal} end,
            {FinalY, FinalVy} = if NovoY - Raio =< 0.0 -> {Raio, 0.0}; NovoY + Raio >= 600.0 -> {600.0 - Raio, 0.0}; true -> {NovoY, VyFinal} end,
            
            {NovaMassaCrua, NovosObjetos} = verificar_colisoes(FinalX, FinalY, Raio, Massa, AccObjetos, []),
            NovoEstado = if
                NovaMassaCrua < 10.0 -> {User, rand:uniform(700)+50.0, rand:uniform(500)+50.0, 0.0, 0.0, 0.0, 0.0, 10.0, Kills, Teclas}; % Mantém as kills se morrer para um objeto vermelho
                true -> {User, FinalX, FinalY, FinalVx, FinalVy, NovoAng, VAFinal, NovaMassaCrua, Kills, Teclas}
            end,
            {maps:put(Socket, NovoEstado, AccJogadores), NovosObjetos}
        end, {maps:new(), Objetos}, Jogadores),

        JogadoresAtualizados = resolver_colisoes_jogadores(JogadoresMovidos),

        SocketsList = maps:keys(JogadoresAtualizados),
        ObjetosGarantidos = if
            length(SocketsList) > 0 ->
                MinMassa = lists:foldl(fun(S, Acc) -> {_, _, _, _, _, _, _, M, _, _} = maps:get(S, JogadoresAtualizados), min(M, Acc) end, 99999.0, SocketsList),
                MinTamJogador = MinMassa * 2.5,
                TemPequeno = lists:any(fun({_, _, TamObj, TipoObj}) -> (TipoObj == 1) andalso (TamObj < MinTamJogador) end, ObjetosFinais),
                if not TemPequeno ->
                        TamGarantido = max(3.0, MinTamJogador - 3.0), 
                        ObjGarantido = {rand:uniform(700)+50.0, rand:uniform(500)+50.0, TamGarantido, 1},
                        [_H | T] = ObjetosFinais, [ObjGarantido | T];
                    true -> ObjetosFinais
                end;
            true -> ObjetosFinais
        end,

        maps:foreach(fun(MeuSocket, {MeuUser, MeuX, MeuY, _, _, MeuAng, _, MeuMassa, MeuKills, _}) ->
            MeuTamanho = MeuMassa * 2.5,
            % Envia o número de Kills na mensagem 'STATE'
            gen_tcp:send(MeuSocket, list_to_binary(io_lib:format("STATE,~s,~f,~f,~f,~f,~w\n", [MeuUser, MeuX, MeuY, MeuTamanho, MeuAng, MeuKills]))),
            
            maps:foreach(fun(OutroSocket, {OutroUser, OutroX, OutroY, _, _, OutroAng, _, OutroMassa, _, _}) ->
                if OutroSocket =/= MeuSocket -> 
                    OutroTamanho = OutroMassa * 2.5,
                    gen_tcp:send(MeuSocket, list_to_binary(io_lib:format("OTHER,~s,~f,~f,~f,~f\n", [OutroUser, OutroX, OutroY, OutroTamanho, OutroAng])));
                true -> ok end
            end, JogadoresAtualizados),
            enviar_objetos(MeuSocket, ObjetosGarantidos),
            gen_tcp:send(MeuSocket, <<"SYNC\n">>)
        end, JogadoresAtualizados),

        sala_de_jogo(JogadoresAtualizados, ObjetosGarantidos, GestorScoresPid)
    end.

resolver_colisoes_jogadores(Jogadores) -> pares_colisao(maps:keys(Jogadores), Jogadores).
pares_colisao([], Jogadores) -> Jogadores;
pares_colisao([S1 | Resto], Jogadores) -> pares_colisao(Resto, lists:foldl(fun(S2, Acc) -> colidir_dois_jogadores(S1, S2, Acc) end, Jogadores, Resto)).

colidir_dois_jogadores(S1, S2, Jogadores) ->
    case {maps:find(S1, Jogadores), maps:find(S2, Jogadores)} of
        {{ok, P1}, {ok, P2}} ->
            {U1, X1, Y1, Vx1, Vy1, A1, VA1, M1, K1, T1} = P1, 
            {U2, X2, Y2, Vx2, Vy2, A2, VA2, M2, K2, T2} = P2,
            R1 = (M1 * 2.5) / 2.0, R2 = (M2 * 2.5) / 2.0,
            Dist = math:sqrt(math:pow(X2-X1, 2) + math:pow(Y2-Y1, 2)),
            if (M1 > M2) andalso ((Dist + R2) < R1) ->
                    % P1 ganha 1 Kill (K1 + 1). P2 mantém as Kills (K2) mas volta a nascer.
                    maps:put(S2, {U2, rand:uniform(700)+50.0, rand:uniform(500)+50.0, 0.0, 0.0, 0.0, 0.0, 10.0, K2, {false,false,false}}, 
                    maps:put(S1, {U1, X1, Y1, Vx1, Vy1, A1, VA1, M1 + (M2 / 4.0), K1 + 1, T1}, Jogadores));
               (M2 > M1) andalso ((Dist + R1) < R2) ->
                    % P2 ganha 1 Kill (K2 + 1).
                    maps:put(S2, {U2, X2, Y2, Vx2, Vy2, A2, VA2, M2 + (M1 / 4.0), K2 + 1, T2}, 
                    maps:put(S1, {U1, rand:uniform(700)+50.0, rand:uniform(500)+50.0, 0.0, 0.0, 0.0, 0.0, 10.0, K1, {false,false,false}}, Jogadores));
               true -> Jogadores
            end;
        _ -> Jogadores
    end.

gerar_um_objeto() -> Tamanho = rand:uniform(50) + 5.0, Raio = Tamanho / 2.0, {Raio + rand:uniform(round(800.0 - Tamanho)), Raio + rand:uniform(round(600.0 - Tamanho)), Tamanho, rand:uniform(2)}.
gerar_objetos(0, Lista) -> Lista;
gerar_objetos(N, Lista) -> gerar_objetos(N - 1, [gerar_um_objeto() | Lista]).
verificar_colisoes(_Px, _Py, _PRaio, Massa, [], Sobreviventes) -> {Massa, Sobreviventes};
verificar_colisoes(Px, Py, PRaio, Massa, [{Ox, Oy, OTamanho, Tipo} = Obj | Resto], Sobreviventes) ->
    ORaio = OTamanho / 2.0, Distancia = math:sqrt(math:pow(Px - Ox, 2) + math:pow(Py - Oy, 2)),
    case Tipo of
        2 -> if Distancia < (PRaio + ORaio) -> verificar_colisoes(Px, Py, PRaio, Massa - (OTamanho / 2.5), Resto, [gerar_um_objeto() | Sobreviventes]); true -> verificar_colisoes(Px, Py, PRaio, Massa, Resto, [Obj | Sobreviventes]) end;
        1 -> if (Distancia + ORaio) < PRaio -> verificar_colisoes(Px, Py, PRaio, Massa + (OTamanho / 2.5), Resto, [gerar_um_objeto() | Sobreviventes]); true -> verificar_colisoes(Px, Py, PRaio, Massa, Resto, [Obj | Sobreviventes]) end
    end.
enviar_objetos(_Socket, []) -> ok;
enviar_objetos(Socket, [{X, Y, Tamanho, Tipo} | Resto]) -> gen_tcp:send(Socket, list_to_binary(io_lib:format("OBJ,~f,~f,~f,~w\n", [X, Y, Tamanho, Tipo]))), enviar_objetos(Socket, Resto).