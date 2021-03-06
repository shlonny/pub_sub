%%%-------------------------------------------------------------------
%%% @author John Cartwright <jcartwright@BNA-CH0175-Mac.local>
%%% @copyright (C) 2016, John Cartwright
%%% @doc
%%%
%%% @end
%%% Created :  6 Jan 2016 by John Cartwright <jcartwright@BNA-CH0175-Mac.local>
%%%-------------------------------------------------------------------
-module(pub_sub_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(ServerName) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Key) ->
    gen_server:start_link(?MODULE, [Key], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    add_self_to_handler(Args),
    {ok, []}.

add_self_to_handler([Url]) ->
    Key = list_to_atom(Url),
    maybe_call_and_notify_handlers(gen_event:start({local, Key}), Url, Key).

maybe_call_and_notify_handlers({ok, _Pid}, Url, Key) ->
    gen_event:add_handler(Key, pub_sub_event, [self()]),
    call_and_notify_handlers(Key, Url);
maybe_call_and_notify_handlers(_, _Url, Key) ->
    gen_event:add_handler(Key, pub_sub_event, [self()]).

call_and_notify_handlers(Key, Url) ->
    case json_getter:get(Url) of
        {ok, Data} ->
            gen_event:notify(Key, Data)
    end,
    % stopping the event manager whether we successfully
    % got data or not. gen_event:stop calls terminate/2
    % on all the event handlers.
    gen_event:stop(Key).    
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({completed, _Data}, State) ->
    %% io:format("~p received ~p~n", [self(), Data]),
    %% {noreply, State};
    {stop, "Data Received", State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
