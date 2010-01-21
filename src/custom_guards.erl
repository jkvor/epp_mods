%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(custom_guards).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
	Forms1 = [begin
				case Form of
					{function,Line,Name,Arity,Clauses} ->
						{function,Line,Name,Arity,expand_function_clauses(Clauses)};
					_ ->
						Form
				end
			 end || Form <- Forms],
	Forms1.

%% @spec expand_function_clauses(Line, Name, Arity, Clauses, Acc) -> Clauses
%%		Name = atom()
%%		Arity = integer()
%%		Clauses = list()
%%		Acc = list()
expand_function_clauses(Clauses) ->
    expand_function_clauses(Clauses, []).
expand_function_clauses([Clause|Tail], Clauses) ->
    {Required, OtherGuards} = get_required_fields(Clause),
    ExitClauses = create_exit_clauses(Clause, Required),
    %io:format("exit clauses: ~p~n", [ExitClauses]),
    OrigClause = alter_original_clause(Clause, Required, OtherGuards),
    %io:format("orig clause: ~p~n", [OrigClause]),
    Clauses1 = Clauses ++ ExitClauses ++ [OrigClause],
    %io:format("new clauses: ~p~n", [Clauses1]),
    expand_function_clauses(Tail, Clauses1);
expand_function_clauses([], Acc) -> Acc.

%% @spec get_required_fields(list()) -> [term()]
%% @doc extract a list of variables from any is_required guards
get_required_fields({clause,_,_,Guards,_}) ->
    get_required_fields(Guards);
get_required_fields(Guards) ->
    get_required_fields(Guards, {dict:new(), []}).
get_required_fields([{call,_,{atom,_,is_required},Args}|Tail], {Dict,NewGuards}) ->
	{Fields,Opts} =
		case Args of
			[Cons0,Opts0] -> {cons_to_list(Cons0),convert_to_proplist(Opts0)};
			[Cons0] -> {cons_to_list(Cons0),[]}
		end,
	Dict1 = lists:foldl(fun(K,Acc) -> dict:store(K,Opts,Acc) end, Dict, Fields),
    get_required_fields(Tail, {Dict1,NewGuards});
get_required_fields([SubList|Tail], Acc) when is_list(SubList) ->
    get_required_fields(Tail, get_required_fields(SubList, Acc));
get_required_fields([Other|Tail], {Acc,NewGuards}) ->
    get_required_fields(Tail, {Acc,[Other|NewGuards]});
get_required_fields([], Acc) -> Acc.

%% @spec convert_to_proplist(Cons) -> list()
convert_to_proplist(Cons) ->
	convert_to_proplist(Cons, []).
convert_to_proplist({cons, _, {tuple, _, [{atom, _, reject}, Cons]}, {nil, _}}, Acc) ->
	Values = cons_to_list(Cons),
	[{reject, Values}|Acc];
convert_to_proplist({cons,_,{tuple,_,[{atom,_,mode},{atom,_,Mode}]}, {nil,_}}, Acc) ->
    [{mode, Mode}|Acc].
	
%% @spec
%% @doc create exit clauses for required fields in active mode
create_exit_clauses({clause,Line,Args,_,_}, Dict) ->
    lists:foldr(
        fun({Var, Opts}, Acc) ->
            case proplists:get_value(mode, Opts, active) of
                active ->
                    [exit_required_clause(Line, Args, Var, Opts)|Acc];
                passive ->
                    Acc
            end
        end, [], dict:to_list(Dict)).
    
%% @spec
alter_original_clause({clause,Line,Args,_,Contents}, Dict, OtherGuards0) ->
    OtherGuards = expand_is_member(OtherGuards0, []),
    Guards =
        lists:foldl(
            fun({Var, Opts}, Acc) ->
                case proplists:get_value(mode, Opts, active) of
                    active ->
                        Acc;
                    passive ->
                        RejectValues = proplists:get_value(reject, Opts, [{atom,Line,undefined}]),
                        Acc ++ [{op,Line,'=/=',Var,RValue} || RValue <- RejectValues]
                end
            end, OtherGuards, dict:to_list(Dict)),
    {clause,Line,Args,wrap_guard_list(Guards),Contents}.
    
%% @spec
expand_is_member([Guards|Tail], Acc) when is_list(Guards) ->
    expand_is_member(Tail, Acc ++ expand_is_member(Guards, []));
expand_is_member([{call,Line,{atom,_,is_member},[Item,Cons]}|Tail], Acc) ->
    NewGuard =
        lists:foldl(
            fun (Match, undefined) ->
                    {op,Line,'==',Item,Match};
                (Match, Op) ->
                    {op,Line,'orelse',{op,Line,'==',Item,Match},Op}
            end, undefined, cons_to_list(Cons)),
    expand_is_member(Tail, Acc ++ [NewGuard]);
expand_is_member([Other|Tail], Acc) ->
    expand_is_member(Tail, Acc ++ [Other]);
expand_is_member([], Acc) -> 
    Acc.
   
%% @spec
wrap_guard_list([]) -> [];
wrap_guard_list(Guards) -> [Guards].

%% @spec required_field_clause(Line, Args, Arg, Opts) -> {clause,Line,Args,Guards,Content}
%% 		Line = integer()
%%		Args = [Arg]
%%		Arg = term()
%%		Opts = list()
%%		Guards = list()
%%		Content = list()
exit_required_clause(Line, Args, UsedArg, Opts) ->
    Args1 = lists:reverse(strip_unused_args(Args, UsedArg, Line)),
	RejectValues = proplists:get_value(reject, Opts, [{atom,Line,undefined}]),
    {clause,Line,Args1,
        [[{op,Line,'==',UsedArg,RValue}] || RValue <- RejectValues],
        [{call,Line,{atom,Line,exit},
            [{tuple,Line,[
                {atom,Line,error},
                {tuple,Line,[
                    {atom,Line,field_cannot_be_undefined},
                    undefined_field(UsedArg)
                ]}
            ]}]
        }]
    }.

%% @spec undefined_field(Field) -> Result
%%		Field = {atom,integer(),atom()} | {record_field,integer(),{var,integer(),atom()},atom(),{atom,integer(),atom()}}
%%	 	Result = {atom,integer(),atom()}
undefined_field({var,Line,Name}) -> {atom,Line,Name};
undefined_field({record_field,Line,{var,Line,_Var},_Record,{atom,Line,Field}}) -> {atom,Line,Field}.

%% @spec strip_unused_args(Args, Arg, Line)-> Result
%%		Args = [Arg]
%%		Arg = term()
%%		Line = integer()
%%		Result = Args
strip_unused_args(Args, Arg, Line)->
    strip_unused_args(Args, Arg, Line, []).

strip_unused_args([Arg|Tail], UsedArg, Line, Acc) ->   
    Acc1 = 
        case argument_contains_variable(Arg, UsedArg) of
            true ->
                [Arg|Acc];
            false ->
                [{var,Line,'_'}|Acc]
        end,
    strip_unused_args(Tail, UsedArg, Line, Acc1);
    
strip_unused_args([], _, _, Acc) -> Acc.
    
%% @spec argument_contains_variable(Arg, Var) -> true | false
%%		Arg = term()
%%		Var = term()
argument_contains_variable(Arg, {record_field,_,{var,_,Name},_,_}) ->
    argument_contains_variable(Arg, Name);
    
argument_contains_variable(Arg, {var,_,Name}) ->
    argument_contains_variable(Arg, Name);
    
argument_contains_variable({var,_,Name}, Name) -> true;
    
argument_contains_variable(Arg, Name) when is_tuple(Arg) ->
    argument_contains_variable(tuple_to_list(Arg), Name);
    
argument_contains_variable([Arg|Tail], Name) ->
    case argument_contains_variable(Arg, Name) of
        true -> true;
        false ->
            argument_contains_variable(Tail, Name)
    end;
    
argument_contains_variable(_, _) -> false.    

%% @spec cons_to_list(Cons) -> list()
%%		Cons = {cons,integer(),term(),Cons}
cons_to_list(Cons) ->
    cons_to_list(Cons, []).
   
cons_to_list({cons,_,Field,{nil,_}}, Acc) ->
    [Field|Acc];

cons_to_list({cons,_,Field,Cons}, Acc) ->
    cons_to_list(Cons, [Field|Acc]).
    