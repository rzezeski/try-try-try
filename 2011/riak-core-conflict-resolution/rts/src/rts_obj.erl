%% @doc A suite of functions that operate on the algebraic data type
%% `rts_obj'.
%%
%% TODO Possibly move type/record defs in there and use accessor funs
%% and opaque types.
-module(rts_obj).
-export([ancestors/1, children/1, equal/1, equal/2, reconcile/2, unique/1,
         update/3]).
-export([meta/1, val/1, vclock/1]).

-include("rts.hrl").

%% @pure
%%
%% @doc Given a list of `rts_obj()' return a list of all the
%% ancesotrs.  Ancestors are objects that all the other objects in the
%% list have descent from.
-spec ancestors([rts_obj()]) -> [rts_obj()].
ancestors(Objs) ->
    As = [[O2 || O2 <- Objs,
                 ancestor(O2#rts_vclock.vclock,
                          O1#rts_vclock.vclock)] || O1 <- Objs],
    unique(lists:flatten(As)).

%% @pure
%%
%% @doc Predicate to determine if `Va' is ancestor of `Vb'.
-spec ancestor(vclock:vclock(), vclock:vclock()) -> boolean().
ancestor(Va, Vb) ->
    vclock:descends(Vb, Va) andalso (vclock:descends(Va, Vb) == false).

%% @pure
%%
%% @doc Given a list of `rts_obj()' return a list of the children
%% objects.  Children are the descendants of all others objects.
children(Objs) ->
    unique(Objs) -- ancestors(Objs).

%% @pure
%%
%% @doc Predeicate to determine if `ObjA' and `ObjB' are equal.
-spec equal(ObjA::rts_obj(), ObjB::rts_obj()) -> boolean().
equal(#rts_vclock{val=V, vclock=C}, #rts_vclock{val=V, vclock=C}) -> true;
equal(#rts_vclock{}, #rts_vclock{}) -> false;

equal(#rts_basic{val=V}, #rts_basic{val=V}) -> true;
equal(#rts_basic{}, #rts_basic{}) -> false;

equal(not_found, not_found) -> true;

equal(_, _) -> false.

%% @pure
%%
%% @doc Closure around `equal/2' for use with HOFs (damn verbose
%% Erlang).
-spec equal(ObjA::rts_obj()) -> fun((ObjB::rts_obj()) -> boolean()).
equal(ObjA) ->
    fun(ObjB) -> equal(ObjA, ObjB) end.

%% @pure
%%
%% @doc Reconcile the list of `Objs'.
-spec reconcile(reconcile_fun(), [rts_obj()]) -> rts_obj().
reconcile(RecFun, [#rts_basic{}|_]=Objs) ->
    case unique(Objs) of
        [Obj] -> Obj;
        Mult -> RecFun(Mult)
    end;

reconcile(RecFun, [#rts_vclock{}|_]=Objs) ->
    case rts_obj:children(Objs) of
        [Child] -> Child;
        Chldrn -> RecFun(Chldrn)
    end.

%% @pure
%%
%% @doc Given a list of `Objs' return the list of uniques.
-spec unique([rts_obj()]) -> [rts_obj()].
unique(Objs) ->
    F = fun(Obj, Acc) ->
                case lists:any(equal(Obj), Acc) of
                    true -> Acc;
                    false -> [Obj|Acc]
                end
        end,
    lists:foldl(F, [], Objs).

%% @pure
%%
%% @doc Given a `Val' update the `Obj'.  The `Updater' is the name of
%% the entity performing the update.x
%%
%% TODO Do I want to limit `Updater' to `node()'?
-spec update(val(), node(), rts_obj()) -> rts_obj().
update(Val, _Updater, #rts_basic{}=Obj0) ->
    Obj0#rts_basic{val=Val};

update(Val, Updater, #rts_vclock{vclock=VClock0}=Obj0) ->
    VClock = vclock:increment(Updater, VClock0),
    Obj0#rts_vclock{val=Val, vclock=VClock}.

-spec meta(rts_obj()) -> meta().
meta(#rts_basic{meta=Meta}) -> Meta;
meta(#rts_vclock{meta=Meta}) -> Meta.

-spec val(rts_obj()) -> any().
val(#rts_basic{val=Val}) -> Val;
val(#rts_vclock{val=Val}) -> Val;
val(not_found) -> not_found.

%% @pure
%%
%% @doc Given a vclock type `Obj' retrieve the vclock.
-spec vclock(rts_vclock()) -> vclock:vclock().
vclock(#rts_vclock{vclock=VC}) -> VC.
