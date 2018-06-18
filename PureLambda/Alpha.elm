module PureLambda.Alpha exposing (..)

import Tuple exposing (..)
import Dict exposing (..)

import PureLambda.Parser exposing (..)

type alias Id = Int

type alias Env = (Id, Dict String Id)


find : String -> Env -> Id
find k env =
    Maybe.withDefault 0 (get k (second env))


type alias A = Expr (String, Id)

conv_in : Env -> A -> (A, Env)
conv_in env e =
    case e of
        EVar (x, _) ->
            let id = find x env in
            (EVar (x, id), env)

        EName n ->
            (EName n, env)

        EAbs xs body ->
            let id = first env in
            let (id_, xs_) = List.foldr
                             (\(x, _) (id, acc) -> (id+1, (x, id)::acc))
                             (id, [])
                             xs
            in
            let env_ = (id_, union (fromList xs_) (second env)) in
            let (body_, env__) = conv_in env_ body in
            (EAbs xs_ body_, env__)

        EApp es ->
            let update e (acc, env) =
                let (a, env_) = conv_in env e in
                (a::acc, env_)
            in
            let (es_, env_) = List.foldr update ([], env) es in
            (EApp es_, env_)


init : E -> A
init e =
    case e of
        EVar x ->
            EVar (x, 0)

        EName n ->
            EName n

        EAbs xs body ->
            EAbs (List.map (\x -> (x, 0)) xs) (init body)

        EApp es ->
            EApp (List.map init es)


conv : A -> (A, Env)
conv = conv_in (1, Dict.empty)
