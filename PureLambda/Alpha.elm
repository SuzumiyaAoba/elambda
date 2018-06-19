module PureLambda.Alpha exposing (..)

import Tuple exposing (..)
import Dict exposing (..)

import PureLambda.Parser exposing (..)

type alias Id = Int

type alias Env = (Id, Dict String Id)


find : String -> Env -> Id
find k env =
    Maybe.withDefault 0 (get k (second env))


updateId : Int -> Env -> Env
updateId id_ (id, dict) = (id_, dict)


dict : Env -> Dict String Id
dict = second


nextId : Env -> Id
nextId = first


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
            let id = nextId env in
            let (id_, xs_) = List.foldr
                             (\(x, _) (id, acc) -> (id+1, (x, id)::acc))
                             (id, [])
                             xs
            in
            let env_ = (id_, union (fromList xs_) (dict env)) in
            let (body_, env__) = conv_in env_ body in
            (EAbs xs_ body_, env__)

        EApp es ->
            let alpha e (acc, env) =
                let (a, (id, _)) = conv_in env e in
                (a::acc, updateId id env)
            in
            let (es_, env_) = List.foldr alpha ([], env) es in
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
