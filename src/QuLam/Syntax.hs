module QuLam.Syntax where
import Data.List (nub, intercalate)
import Data.Maybe (isNothing)

-- 型の定義（とりあえず最小限）
data Type
  = TBot                 -- ⊥
  | TVar String          -- 型変数 α, β, ...
  | TProd Type Type      -- A × B
  | TArr  Type Type      -- A → B
  deriving (Show, Eq)

-- 型付け用のコンテキスト（変数名とその型のリスト）
type Context = [(String, Type)]

-- 型エラーの種類
data TypeError
  = UnknownVar String
  | Mismatch Type Type
  | UnclosedLambda [String]
  | OtherTypeError String
  deriving (Show, Eq)

prettyTypeError :: TypeError -> String
prettyTypeError (UnknownVar x) =
  "Unknown variable: " ++ x
prettyTypeError (Mismatch expected actual) =
  "Type mismatch: expected " ++ prettyType expected
    ++ ", but got " ++ prettyType actual
prettyTypeError (UnclosedLambda vars) =
  case nub vars of
    []   -> "Lambda is not closed"
    [v]  -> "Lambda is not closed; free variable: " ++ v
    vs   -> "Lambda is not closed; free variables: "
             ++ intercalate ", " vs
prettyTypeError (OtherTypeError msg) =
  msg



-- コンテキストから変数の型を探す
lookupVar :: Context -> String -> Either TypeError Type
lookupVar ctx x =
  case lookup x ctx of
    Just ty -> Right ty
    Nothing -> Left (UnknownVar x)


-- 項の定義（純QuLamのコアの骨組み）
data Term
  = Var   String         -- 変数
  | Lam   String Type Term  -- λx : A. M
  | App   Term Term      -- 関数適用
  | Pair  Term Term      -- ?M, N?
  | Fst   Term           -- π? M
  | Snd   Term           -- π? M
  | Eps   Type Term
  | Tau   Term Term      -- τ M N
  | Eta   Term           -- η M
  | Theta Term           -- θ M
  | Zeta  Term
  | Xi    Term
  deriving (Show, Eq)

-- 自由変数
fv :: Term -> [String]
fv t =
  case t of
    Var x          -> [x]
    Lam x _ m      -> filter (/= x) (fv m)
    App m n        -> fv m ++ fv n
    Pair m n       -> fv m ++ fv n
    Fst m          -> fv m
    Snd m          -> fv m
    Eps _ m        -> fv m
    Tau m n        -> fv m ++ fv n
    Eta m          -> fv m
    Theta m        -> fv m
    Zeta m         -> fv m
    Xi m           -> fv m

-- x := u を M に代入する（capture-avoiding）
subst :: String -> Term -> Term -> Term
subst x u term =
  case term of
    Var y ->
      if y == x then u else Var y

    Lam y ty m | x `notElem` fv m ->
      Lam y ty m

    Lam y ty m ->
      if y == x
        then
          -- λx.m の中では x は束縛変数なので、それ以上は降りない
          Lam y ty m
        else
          -- capture-avoiding:
          -- y が u の自由変数に出てくるなら、m 内の y を新しい変数にリネームしてから降りる
          if y `elem` fv u
            then
              let y'   = freshVar y (allVars m ++ allVars u)
                  mRen = renameVar y y' m
              in Lam y' ty (subst x u mRen)
            else
              Lam y ty (subst x u m)

    App m n ->
      App (subst x u m) (subst x u n)

    Pair m n ->
      Pair (subst x u m) (subst x u n)

    Fst m ->
      Fst (subst x u m)

    Snd m ->
      Snd (subst x u m)

    Eps ty m ->
      Eps ty (subst x u m)

    Tau m n ->
      Tau (subst x u m) (subst x u n)

    Eta m ->
      Eta (subst x u m)

    Theta m ->
      Theta (subst x u m)

    Zeta m ->
      Zeta (subst x u m)

    Xi m ->
      Xi (subst x u m)


-- λ抽象の変数を別名に付け替える（y を y' に置き換える）
renameVar :: String -> String -> Term -> Term
renameVar y y' term =
  case term of
    Var z ->
      if z == y then Var y' else Var z

    Lam z ty m ->
      if z == y
        then Lam z ty m
        else Lam z ty (renameVar y y' m)

    App m n ->
      App (renameVar y y' m) (renameVar y y' n)

    Pair m n ->
      Pair (renameVar y y' m) (renameVar y y' n)

    Fst m ->
      Fst (renameVar y y' m)

    Snd m ->
      Snd (renameVar y y' m)

    Eps ty m ->
      Eps ty (renameVar y y' m)

    Tau m n ->
      Tau (renameVar y y' m) (renameVar y y' n)

    Eta m ->
      Eta (renameVar y y' m)

    Theta m ->
      Theta (renameVar y y' m)

    Zeta m ->
      Zeta (renameVar y y' m)

    Xi m ->
      Xi (renameVar y y' m)



-- 既存の変数集合 vs に含まれない新しい名前を作る
allVars :: Term -> [String]
allVars term =
  case term of
    Var x          -> [x]
    Lam x _ m      -> x : allVars m
    App m n        -> allVars m ++ allVars n
    Pair m n       -> allVars m ++ allVars n
    Fst m          -> allVars m
    Snd m          -> allVars m
    Eps _ m        -> allVars m
    Tau m n        -> allVars m ++ allVars n
    Eta m          -> allVars m
    Theta m        -> allVars m
    Zeta m         -> allVars m
    Xi m           -> allVars m

freshVar :: String -> [String] -> String
freshVar base vs =
  if base `elem` vs
    then freshVar (base ++ "_") vs
    else base

-- 1-step reduction (outermost-leftmost).
-- 現時点では β は扱わず、射影と eta/theta のみ簡約する。
step :: Term -> Maybe Term
step term =
  case term of
    -- fst <M, N>  --->  M
    Fst (Pair m _) ->
      Just m

    -- snd <M, N>  --->  N
    Snd (Pair _ n) ->
      Just n

    -- eta (theta M)  --->  M
    Eta (Theta m) ->
      Just m

    -- それ以外の形では、外側に redex が無ければ内側を左から順に探す。

    Fst m -> do
      m' <- step m
      Just (Fst m')

    Snd m -> do
      m' <- step m
      Just (Snd m')

    Eta m -> do
      m' <- step m
      Just (Eta m')

    Theta m -> do
      m' <- step m
      Just (Theta m')

    Zeta m -> do
      m' <- step m
      Just (Zeta m')

    Xi m -> do
      m' <- step m
      Just (Xi m')

    Pair m n -> do
      -- 左から優先的に簡約
      case step m of
        Just m' -> Just (Pair m' n)
        Nothing -> do
          n' <- step n
          Just (Pair m n')

    -- β-reduction: (\x : ty -> m) n  --->  m[x := n]
    App (Lam x ty m) n ->
      Just (subst x n m)

    App m n -> do
      -- β-redex でない場合は、外側に redex が無いので内側を簡約
      case step m of
        Just m' -> Just (App m' n)
        Nothing -> do
          n' <- step n
          Just (App m n')

    Eps ty m -> do
      m' <- step m
      Just (Eps ty m')

    Tau m n -> do
      case step m of
        Just m' -> Just (Tau m' n)
        Nothing -> do
          n' <- step n
          Just (Tau m n')

    Lam x ty m -> do
      m' <- step m
      Just (Lam x ty m')

    Var _ ->
      Nothing

-- 正規形まで簡約（簡約できなくなるまで step を繰り返す）
nf :: Term -> Term
nf t =
  case step t of
    Nothing  -> t
    Just t'  -> nf t'

-- 1-step もできなければ正規形
isNormalForm :: Term -> Bool
isNormalForm t = isNothing (step t)

-- 動作確認用のサンプル（あとで消してOK）
exampleType :: Type
exampleType = TArr (TVar "a") (TVar "a")
-- 'a -> 'a に対応

exampleTerm :: Term
exampleTerm = Lam "x" (TVar "a") (Var "x")
-- λx : 'a. x に対応

-- 型を人間向けにきれいに表示する関数
prettyType :: Type -> String
prettyType ty = case ty of
  TBot         -> "Bot"
  TVar a       -> "'" ++ a
  TProd a b    -> "(" ++ prettyType a ++ " * " ++ prettyType b ++ ")"
  TArr  a b    -> "(" ++ prettyType a ++ " -> " ++ prettyType b ++ ")"

-- 項を人間向けにきれいに表示する関数（とりあえず単純版）
prettyTauArg :: Term -> String
prettyTauArg t@(Lam _ _ _) = "(" ++ prettyTerm t ++ ")"
prettyTauArg t = prettyTerm t

prettyTerm :: Term -> String
prettyTerm t = case t of
  Var x           -> x
  Lam x ty body   -> "\\"
                    ++ x ++ " : " ++ prettyType ty
                    ++ " -> " ++ prettyTerm body
  App f x -> "((" ++ prettyTerm f ++ ") (" ++ prettyTerm x ++ "))"
  Pair a b        -> "(" ++ prettyTerm a ++ ", " ++ prettyTerm b ++ ")"
  -- 単項演算子は常に引数をカッコで囲む
  Fst m        -> "(fst "   ++ prettyTerm m ++ ")"
  Snd m        -> "(snd "   ++ prettyTerm m ++ ")"
  Eps ty m     -> "(eps "   ++ prettyType ty ++ " " ++ prettyTerm m ++ ")"
  Tau m n -> "(tau " ++ prettyTauArg m ++ " " ++ prettyTauArg n ++ ")"
  Eta m        -> "(eta "   ++ prettyTerm m ++ ")"
  Theta m      -> "(theta " ++ prettyTerm m ++ ")"
  Zeta m       -> "(zeta " ++ prettyTerm m ++ ")"
  Xi m         -> "(xi " ++ prettyTerm m ++ ")"

-- 項の型を推論する（まだ純粋な骨組み）
typeOf :: Context -> Term -> Either TypeError Type
typeOf ctx term =
  case term of
    Var x ->
      lookupVar ctx x

    Lam x ty m -> do
      -- 純QuLam: λ抽象は引数以外の外部変数をキャプチャしない（閉じている）
      let free = filter (/= x) (fv m)
      if not (null free)
        then
          Left (UnclosedLambda free)
        else do
          bodyTy <- typeOf [(x, ty)] m
          return (TArr ty bodyTy)


    App f u -> do
      -- NOTE:
      -- 含意除去（関数適用）の型付け規則も論文では、
      --
      --   Gamma1 |- M : A -> B    Gamma2 |- N : A
      --   --------------------------------------
      --              Gamma1 ∪ Gamma2 |- M N : B
      --
      -- のように Gamma1, Gamma2 を分けて書かれている。
      -- 実装では Pair と同様に、常に同じ ctx を渡しており
      -- （Gamma1 = Gamma2 = ctx の場合に対応）、個別の部分環境を区別していない。
      --
      -- これは、QuLam において
      --
      --   Gamma |- t : tau なら任意の Delta について
      --   Gamma ∪ Delta |- t : tau
      --
      -- という「弱化」がメタ定理として成り立つ、という事実に基づく実装上の簡略である。
      -- したがって、ここで常に単一の ctx を使っても、理論的に許される
      -- 型付け判断のクラスは論文の規則と外延的に同じになる。
      fTy <- typeOf ctx f
      uTy <- typeOf ctx u
      case fTy of
        TArr argTy resTy ->
          if argTy == uTy
            then return resTy
            else Left (Mismatch argTy uTy)
        _ ->
          Left (OtherTypeError "function position is not a function type")

    Pair a b -> do
      -- NOTE:
      -- 論文中の直積導入規則は、
      --
      --   Gamma1 |- M : A    Gamma2 |- N : B    Gamma1, Gamma2 は整合
      --   ----------------------------------------------------------
      --               Gamma1 ∪ Gamma2 |- <M, N> : A × B
      --
      -- のように Gamma1 と Gamma2 を明示的に分けている。
      -- 一方、この実装では Gamma1, Gamma2 を個別に管理せず、
      -- 常に同じ ctx を渡している（Gamma1 = Gamma2 = ctx に対応）。
      --
      -- これは、QuLam において
      --
      --   Gamma |- t : tau なら任意の Delta について
      --   Gamma ∪ Delta |- t : tau
      --
      -- という「弱化」がメタ定理として成立することを前提にした簡略化である。
      -- すなわち、実装では「少し大きめの環境 ctx」のもとで M, N を同時に型付けしているが、
      -- その結果として得られる型付き項のクラスは、論文の体系で許されるものと外延的に一致する。
      aTy <- typeOf ctx a
      bTy <- typeOf ctx b
      return (TProd aTy bTy)

    Fst p -> do
      pTy <- typeOf ctx p
      case pTy of
        TProd aTy _ -> return aTy
        _           -> Left (OtherTypeError "fst expects a pair")

    Snd p -> do
      pTy <- typeOf ctx p
      case pTy of
        TProd _ bTy -> return bTy
        _           -> Left (OtherTypeError "snd expects a pair")

    Eps ty m -> do
      mTy <- typeOf ctx m
      case mTy of
        TBot ->
          -- ε : ⊥ → α （ここでは ty が α）
          return ty
        _ ->
          Left (OtherTypeError "Eps expects an argument of type Bot")

    Tau m n -> do
      -- M は本来「閉じた」α→β なので、ここでは空コンテキストでチェック
      mTy <- typeOf [] m
      nTy <- typeOf ctx n
      case (mTy, nTy) of
        (TArr a b1, TArr b2 TBot) | b1 == b2 ->
          -- τ(M,N) : α → ⊥
          return (TArr a TBot)
        _ ->
          Left (OtherTypeError "Tau expects m : A -> B and n : B -> Bot")

    Eta m -> do
      mTy <- typeOf ctx m
      case mTy of
        TArr (TArr a TBot) TBot ->
          return a
        _ ->
          Left (OtherTypeError "Eta expects a term of type (A -> Bot) -> Bot")

    Theta m -> do
      aTy <- typeOf ctx m
      return (TArr (TArr aTy TBot) TBot)

    Zeta m -> do
      mTy <- typeOf ctx m
      case mTy of
        TArr (TProd a (TArr (TProd a2 b) TBot)) TBot
          | a == a2 -> Right (TArr a b)
        _ -> Left (OtherTypeError "Zeta: expected not (A * not (A * B))")

    Xi m -> do
      mTy <- typeOf ctx m
      case mTy of
        TArr a b ->
          Right (TArr (TProd a (TArr (TProd a b) TBot)) TBot)
        _ -> Left (OtherTypeError "Xi: expected A -> B")

