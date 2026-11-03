module QuLam.Parser
  ( parseType
  , parseTerm
  , parseFile
  ) where

import Data.Void (Void)
import Data.List (nub)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import QuLam.Syntax

-- 変数名として使えない予約語
reserved :: [String]
reserved = ["zeta", "xi"] ++
  [ "let"
  , "in"
  , "eps"
  , "tau"
  , "eta"
  , "theta"
  , "fst"
  , "snd"
  ]

-- 入力は String にしておく（あとでファイルから読む）
type Parser = Parsec Void String

-- スペース・コメントのスキップ
sc :: Parser ()
sc =
  L.space
    space1              -- 空白・改行
    (L.skipLineComment "--")  -- 行コメント: "-- ?行末"
    empty               -- ブロックコメントはとりあえず無し

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keyword :: String -> Parser String
keyword kw =
  lexeme (try (string kw <* notFollowedBy (alphaNumChar <|> char '_')))

-- 識別子（変数名用）: 先頭は小文字、その後は英数字・アンダースコア
ident :: Parser String
ident = (lexeme . try) $ do
  x <- (:) <$> lowerChar <*> many (alphaNumChar <|> char '_')
  if x `elem` reserved
    then fail ("reserved word " ++ x)
    else return x

-- 型変数: 例 "'a"
typeVar :: Parser Type
typeVar = do
  _ <- char '\''
  a <- some alphaNumChar
  sc
  return (TVar a)

-- 型 "Bot" / "'a" / 括弧付き
atomType :: Parser Type
atomType =
      (TBot <$ symbol "Bot")
  <|> typeVar
  <|> between (symbol "(") (symbol ")") typeP

-- A * B や A -> B のための右結合パーサ
typeP :: Parser Type
typeP = makeArrow
  where
    -- A -> B -> C みたいな右結合
    makeArrow = do
      t1 <- makeProd
      restArrow t1

    restArrow t1 =
          (do
              _ <- symbol "->"
              t2 <- makeArrow
              return (TArr t1 t2)
          )
      <|> return t1

    -- 直積 A * B * C ... （右結合）を読む
    makeProd = do
      t1 <- atomType
      restProd t1

    restProd t1 =
          (do
              _  <- symbol "*"
              t2 <- makeProd
              return (TProd t1 t2)
          )
      <|> return t1

-- 公開用：文字列から型をパース
parseType :: String -> Either String Type
parseType input =
  case runParser (sc *> typeP <* eof) "<type>" input of
    Left err  -> Left (errorBundlePretty err)
    Right ty  -> Right ty

-------------------------------------------------------
-- 項のパーサ（今回は Var と Lam だけに絞る）
-------------------------------------------------------

-- 変数: ident をそのまま Var に
varTerm :: Parser Term
varTerm = Var <$> ident

-- λ抽象:  \x : 'a -> x
lamTerm :: Parser Term
lamTerm = do
  _  <- lexeme (char '\\')
  x  <- ident
  _  <- symbol ":"
  ty <- atomType
  _  <- symbol "->"
  body <- termP
  return (Lam x ty body)

-- 括弧とペア:
--   (t)      ? 括弧付き
--   (t, u)   ? ペア

-- n 個の項 [t1, t2, ..., tn] を
--   n=1: t1
--   n>1: Pair t1 (Pair t2 (... tn))
-- に畳み込む
mkTuple :: [Term] -> Term
mkTuple []     = error "mkTuple: empty list"
mkTuple [t]    = t
mkTuple (t:ts) = Pair t (mkTuple ts)

parensOrPairTerm :: Parser Term
parensOrPairTerm =
  between (symbol "(") (symbol ")") inner
  where
    inner = do
      t1   <- termP
      rest <- many (symbol "," *> termP)
      case rest of
        []  -> return t1
        ts  -> return (mkTuple (t1 : ts))

-- 「単純な項」:
--   - 変数
--   - 括弧・ペア
--   - fst t
--   - snd t
termSimple :: Parser Term
termSimple =
      epsTerm
  <|> tauTerm
  <|> etaTerm
  <|> thetaTerm
  <|> zetaTerm
  <|> xiTerm
  <|> fstTerm
  <|> sndTerm
  <|> varTerm
  <|> parensOrPairTerm



-- fst t
fstTerm :: Parser Term
fstTerm = do
  _ <- keyword "fst"
  t <- termSimpleNoPrefix
  return (Fst t)

-- snd t
sndTerm :: Parser Term
sndTerm = do
  _ <- keyword "snd"
  t <- termSimpleNoPrefix
  return (Snd t)

-- fst/snd の「引数側」では fst/snd 自体を再帰的に読まない単純版
-- 例: fst (fst x) は、外側 fst の引数を "(fst x)" と書けばOK
termSimpleNoPrefix :: Parser Term
termSimpleNoPrefix =
      varTerm
  <|> parensOrPairTerm

-- eps 'a t  （ε : ⊥ -> 'a）
epsTerm :: Parser Term
epsTerm = do
  _  <- keyword "eps"
  ty <- atomType
  t  <- termSimpleNoPrefix
  return (Eps ty t)

-- tau m n  （τ : (A -> B) × (B -> Bot) -> (A -> Bot) のイメージ）
tauTerm :: Parser Term
tauTerm = do
  _ <- keyword "tau"
  m <- termSimpleNoPrefix
  n <- termSimpleNoPrefix
  return (Tau m n)

-- eta t
etaTerm :: Parser Term
etaTerm = do
  _ <- keyword "eta"
  t <- termSimpleNoPrefix
  return (Eta t)

-- theta t
thetaTerm :: Parser Term
thetaTerm = do
  _ <- keyword "theta"
  t <- termSimpleNoPrefix
  return (Theta t)

-- zeta t
zetaTerm :: Parser Term
zetaTerm = do
  _ <- keyword "zeta"
  t <- termSimpleNoPrefix
  return (Zeta t)

-- xi t
xiTerm :: Parser Term
xiTerm = do
  _ <- keyword "xi"
  t <- termSimpleNoPrefix
  return (Xi t)

-- 関数適用（左結合）:
--   f x y  ==> ((f x) y)
termApp :: Parser Term
termApp = do
  t  <- termSimple
  ts <- many termSimple
  return (foldl App t ts)


-- let x = e1 in e2  を  e2[x := e1] というマクロに展開する
letTerm :: Parser Term
letTerm = do
  _    <- keyword "let"
  x    <- ident
  _    <- symbol "="
  rhs  <- termP
  _    <- keyword "in"
  body <- termP
  -- subst x rhs body で e2[x:=e1]
  return (subst x rhs body)


-- 項全体:
--   まず λ抽象を試し，ダメなら適用列
termP :: Parser Term
termP =
      letTerm
  <|> lamTerm
  <|> termApp


-- 公開用：文字列から項をパース
parseTerm :: String -> Either String Term
parseTerm input =
  case runParser (sc *> termP <* eof) "<term>" input of
    Left err -> Left (errorBundlePretty err)
    Right t  -> Right t

-------------------------------------------------------
-- ファイル全体: context: ... term: ... を読む
-------------------------------------------------------

contextEntry :: Parser (String, Type)
contextEntry = do
  x  <- ident
  _  <- symbol ":"
  ty <- typeP
  return (x, ty)

fileP :: Parser (Context, Term)
fileP = do
  sc
  _   <- symbol "context"
  _   <- symbol ":"
  -- contextEntry を、「term:」が現れるまで読み続ける
  ctx <- manyTill contextEntry (symbol "term" >> symbol ":")
  let names = map fst ctx
  if length names == length (nub names)
    then pure ()
    else fail "Duplicate variable name in context"
  t   <- termP
  eof
  return (ctx, t)


parseFile :: String -> Either String (Context, Term)
parseFile input =
  case runParser fileP "<file>" input of
    Left err -> Left (errorBundlePretty err)
    Right r  -> Right r
