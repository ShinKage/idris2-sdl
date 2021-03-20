module SDL.Elab

import public Language.Reflection

-- ADAPTED FROM: https://github.com/MarcelineVQ/idris2-elab-deriving

lookupName : Name -> Elab (Name, TTImp)
lookupName n = do
  [(name, imp)] <- getType n
    | xs => fail $ show n ++ case xs of
                                  [] => " is not in scope."
                                  xs => " is not uniquely in scope, these conflicting names exist: " ++
                                          concatMap (show . fst) xs
  pure (name, imp)

mapName : (String -> String) -> Name -> Name
mapName f (UN x) = UN (f x)
mapName f (MN x y) = MN (f x) y
mapName f (NS x y) = NS x (mapName f y)
mapName f (DN x y) = DN (f x) y
mapName f (RF x) = RF (f x)

extractNameStr : Name -> String
extractNameStr (UN x) = x
extractNameStr (MN x y) = x
extractNameStr (NS xs x) = extractNameStr x
extractNameStr (DN x _) = x
extractNameStr (RF x) = x

checkConstructors : Name -> List Name -> Elab ()
checkConstructors tyname names = traverse_ check names
  where
    check : Name -> Elab ()
    check n = do
      (qname, IVar _ _) <- lookupName n
        | _ => fail "Constructor must not have any argument"
      pure ()

mkTy : Name -> TTImp -> ITy
mkTy = MkTy EmptyFC EmptyFC

var : Name -> TTImp
var = IVar EmptyFC

str : String -> TTImp
str = IPrimVal EmptyFC . Str

eqObject : Name -> Name -> Name -> Visibility -> Elab (Decl, Decl)
eqObject qname decname eqfun vis = do
  (eqname, _) <- lookupName `{{Eq}}
  [NS _ (DN _ eqcon)] <- getCons eqname
    | _ => fail "eqObject: error during Eq constructor lookup"
  let claim = IClaim EmptyFC MW vis [Hint True] (mkTy decname `(Eq ~(var qname)))
  let neqfun = `(\x, y => not (~(var eqfun) x y))
  let rhs = `(~(var eqcon) ~(var eqfun) ~(neqfun))
  let body = IDef EmptyFC decname [(PatClause EmptyFC (var decname) rhs)]
  pure (claim, body)

export
deriveUnitSumEq : Visibility -> Name -> Elab ()
deriveUnitSumEq vis sname = do
  (qname, IType _) <- lookupName sname
    | _ => fail "Expected data constructor without indices"
  let decn = mapName ("eqImpl" ++) sname
  let funn = mapName (\d => "eqImpl" ++ d ++ "Fun") sname
  connames <- getCons qname
  checkConstructors qname connames
  let funclaim = IClaim EmptyFC MW Private [Inline] (mkTy funn `(~(var qname) -> ~(var qname) -> Bool))
  let funpats = map (\n => PatClause EmptyFC `(~(var funn) ~(var n) ~(var n)) `(True)) connames
  let catchall = PatClause EmptyFC `(~(var funn) ~(Implicit EmptyFC True) ~(Implicit EmptyFC True)) `(False)
  let fundecl = IDef EmptyFC funn (funpats ++ [catchall])
  (objclaim, objclause) <- eqObject qname decn funn vis
  declare [funclaim, objclaim]
  declare [fundecl, objclause]

showObject : Name -> Name -> Name -> Visibility -> Elab (Decl, Decl)
showObject qname decname showfun vis = do
  (showname, _) <- lookupName `{{Show}}
  [NS _ (DN _ showcon)] <- getCons showname
    | _ => fail "eqObject: error during Show constructor lookup"
  let claim = IClaim EmptyFC MW vis [Hint True] (mkTy decname `(Show ~(var qname)))
  let precfun = `(\_, x => ~(var showfun) x)
  let rhs = `(~(var showcon) ~(var showfun) ~(precfun))
  let body = IDef EmptyFC decname [(PatClause EmptyFC (var decname) rhs)]
  pure (claim, body)

export
deriveUnitSumShow : Visibility -> Name -> Elab ()
deriveUnitSumShow vis sname = do
  (qname, IType _) <- lookupName sname
    | _ => fail "Expected data constructor without indices"
  let decn = mapName ("showImpl" ++) sname
  let funn = mapName (\d => "showImpl" ++ d ++ "Fun") sname
  connames <- getCons qname
  checkConstructors qname connames
  let funclaim = IClaim EmptyFC MW Private [Inline] (mkTy funn `(~(var qname) -> String))
  let funpats = map (\n => PatClause EmptyFC `(~(var funn) ~(var n)) `(~(str $ extractNameStr n))) connames
  let fundecl = IDef EmptyFC funn funpats
  (objclaim, objclause) <- showObject qname decn funn vis
  declare [funclaim, objclaim]
  declare [fundecl, objclause]
