# data-extend-th

TH to define a new record data type that extends the existing simple record data type.

## examples

### by union exstsing record type type

```
data Animal = Animal
  { name :: String
  , age  :: Int
  } deriving (Show, Eq, Generic)

data HumanInfo = HumanInfo
  { address :: String
  } deriving (Show, Eq, Generic)

$(extendD "data Human = Animal <> HumanInfo deriving (Show, Generic) ")
```

`data Human`

```
:i Human
data Human
  = Human {name :: String,
           age :: Int,
           address :: String}
instance Show Human
instance Generic Human
```

### higher kind, define field records anonymously

```
data X a = X {f1 :: a Int} deriving (Generic)
data Y a = Y {f2 :: Maybe a} deriving (Generic, Show)
data Z a = Z { x :: X a } deriving(Generic)
$(extendD "data XY = X <> Y <> { e1 :: String, e2 :: String } <> Z deriving (Generic)")
```

`XY`
```
data XY (a :: * -> *) a1 (a2 :: * -> *)
  = XY {f1 :: a Int,
        f2 :: Maybe a1,
        e1 :: String,
        e2 :: String,
        x :: X a2}
instance Generic (XY a1 a2 a3)
```
