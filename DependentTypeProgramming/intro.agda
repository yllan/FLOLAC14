module Intro where

data Bool : Set where
  true  : Bool
  false : Bool

not : Bool → Bool
not true = false
not false = true

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A

infixr 5 _∷_

id : (A : Set) → A → A
id A x = x

ex3 : ∀ {A B C : Set} → (A → B) → (B → C) → A → C
ex3 x y z = y (x z)

ex4 : ∀ {A B C : Set} → (A → B → C) → (A → B) → A → C
ex4 x y z = x z (y z)

-- 大括號括起來可以不用每次給 type
null : ∀ {A} → List A → Bool
null [] = true
null (x ∷ xs) = false

ℕorBool : Bool → Set
ℕorBool false = ℕ
ℕorBool true = Bool

ex : (b : Bool) → ℕorBool b
ex true = true
ex false = 1

data ⊤ : Set where
  tt : ⊤

data ⊥ : Set where

IsTrue : Bool → Set
IsTrue true = ⊤
IsTrue false = ⊥

headOk : ∀ {A}
         → (xs : List A)
         → IsTrue (not (null xs))
         → A
headOk [] ()
headOk (x ∷ xs) p = x

length : ∀ {A} → List A → ℕ
length [] = zero
length (x ∷ xs) = suc (length xs)

last : ∀ {A} → (xs : List A) → IsTrue (not (null xs)) → A
last [] ()
last (x ∷ []) p = x
last (x ∷ y ∷ xs) p = last (y ∷ xs) tt

{- * Use headOk to extract the first component of ex1 -}

headex1 : ℕ
headex1 = headOk (1 ∷ 2 ∷ []) tt

{- * Can you apply headOk to []? How, or why not? -}

-- a more complex example
_∨_ : Bool → Bool → Bool
true ∨ _ = true
false ∨ b = b

_∧_ : Bool → Bool → Bool
true ∧ c = c
false ∧ c = false

somewhere : ∀ {A} → (A → Bool) → List A → Bool
somewhere p [] = false
somewhere p (x ∷ xs) = p x ∨ somewhere p xs

find1st : ∀{A} → (p : A → Bool) → (xs : List A) →
           IsTrue (somewhere p xs) → A
find1st p [] ()
find1st p (x ∷ xs) q with p x
... | false = find1st p xs q
... | true = x

_==_ : ℕ → ℕ → Bool
zero == zero = true
zero == suc n = false
suc m == zero = false
suc m == suc n = m == n

_≤_ : ℕ → ℕ → Bool
zero ≤ n = true
suc m ≤ zero = false
suc m ≤ suc n = m ≤ n

_<_ : ℕ → ℕ → Bool
m < zero = false
--zero < zero = false
--suc m < zero = false
zero < suc n = true
suc m < suc n = m < n

index : ∀ {A} → (xs : List A) → (n : ℕ) → IsTrue (n < length xs) → A
index [] n ()
index (x ∷ xs) zero t = x
index (x ∷ xs) (suc n) t = index xs n t

-- Pairs.

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

fst : ∀ {A B} → A × B → A
fst (x , y) = x

snd : ∀ {A B} → A × B → B
snd (x , y) = y

zip : ∀ {A B} → (xs : List A) → (ys : List B) → List (A × B)
zip [] ys = []
zip (x ∷ xs) [] = []
zip (x ∷ xs) (y ∷ ys) = x , y ∷ zip xs ys

zip= : ∀ {A B} → (xs : List A) → (ys : List B)
      → IsTrue (length xs == length ys) → List (A × B)
zip= [] ys t = []
zip= (x ∷ xs) [] ()
zip= (x ∷ xs) (y ∷ ys) t = x , y ∷ zip= xs ys t

-- Σ type.                                                                                                                                                                    

data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (x : A) → B x → Σ A B

fst' : ∀ {A B} → Σ A B → A
fst' (x , y) = x

snd' : ∀ {A B} → (p : Σ A B) → B (fst' p)
snd' (x , y) = y

-- Again, use ℕorBool to form some useless examples.                                  
--  ℕorBool : Bool → Set                                                              
--  ℕorBool false = ℕ                                                                 
--  ℕorBool true  = Bool
-- The type Σ Bool ℕorBool is a pair. The type of its                                 
-- second component is                                                               
--   - ℕ if the *value* of its first component is false, or                          
--   - Bool if the value of its first component it true.                                                                                                                      

ex6 : Σ Bool ℕorBool
ex6 = (false , 1)

ex7 : Σ Bool ℕorBool
ex7 = (true , true)

ex8 : ∀ {A B} → (A × B) → (B × A)
ex8 x = snd x , fst x

data _∔_ (A B : Set) : Set where
  left  : A → A ∔ B
  right : B → A ∔ B

    -- ∔ can be keyed in by \dotplus                                                                                                                                          

ex9 : ∀ {A B} → A ∔ B → B ∔ A
ex9 (left x) = right x
ex9 (right x) = left x

ex10 : ∀ {A B C} → A ∔ (B ∔ C) → (A ∔ B) ∔ C
ex10 (left x) = left (left x)
ex10 (right (left x)) = left (right x)
ex10 (right (right x)) = right x

ex11 : ∀ {A B C} → A ∔ (B × C) → (A ∔ B) × (A ∔ C)
ex11 (left x) = left x , left x
ex11 (right x) = right (fst x) , right (snd x)

¬ : Set → Set
¬ P = P → ⊥
  -- ¬ can be keyed in by \neg                                                                                                                                                

ex12 : ∀ {A} → A → ¬ (¬ A)
ex12 x y = y x

ex13 : ∀ {A B} → (¬ A) ∔ (¬ B) → ¬ (A × B)
ex13 (left x) y = x (fst y)
ex13 (right x) y = x (snd y)

ex14 : ∀ {A B} → (A → B) → (¬ B → ¬ A)
ex14 x y z = y (x z)
