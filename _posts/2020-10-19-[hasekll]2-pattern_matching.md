---
title: (Haskell Tutorial) 2. Pattern matching
tags: haskell
mathjax: true
---

오늘은 하스켈의 function이다. 처음이라 이해하기 어려운 syntax와 새로운 style이 많았다. <http://learnyouahaskell.com>에서 읽고 재정리 한 글임을 밝힙니다.

<!--more-->

## Simple Function Definition

아래의 문법이 아주아주 간단한 버전의 함수이다. `func_name {args seperated with space} = {expression}`. 입문자의 입장에서 아주아주 난감했다. 함수에 괄호(parenthesis)도 등장하지 않다니, 하스켈을 할줄아는 친구에게 물어봤다. *"함수에 괄호가 없으면 인자가 몇개인지 뭔지 헷갈리지 않아?"*라고 했으나, 돌아온 답변은 *"해보면 문제없다"*였다. 아주아주 난감했지만, 일단 해보기로

```haskell
> inc x = x + 1
> inc 2 
3 
> add_ x y = x + y 
> add_ 1 2 
3
> 1 `add_` 2
3 
```

  backtip으로 함수를 wrap해 `func x y`를 `x func y`처럼 쓸수있다. 

## Function declaration with :t 

Tutorial 1에서 사용했던 `:t` 명령어를 선언했던 function에 사용해보자. 

```haskell
> :t inc 
inc :: Num a => a -> a
> :t add_
add_ :: Num a => a -> a -> a
```

이전 tutorial에서 언급했듯이 :t는 `expression :: type`이다. 그러므로 `{expression}=inc :: {type}=(Num a => a -> a) ` 또한 함수의 정의가 아닌 **type** 이다. 

사용 실행하면 새로운 문법이 등장한다. `=>` 기호 전 문자들은 **class constraint**로 `var a`의 type을 미리 하고 기호의 이후는 `a`를 인자로 받아 `Num` type 값을 반환한다는 의미다.  `add_`는 인자 값을 두개로 받는데 `(a, a) -> a`가 아닌 `a -> a -> a` 이유는 currying을 배우게 되면 알게될 것 이다. 

##  Pattern Matching

pattern matching은 data를 함수의 pattern에 따라 분석된다. 아래 구문을 보면 바로 이해가 될것임, `:set +m`은 `ghci`의 multiline을 enable하는 명령이다. 그리고 `let` 키워드로 함수를 정의시에 **라인의 끝을 space없이 개행**하면 multiline definition이 가능하다.

```haskell
> :set +m 
> let inc 1 = 2
|     inc x = inc (x-1) + 1
> inc 3 
4 
> let first :: (a, b, c) -> a 
|     first (x, _, _) = x 
> first 1, 2, 3 
1 
```

`_`은 해당 값은 무시한다는 의미이다.

## List

list는 특히 recursive한 function에서 자주 쓰이는 것 같다.  *`ghci` 인터프리터 쉘에서는 함수 정의시의 indent를 다음 pattern에서 indent를 맞추지 않으면 에러가 발생한다.*

```haskell
> let head :: [a] -> a
|     head [] = error "Empty list"
|			head (x:_) = x
> head [1, 2, 3]
1 
> let print :: (Show a) => [a] -> String
|			print [] = "Empty"
| 		print (x:[]) = "size one: " ++ show x 
| 		print (x:y:_) = "long with: " ++ show x ++ " and " ++ show y
> print [1]
"size one: 1"
> print [1, 2, 3]
"long with: 1 and 2"
> print [1, 2]
"*** Exception: <interactive>:(): Non-exhaustive patterns in function print
```

`func []`는 빈 list를 argument로 받았을 때, `(x:y:_)`로 list의 arg-1:x, arg-2: y, _: tail로 표현가능하다. 굳이 x, y 말고도 a, b, c, d, e 이런식으로 여러개의 n-th argument를 지정한 다음 ` _` : (리스트의 나머지: tail)로 사용가능하다. 

### As Pattern

list의 pattern matching에서 `func (x:xs) = "do something"` 처럼 head:tail를 의미하는 `x:xs`가 있다. 앞에 `var@` 를 붙여 head:tail이 아닌 all, head,tail로 사용할 수 있다. 

```haskell
> let print _list@(x:xs) = "First char is '" ++ [x] ++ "' else '" ++ xs ++ "' original string is '" ++ _list ++ "'"
> print "Hello World"
"First char is 'H' else 'ello World'' original string is 'Hello World'"
```

### Condition

Condition pattern matching은 `if-elif-elif-else` 처럼 값의 condition으로 pattern matching을 한다.  ~~condition pattern matcing이 정식명칙인지는 모르겠으나 tutorial page에서 따로 keyword를 알려주지 않았기 때문에 일단 이렇게 넘어간다.~~

```haskell
> let max` a b 
|        | a > b = a
|        | otherwise = b
> max` 3 1 
3 
> max` 1 3 
3
```

특이한 점은 위 pattern matching에서는 함수 이름을 indentation에 맞춰쓰면 되지만, condition pattern은 함수 이름이 아닌 condition expressoin을 써야되기 때문에 `|` 로 대체 가능하다.

### Where 

아래는 [여기](http://learnyouahaskell.com/syntax-in-functions)에서 그냥 가져왔다. 딱봐도 뭔지 알겠지. 

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
```

이 하스켈은 놀랍게도 where 문 안에서 함수를 정의하는 것 또한 가능하다! 

```haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  
```

