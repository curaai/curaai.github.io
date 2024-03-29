--- 
title: (Haskell Tutorial) 1. Simple Types
tags: haskell
mathjax: true
---

C++, python만 할줄아는 나는 `FP(Functional Progamming)` 을 배우던중 지금 하고있는 언어에서 사용하면서 배우는 것이 아닌 `FP`를 base로 하는 언어에서 concept과 방식, 설계를 배워볼려고 한다.

얼마전 흥미로운 글이 `Haskell`로 되어있어 이해하는데 어려움이 있었는데 이것이 위 욕구를 자극하는 도화선이 됐다. 이 시리즈는 함수형 언어를 배우면서 쓰는 튜토리얼을 가장한 TIL이다. 

<!--more-->

앞으로 `ghic >`를 `>` notation으로 교체하겠다.

## Types & Simple Arithmetic

기본적인 연산들, 설명 X 

```haskell
> True && True 
True 
> True && False 
False 
> False || True
True
> not False 
True 
> 1 + 2 
3 
> 3 / 2
1.5
> "hello" == "hello"
True
```

다른 언어에서 배운 지식이 어려움 없이 그대로 익힐 수 있는 **type**이다. `:t` 와 valid expression으로 type을 추론할 수 있다.  `expression :: type` format의 output이다.

```haskell
>:t True
True :: Bool
>:t "Hello"
"Hello" :: [Char]
>:t 2.5
2.5 :: Float
```

### List 

`[T]`는 `T` type의 list, notation이다. 

-  `++` 연산자: 같은 type의 element를 가지는 두 array를 붙일 수 있다. 
- `:` 연산자: `T` type의 element와 `[T]`를 붙일 수 있다.
- `!!`연산자: n 번째 element를 가져온다.

```haskell
> [1, 2] ++ [3, 4]
[1, 2, 3, 4]
> 'H':"ello"
"Hello"
> "World" !! 2
'r'
> "Hello" ++ " " ++ "World!"
"Hello World!"
```

### Compare Operation in List

haskell의 list compare operatoin은 다소 신기하다. 두 list의 type이 같지 않아도 comparable이면 가능하고, 길이가 같지 않아도 가능하다. 각 리스트의 0번째부터 시작해 n번째 요소까지 비교한다. 만약 i번째 결과가 결정된다면 ` i+1 ... n` 까지의 연산은 하지 않는다.

```haskell
> [1, 2, 3] > [1, 2, 0]
True
> [3, 4, 5] > [6]
False 
> [1.1, 2.2, 3.3] > [1, 2, 3]
True
```

## Range

range operation은 **대소 비교가능한** range면 무슨 타입이든 가능하다. python은 `range(10), range(2, 30, 3`) 이런 스타일의 end 혹은 start, end, step의 인자를 받는 내장함수가 있다. haskell은 아래와 같이 표현한다.

`[start, next_start..upper_limit]` `next_start - start`만큼을 step으로 해서 upper_limit까지의 sequence를 return한다.

```haskell
> [0..9]
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
> [1, 4..10]
[1, 4, 7, 10]
> ['a', 'b'..'z']
"abcdefghijklmnopqrstuvwxyz"
```

### List Comprehension

haskell의 `List Comprehension`은 수학의 set comprehension과 유사하다.

$$S = \{ 2 \cdot x \vert x \in \mathbb N, x \le 10\}$$ 은 haskell로 `[x*2 | x <- [1..10]] `이다. 여기에 컨디션을 추가 하고싶다면, set comprehension처럼 `,`로 구분된 compare expression을 추가한다. 


```haskell
> [ x * 2 | x <- [1..10], x >= 12]
[12, 14, 16, 18, 20]
```



