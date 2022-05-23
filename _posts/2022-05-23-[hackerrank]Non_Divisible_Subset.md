---
title: (HackerRank) Non-Divisible Subset
tags: algorithm hackerrank
mathjax: true
---

  정수 집합 $$S$$가 주어졌을 때, 아래 조건을 충족하는 $$S$$의 가장 큰 부분집합 $$S'$$의 크기를 구합니다.

  조건: $$k$$가 주어집니다. $$S'$$의 크기가 2인 모든 부분집합은 각 부분집합의 합이 $$k$$로 나누어지지 않아야 합니다. 

<!--more-->

---

참고 

- <https://github.com/curaai/StudyAlgorithms/blob/main/haskell/hackerrank/non_divisible_subset.hs>
- <https://www.hackerrank.com/challenges/non-divisible-subset/problem>

## Code

```haskell
module Main where formingMagicSquare

import Data.Bool (bool)

nonDivisibleSubset :: Int -> [Int] -> Int
nonDivisibleSubset k s = a + b + c
  where
    a = min 1 (hist !! 0)
    b = bool 0 (min 1 (hist !! (k `div` 2))) $ even k
    c = sum [max (hist !! i) (hist !! (k - i)) | i <- [1 .. ((k -1) `div` 2)]]
    hist = let mods = map (`mod` k) s in [length . filter (== i) $ mods | i <- [0 .. (k -1)]]
```

## 설명

 숫자 a,b 합의 나머지(`(a+b)%k`) 는 a나머지 + b나머지 (`a%k + b%k`)와 같습니다. *a나머지 + b나머지가 k를 넘지 않을 때*

 두 정수(나머지)의 합이 k이 되는 경우는 아래와 같습니다. 아래 경우를 제외하고는 두정수의 합의 나머지가 0이 되지 않습니다. 

1. 나머지 0 + 나머지 0 = 0, (a+b)%k == 0, `case a` 
2. k가 짝수 일때, k/2 + k/2, `case b` 
3. 1 + (k-1) = k, `case c`

s를 k로 나눈 나머지의 값 개수를 `hist`에 저장합니다.

1. 나머지가 0인 값은 하나만 S’에 들어갈 수 있습니다. 
2. 나머지가 짝수인 값은 하나만 S’에 들어갈 수 있습니다.
3. 나머지 1과 나머지 k-1 중 더 많은 값을 S’에 넣습니다.

### 예시

예시를 들어보자면 나머지가 [0, 1, 2, 3, 4, 5, 6, 9], k가 6일때

1. 0: [1, 2, 3, 4, 5]와 각각 더했을 때 k가 되지 않습니다. 
2. 3: 3(9%6)을 제외하고, [0, 1, 2, 4, 5]와 각각 더했을 떄 k가 되지 않습니다. 
3. 1: 5를 제외하고, [0, 2, 3, 4]와 각각 더했을 때 k가 되지 않습니다.