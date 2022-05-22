---
title: (HackerRank) Forming a Magic Square
tags: algorithm hackerrank
---

 잘못된 3*3 Magic Square가 입력으로 주어졌을 때, 행렬을 최소로 수정한 값의 차이를 출력하는 문제입니다.

- [Magic Square: 마방진](https://en.wikipedia.org/wiki/Magic_square)은 정방 행렬의 칸 하나하나가 가로/세로/대각 칸들의 합이 같습니다.
- 마방진(N*N, N=even)은 행렬을 회전/전치 시켜 총 8가지의 마방진을 만들어낼 수 있습니다.

<!--more-->

---

참고 

- <https://github.com/curaai/StudyAlgorithms/blob/main/haskell/hackerrank/magic_square_forming.hs>
- <https://www.hackerrank.com/challenges/magic-square-forming/problem>

## Code

```haskell
module Main where formingMagicSquare

import Data.List (transpose)

allMagicSquare = let a = take 4 (iterate rot90 completeArray) in a ++ map transpose a
  where
    completeArray =
      [ [8, 1, 6],
        [3, 5, 7],
        [4, 9, 2]
      ]
    rot90 = map reverse . transpose

formingMagicSquare :: [[Int]] -> Int
formingMagicSquare s = minimum $ map (diff s) allMagicSquare
  where
    diff a b = sum . map abs $ zipWith (-) (concat a) (concat b)
```

## 설명

 3*3 행렬이 전제입니다.

1. 기본 마방진에서 나올수 있는 모든 경우를 생성합니다.
2. 틀린 마방진과 모든 마방진을 각각 비교하여 가장 작은 값을 반환합니다.

### 참고: 마방진 생성 방법(홀수)

1. 첫번째 행의 가운데 열에 1로 시작합니다.
2. 다음으로 기본적인 방법은
  - 위 + 오른쪽 값을 채웁니다.
  - 만약 위 칸에 값이 채워져 있다면 아래칸에 값을 채웁니다.
  - 위 과정을 반복합니다.
