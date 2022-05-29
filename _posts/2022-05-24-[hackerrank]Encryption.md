---
title: (HackerRank) Encryption
tags: algorithm hackerrank haskell
mathjax: true
---


  문자열 s가 주어집니다.

  s 길이의 제곱근을 flooring, ceiling하여 row, column로 정합니다. 

  `haveaniceday` 입력이 주어 졌을때 length는 12:$$\sqrt{12}$$, 3 rows 와 4 columns를 가집니다.

```
have
anic
eday
```

  각 열을 단어로 묶어 `' '`와 함께 반환 합니다.  `hae and via ecy`

<!--more-->

---

참고 

- <https://github.com/curaai/StudyAlgorithms/blob/main/haskell/hackerrank/encryption.hs>
- <https://www.hackerrank.com/challenges/encryption/problem>

## Code

```haskell
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.List.Split (chunksOf)

trim = f . f
  where
    f = reverse . dropWhile isSpace

encryption :: String -> String
encryption s = unwords $ map (trim . crawlChar) [0 .. col -1]
  where
    crawlChar n = map getChar rows
      where
        getChar row = bool ' ' (row !! n) (n < length row)
    rows = chunksOf col s
    col = ceiling . sqrt . fromIntegral . length $ s
```

## 설명

1. column을 구해 문자열을 col개씩 나눕니다. ⇒ rows
2. 각 행에서 n번째 문자를 가져옵니다. 없다면 ‘ ‘로 ⇒ crawlChar
3. 0부터 col-1까지의 crawlChar를 실행합니다.