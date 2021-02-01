---
title: (codewars) Explosive Sum, Partial Number
tags: algorithm, math
mathjax: true
---

codewars, 4kyu, Explosive Sum의 문제풀이입니다. 자연수의 분할을 이용해서 알고리즘을 해결했습니다. 
<!--more-->

---

## 공식 1.

임의의 자연수 `n`은 1이상의 자연수의 합으로 표현될 수 있습니다.  예를들어 숫자 4는 다음처럼 표현가능합니다:

```bash
4              # p(4, 1) => 1 
3 + 1          # p(4, 2) => 2  
2 + 2 
2 + 1 + 1      # p(4, 3) => 1 
1 + 1 + 1 + 1  # p(4, 4) => 1
# f(4) = 5 
```

> `n` 을 표현할 수 있는 가짓수를 구하는 함수 `f(n)`

는 아래의 함수를 이용해서 답을 구할 수 있습니다. 

> `n`을 `k`개의 자연수의 합으로 표현할 수 있는 가짓수를 구하는 함수 `p(n, k)` 가 있습니다.

그럼 `f(n)` 은 1부터 n개 까지의 partial함수의 합이겠죠.

코드로 표현하자면: 

```python
def f(n):
    res = 0 
    for i in range(1, n):
        res += p(n, i)
    return res 
```

수식으로 표현하자면: 

$$f(n) = p(n, 1) + p(n, 2) + \dots + p(n, n)$$

입니다. 

## 공식 2.

그럼 `p(n, k)` 는 어떻게 구할 수 있을까요? 

`n ≤ k` 일때 1 이상의 자연수의 합입니다.

$$p(n-k, k) = p(n-k, 1) + p(n-k, 2) + \dots + p(n-k, k)$$

아래 예제를 보시죠: 

```bash
# p(7,4)
(1+a) + (1+a) + (1+a) + (1+a)
# p(7,4) = p(3,1) + ...
= (1+3) + 1 + 1 + 1 
= 4 + 1 + 1 + 1  
# p(7,4) = ... + p(3,2)  + ...
= (1+2) + (1+1) + 1 + 1
= 3 + 2 + 1 + 1 
# p(7,4) = ... + p(3,3)
= (1+1) + (1+1) + (1+1) + 1 
= 2 + 2 + 2 + 1  
```

당연히 `p(n, k)`에서 `n < k`일 경우는 0입니다.  

## 공식 3.

`p(n, k)` 는 합하는 수에 1이 있는 경우와 없는 경우를 합해 값을 얻을 수 있습니다.

- 1이 포함되는 경우는, 1을 빼고 `p(n-1, k-1)` 로 다시 값을 구할 수 있습니다.
- 1이 포함되지 않는 경우는, k개의 자연수가 모두 1보다 크다는 것이므로 k개에 각각 1을 빼버리면 `p(n-k, k)` 가 됩니다.

```bash
# p(7,2) = p(6,1) + p(5,2) = 3 
1 + 6 # p(6, 1)
2 + 5 # 1 + 4 in p(5,2)
3 + 4 # 2 + 3 in p(5,2)
```

$$p(n, k) = p(n-1, k-1) + p(n-k, k)$$

## Code

최종적으로 아래 수식을 조합한 코드는 아래와 같습니다. *추가적으로 dynamic programming을 넣어 불필요한 연산을 최소화 했습니다* 

```python
from collections import defaultdict
cache = defaultdict(dict)

def exp_sum(n):
    return sum(map(lambda k: partial(n, k), range(1, n+1)))

def partial(n, k):
    if k == 1 or n == k:
        return 1 
    if n < k:
        return 0
    if k in cache[n]:
        return cache[n][k]
    res = partial(n-1, k-1) + partial(n-k, k)
    cache[n][k] = res 
    return res
```

## Reference

- [https://m.blog.naver.com/math_kkh/221212645641](https://m.blog.naver.com/math_kkh/221212645641)
- [https://youtu.be/ZG3wPMorAAg](https://youtu.be/ZG3wPMorAAg)