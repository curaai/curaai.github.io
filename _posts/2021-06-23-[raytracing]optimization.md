--- 
title: (RayTracing) 최적화
tags: haskell graphics raytracing
mathjax: true
---

 RayTracing 프로젝트(Ray Tracing in one Weekend)를 끝냈습니다. 하지만 이미지의 크기를 400x266으로 했음에도 불구하고, 결과를 확인하는 시간이 12분이 걸립니다. 그래서 여러 레퍼런스를 따라 프로그램을 최적화하는 과정을 문서로 남깁니다. 많이 봐주시고 피드백 주시면 감사하겠습니다. 
 
 https://github.com/curaai00/H-R-Tracing

<!--more-->

## 기본 세팅과 시간 측정하기

현재 `RayTracing` 프로그램에서 설정할 수 있는 파라미터들은 아래와 같이 설정되었습니다.

- Width: 400
- aspectRatio: 3/2
- samplePerPixel: 50
- rayDepth: 50

지금 프로젝트에서 고려해야할 점들은 3가지가 있습니다. [실행 속도, 메모리 사용량, 테스트 커버리지]입니다만, 현재 테스트 커버리지는 raytracing에서 랜덤 함수들을 사용하고 있어, 테스트를 구상하고 검사하는 것보다 실행 속도에 중점을 둡니다.

저는 `stack` 으로 프로젝트를 진행합니다. 글을 읽으시는 분들은 cabal, nix 등등 옵션을 따라 사용하시면 됩니다. 

stack의 package.yaml은 프로젝트의 라이브러리 및 `ghc-option` , subproject를 관리합니다. ghc의 RunTime System( `-with-rtsopts`)옵션에 값을 추가할 수 있습니다. *[여기](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html)에서 추가 옵션을 확인 가능합니다.* 

```yaml
executables:
  H-R-Tracing-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -rtsopts
    - '"-with-rtsopts=-s"'
    dependencies:
    - H-R-Tracing
```

RTS의 `-s` 옵션으로 프로그램에서 실행된 memory 사용량과 Task, Spark, Computation time을 확인할 수 있습니다. 

- INIT: 프로그램 실행  시간
- MUT: 실제 Computation 시간
- GC: Garbage Collector 시간
- EXIT: 프로그램 종료 시간
- TOTAL: 위 시간들의 총합

아래 Productivity는 GC와 MUT의 시간으로 계산됩니다. 데이터가 GC되는 시간보다 실제 계산하는 시간이 많을 수록 생산성이 높게 측정됩니다.

```bash
1,099,706,668,472 bytes allocated in the heap
  28,079,509,784 bytes copied during GC
      31,534,904 bytes maximum residency (334 sample(s))
         520,392 bytes maximum slop
              93 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     1051959 colls,     0 par   30.576s  33.391s     0.0000s    0.0047s
  Gen  1       334 colls,     0 par    2.137s   2.138s     0.0064s    0.0215s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time  660.110s  (656.630s elapsed)
  GC      time   32.713s  ( 35.530s elapsed)
  EXIT    time    0.001s  (  0.010s elapsed)
  Total   time  692.824s  (692.170s elapsed)

  Alloc rate    1,665,943,860 bytes per MUT second

  Productivity  95.3% of total user, 94.9% of total elapsed
```

## LLVM

가장먼저 실시한 것은 LLVM backend 최적화입니다. 아직은 결과를 보기까지 많은 시간이 소모되기 때문에 코드를 일일이 바꾸며 테스트 하기에는 비효율적입니다. 가장 쉽게 시작할 수 있는 LLVM으로 단축했습니다. 사용된 옵션은 다음과같습니다:

```yaml
ghc-options:
    - -rtsopts
    - '"-with-rtsopts=-s"'
    - -fllvm -optlo-O3 -optl-ffast-math -funfolding-use-threshold1000  # added
```

추가된 옵션을 하나씩 뜯어봅시다. 

`-fllvm` 은 GHC의 backend code generator를 "llvm"으로 바꾸는 옵션입니다. 기본은 `-fasm` 어셈블리로 변환을 하지만, numerical or vector 연산이 많은 경우에는 속도를 더 높일 수 있습니다. 대신에 컴파일 시간은 훨씬 늘어나니 주의하시길 바랍니다. *[참고](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/codegens.html?highlight=llvm)*

### Optimization Level

`-optlo` 는 LLVM optimizer로 <옵션>을 전달합니다.  `-O3` 는 LLVM optimizer Level 3를 의미합니다. *[참고](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/phases.html?highlight=optlo)*

### Math Operations

`-optl` 는 링커로 <옵션>을 전달합니다. `-ffast-math` 는 부동소수점 연산을 줄여 최적화합니다. [여기](https://llvm.org/docs/LangRef.html#fastmath)에 vector와 관련된 연산이 있으니 사용에 따라 옵션을 추가하시길 바랍니다. *옵션을 [이 페이지](https://clang.llvm.org/docs/ClangCommandLineReference.html)에서 검색해 `-optl-freciprocal-math` 이런식으로 이용하면 됩니다.* 

*LLVM에서도 벡터연산 최적화가 사용될텐데, or 혹은 and로 옵션을 줘야하는지는 아직 테스트 해보지 않았습니다.*

### Unfolding Threshold

`-funfolding-use-threshold1000` 는 함수 정의의 크기가 `1000` 보다 작으면  **unfolding(aka inlining)**시킵니다. 함수의 크기는 두가지에 의해 정해집니다: 식의 실제 사이즈를 제외하고, 문맥에서 인라인화 될 수 있는 식들을 적용한 사이즈 

여러 문서에서 함수를 인라인화하는 것은 최적화를 한다는 것이 이미 유명합니다.

```bash
180,352,505,768 bytes allocated in the heap
   4,135,946,656 bytes copied during GC
      30,222,632 bytes maximum residency (69 sample(s))
         493,272 bytes maximum slop
              87 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     173284 colls,     0 par    5.662s   6.068s     0.0000s    0.0040s
  Gen  1        69 colls,     0 par    0.611s   0.611s     0.0089s    0.0214s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time  481.830s  (481.426s elapsed)
  GC      time    6.274s  (  6.679s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time  488.104s  (488.105s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    374,307,342 bytes per MUT second

  Productivity  98.7% of total user, 98.6% of total elapsed
```

## Multi Processing

하스켈 RTS(RunTime System)에  `-threaded` 옵션과 `-N4` 로 쓰레드 기능과 코어를 지정했습니다: 

```yaml
dependencies:
- parallel

ghc-options:
    - -threaded
    - -rtsopts
    - '"-with-rtsopts=-N4 -s"'
    - -fllvm -optlo-O3 -optl-ffast-math -funfolding-use-threshold1000
```

`computeColor` 를 매핑시에 병렬처리 정책을 `rseq` 로 `parBuffer`라는 100개의 spark pool을 만들고 각 data를 병렬로 실행합니다.  `parMap`은 메모리 에러가 발생할 수 있기 때문에 위 함수를 사용했습니다: 

```haskell
render cam (Size w h) spp rayDepth objs = map computeColor coords

renderPar cam (Size w h) spp rayDepth objs =
  withStrategy (parBuffer 100 rseq) $ map computeColor coords
```

아래는 결과입니다:

```bash
182,202,604,096 bytes allocated in the heap
   9,224,088,184 bytes copied during GC
      31,778,288 bytes maximum residency (294 sample(s))
         546,008 bytes maximum slop
              97 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     49101 colls, 49101 par   13.256s   3.168s     0.0001s    0.0120s
  Gen  1       294 colls,   293 par    8.869s   2.254s     0.0077s    0.0238s

  Parallel GC work balance: 48.22% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 106400 (106399 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time  495.065s  (123.591s elapsed)
  GC      time   22.124s  (  5.422s elapsed)
  EXIT    time    0.005s  (  0.007s elapsed)
  Total   time  517.195s  (129.020s elapsed)

  Alloc rate    368,037,631 bytes per MUT second

  Productivity  95.7% of total user, 95.8% of total elapsed
```

보시면 이전에는 time 이후에 나오는 시간과 괄호로 감싸져 있는 시간이 비슷하게 나왔을 것 입니다. 다른 이유는 왼쪽은 System time으로 여러개의 코어를 사용하면서 4개의 코어에서 시간이 측정됬기 때문입니다. 괄호안의 시간은 Real time으로 실제로 걸린시간을 나타냅니다.

## Code 최적화

## Explicit Type Signatures

기존 벡터 관련 함수들이나 main function에는 generic이 definition 되어 있었습니다(*vscode의 haskell extension이 제안해준 definition*). 

```haskell
vDot :: Num a => Vec3 a -> Vec3 a -> a
vDot v1 v2 = sum $ v1 * v2

vLength :: Floating a => Vec3 a -> a
vLength v = sqrt $ vDot v v

vLengthSquared :: Floating a => Vec3 a -> a
vLengthSquared v = vDot v v

vUnit :: Floating b => Vec3 b -> Vec3 b
vUnit v =
  let k = recip . vLength $ v
   in fmap (* k) v
```

아래와 같이 vector 함수들의 타입을 명시적으로 지정했습니다. *[참고 링크](https://wiki.haskell.org/Performance/Overloading)*

```haskell
type Vec = Vec3 Float

vDot :: Vec -> Vec -> Float
vDot v1 v2 = sum $ v1 * v2

vLength :: Vec -> Float
vLength v = sqrt $ vDot v v

vLengthSquared :: Vec -> Float
vLengthSquared v = vDot v v

vUnit :: Vec -> Vec
vUnit v =
  let k = recip . vLength $ v
   in fmap (* k) v
```

### Refactoring

기존에 where 문에서 값의 정의하던 식들을 함수로 분리했습니다. 

```haskell
ray2color objs g depth r
  | depth <= 0 = (Vec3 0 0 0, g)
  | isNothing hr = (backgroundRayColor, g)
  | otherwise = hitRecursively (fromJust hr) g
  where
    hr = hit objs r (HitRange 0.001 maxValue)
    backgroundRayColor =
      let t' = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
       in pure (1 - t') + pure t' * Vec3 0.5 0.7 1.0
    hitRecursively hr@(HitRecord _ _ _ _ (Material m)) g
      | isJust _scattered =
        let scattered' = fromJust _scattered
            attenuation = attenuationColor scattered'
            (color, g') =
              ray2color objs g1 (depth - 1) (scatteredRay scattered')
         in (attenuation * color, g')
      | otherwise = (Vec3 0 0 0, g)
      where
        (_scattered, g1) = scatter m r hr g
```

위 코드를 보면 depth가 0으로 호출 되어도, `hit`와 `backgroundRayColor` 도 계산을 하게끔 짜여져 있습니다. optimization option 때문에 위 코드가 다르게 동작할 수 있지만 위의 경우처럼 guard에서 조건이 분기가 됨에도 연산이 될만한 여지가 있는 부분들은 모두 함수로 분리해서 lazy하게 바꿨습니다.

최종결과입니다:

```haskell
161,500,852,832 bytes allocated in the heap
     793,755,128 bytes copied during GC
      31,393,248 bytes maximum residency (39 sample(s))
         530,976 bytes maximum slop
              93 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     43606 colls, 43606 par    4.876s   1.180s     0.0000s    0.0087s
  Gen  1        39 colls,    38 par    1.418s   0.362s     0.0093s    0.0235s

  Parallel GC work balance: 14.01% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 106400 (106399 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)

  INIT    time    0.002s  (  0.001s elapsed)
  MUT     time  239.303s  ( 59.598s elapsed)
  GC      time    6.294s  (  1.542s elapsed)
  EXIT    time    0.004s  (  0.009s elapsed)
  Total   time  245.602s  ( 61.150s elapsed)

  Alloc rate    674,879,983 bytes per MUT second

  Productivity  97.4% of total user, 97.5% of total elapsed
```

## Reference

- [https://wiki.haskell.org/Performance/GHC](https://wiki.haskell.org/Performance/GHC)
- [https://medium.com/@s.nawaz/optimizing-ray-tracing-in-haskell-3dc412fff20a](https://medium.com/@s.nawaz/optimizing-ray-tracing-in-haskell-3dc412fff20a)

thread나 time profiling을 시도해보긴 했지만, 거기서 유의미한 결과를 추출해내지 못했습니다. 이점은 좀 아쉽네요.