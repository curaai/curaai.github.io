--- 
title: (RayTracing) 안티앨리어싱
tags: haskell
mathjax: true
---

현실의 카메라로 찍은 사진은 아래 이미지 처럼 계단현상은 거의 보이지 않을 것 입니다. 사진은 픽셀이 물체와 배경이 섞이기 때문입니다. 저희도 픽셀을 샘플링한 후 평균을 내는 것으로 비슷한 효과를 만들어 낼 수 있습니다. 

![0](/assets/images/2021-06-20/c6/Untitled0.png)

## 랜덤 유틸리티

일반적인 범위인 $0 \leq r < 1$에서 실수를 반환하는 랜덤 생성기가 필요합니다. "1보다 작다"라는 점은 몇가지 이점이 있기 때문에 중요합니다. 

저는 하스켈의 `random` 패키지의 `RandomGen` instance와 `randomR` 함수를 사용합니다. 

## 샘플링으로 픽셀 생성

한 픽셀에 $n_s$개 만큼의 무작위 광선을 쏴 결과를 평균냅니다.

![1](/assets/images/2021-06-20/c6/Untitled1.png)

아래는 샘플링을 포함한 카메라의 기능을 하는 함수입니다.

```haskell
render ::
     (Integral b, Hittable a) => Camera -> Size Int -> Int -> a -> [Color b]
render cam size spp objs = map computeColor coords
  where
    coords = (,) <$> reverse [0 .. height size - 1] <*> [0 .. width size - 1]
    computeColor (y, x) =
      vec2color spp . fst $ foldl sampling (Vec3 0 0 0, g) [1 .. spp]
      where
        g = mkStdGen $ y * width size + x
        sampling :: RandomGen g => (Vec3 Float, g) -> a -> (Vec3 Float, g)
        sampling (acc, g) _ = (acc + (ray2color objs . pos2ray cam) (u, v), g')
          where
            func f v = v / ((+ (-1)) . toFloat . f) size
            (u, v) = (func width (toFloat x + i), func height (toFloat y + j))
            (i, g1) = sampleFloat g
            (j, g') = sampleFloat g1

vec2color :: (Integral b, Integral a) => a -> Vec3 Float -> Color b
vec2color spp = fmap (truncate . (* 256) . clamp 0 0.999 . (/ toFloat spp))
  where
    clamp min' max' x = max min' . min max' $ x
```

결과 확대시에 가장자리 픽셀의 변화를 확인할 수 있습니다.

![2](/assets/images/2021-06-20/c6/Untitled2.png)