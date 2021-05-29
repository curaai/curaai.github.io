---
title: (Ray Tracing in One Weekend) 7. Anti Aliasing
tags: haskell graphics ray-tracing
---

현실의 카메라로 찍은 사진을 보면, 계단현상은 거의 없을 것이다. 물체의 가장자리는 배경과 섞이기 때문인데, 픽셀을 샘플링한 후 평균을 내서 비슷한 효과를 만들어 낼 수 있다.  `stratification sampling` 이란 방법도 있지만 사용하지 않을 것이다. 랜덤 샘플링은 논란이 많은 방법이긴 하지만, 이 프로그램에서는 유용하다. 하지만 몇가지 다른 레이트레이서에서는 치명적이다. 우리가 작성하는 일반적인 방법에서는 그다지 많은 효과도 없고, 코드만 더럽게 만들뿐이다. 나중에 카메라에 기능을 추가하기 위해 추상화한다.  

![0](/assets/images/2021-05-29/c7/Untitled0.png)

## 랜덤 유틸리티

일반적인 범위인 $0 \leq r < 1$에서 실수를 반환하는 랜덤 생성기가 필요하다. "1보다 작다"라는 점은 우리가 나중에 몇가지 이점을 얻을 수 있다. 

우리는 하스켈의 `random` 패키지를 사용한다. 

## 샘플링으로 픽셀 생성

픽셀이 주어졌을 때 샘플링할 갯수만큼의 광선을 보내 평균낸다.

![1](/assets/images/2021-05-29/c7/Untitled1.png)

자 이제 화면 샘플링 기능을 카진 카메라 클래스를 만들었다. 

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

결과 이미지를 확대해보면, 물체의 가장자리 픽셀이 달라진 것을 확인할 수 있다. 

![2](/assets/images/2021-05-29/c7/Untitled2.png)