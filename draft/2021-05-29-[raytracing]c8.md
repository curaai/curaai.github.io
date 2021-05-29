---
title: (Ray Tracing in One Weekend) 8. 난반사 재질
tags: haskell graphics ray-tracing
---

이제 diffuse (matte) material을 살펴볼 차례다. 도형과 재질을 합쳐서 볼것인지 별개로 둘것인지 떠오른다.  여기서는 별개- 대부분의 렌더링에서 사용하는- 로 둘 것이고, 여기에 한계점이 있음을 알아두자.

## 단순한 난반사 재질

난반사 물체는 빛을 그대로 반사하는 것이 아니라, 고유의 색으로 조절한뒤 반사한다. 만약 3개의 광선을 두개의 난반사 표면에 쏜다면, 무작위 현상이 일어난다: 

![0](/assets/images/2021-05-29/c8/Untitled0.png)

그리고 반사보다 흡수하는 성질을 가진다. 어두운 표면일수록 더 흡수한다. 랜덤한 방향을 생성하는 어느 알고리즘이든 표면을 보면 matte처럼 보일 것 이다. 일단은 먼저 난반사 표면을 만들어보자.

표면위의 접점 **P**에 접하는 두 단위구가 있습니다. **n**이 표면의 법선일 때, 두 단위구는 중점이 **(P + n)** 과 **(P - n)**입니다. 중점이 **(P + n)**인 구는 표면밖, **(P - n)**인 구는 표면 안입니다. 광선의 원점과 같은 방향인 단위구 안의 무작위 점 **S**를 고릅니다. 그리고 접점 **P**에서 무작위 점 **S**로 향하는**(S-P)** 광선을 만듭니다. 

![1](/assets/images/2021-05-29/c8/Untitled1.png)

-1 ~ 1의 범위를 가지는 x, y, z 좌표의 무작위 점을 계속 만듭니다. 무작위 점이 구 밖을 벗어나지 않을 때 까지 반복합니다. 

```haskell
sampleUnitSphere :: RandomGen g => g -> (Vec3 Float, g)
sampleUnitSphere g = find (Vec3 1 1 1, g)
  where
    find (v, g')
      | vLengthSquared (fmap f v) < 1 = (fmap f v, g')
      | otherwise = find (sampleVector g')
      where
        f x = 2 * (x - 0.5)
```

```haskell
ray2color ::
     (RandomGen g, Hittable a) => a -> g -> Ray -> (Vec3 Float, g)
ray2color objs g r
  | isNothing hr = (backgroundRayColor, g)
  | otherwise = hitRecursively (fromJust hr) g
  where
    hr = hit objs r (HitRange 0 maxValue)
    backgroundRayColor =
      let t' = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
       in pure (1 - t') + pure t' * Vec3 0.5 0.7 1.0
    hitRecursively hr g = (0.5 * color, g')
      where
        (randUnitSphereVec, g1) = sampleUnitSphere g
        target = hitPoint hr + hitNormal hr + sampleUnitSphereVec
        (color, g') =
          ray2color
            objs
            g'
            (Ray (hitPoint hr) (target - hitPoint hr))
```

## 광선의 무한재귀 제한하기

위 코드에서는 부딪히기만 한다면 광선을 재귀적으로 거의 무한히 반복할 수도 있다. 아주아주 오랜시간-스택을 터트릴 만큼-이 걸리수도 있으니, 광선의 최대 깊이를 제한하자.

```haskell
ray2color ::
     (RandomGen g, Hittable a) => a -> g -> Int -> Ray -> (Vec3 Float, g)
ray2color objs g depth r
  | depth <= 0 = (Vec3 0 0 0, g)
  | isNothing hr = (backgroundRayColor, g)
  | otherwise = hitRecursively (fromJust hr) g
  where
    hr = hit objs r (HitRange 0 maxValue)
    backgroundRayColor =
      let t' = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
       in pure (1 - t') + pure t' * Vec3 0.5 0.7 1.0
    hitRecursively hr g = (0.5 * color, g')
      where
        (randUnitSphereVec, g1) = sampleUnitSphere g
        target = hitPoint hr + hitNormal hr + sampleUnitSphereVec
        (color, g') =
          ray2color
            objs
            g'
            (depth - 1)
            (Ray (hitPoint hr) (target - hitPoint hr))
```

예상 결과:

![2](/assets/images/2021-05-29/c8/Untitled2.png)


## 감마 보정

위 결과 사진은 매우 어둡다. 광선이 부딪혀 튀길때 마다 50%의 에너지를 흡수하기 때문인데, 이를 밝게 고쳐보자(현실에서 밝은 회색정도). 현상이 발생하는 이유는 대부분의 이미지 뷰어는 이미지가 "gamma corrected"라고 추정하기 때문이다. 감마 보정은 간단히 색에 "1/gamma"만큼 제곱을 해주는 것이다. 여기서는 "gamma 2"로 보정한다고 하니 color에 sqrt를 적용하자. 

```haskell
vec2color :: (Integral b, Integral a) => a -> Vec3 Float -> Color b
vec2color spp =
  fmap (truncate . (* 256) . clamp 0 0.999 . sqrt . (/ toFloat spp))
  where
    clamp min' max' x = max min' . min max' $ x
```

![3](/assets/images/2021-05-29/c8/Untitled3.png)

## Shadow Acne

여기에 미묘한 버그가 있다. float값의 부동소수점 특성상 정확히 **t=0**이 되지 않고, **t=-0.000001** 혹은 **t=0.000001**이되는 것인데, tMin을 0에 근사하게 바꿔서 해결하자. 

```haskell
hr = hit objs r (HitRange 0.001 maxValue)
```

## 람버르티안 반사

현재 반사방법은 표면에 접하는 단위구에서 무작위 점을 만들어낸다. 구체의 표면에서 반사되는 벡터를 고르는 것은 낮은 확률로 넓은 각도의 난반사를 보이고, 높은 확률로 법선과 유사한 결과를 보인다. Φ를 법선에서 반사되는 벡터까지의 각도라고 할때, 이 분포를 $cos^3(\phi)$ 로 scaling하면 빛이 얕고 넓게 퍼져 최종적인 색에 얕은 기여를 미친다.

분포는 좀더 균등한, $cos(\phi)$ 의 참 람버르티안은 법선에 가깝게 광선을 뿌린다. 이는 표면에 접한 단위구 위의 점을 고르는 것으로 해결할 수 있다. 이는 다시말해 단위구 안의 무작위 점을 normalize하는 것으로 대신할 수 있다.

```haskell
sampleUnitVector :: RandomGen g => g -> (Vec3 Float, g)
sampleUnitVector g =
  let sampled = sampleUnitSphere g
   in first vUnit sampled
```

![4](/assets/images/2021-05-29/c8/Untitled4.png)

`randomUnitSphere` 를 `randomUnitVector` 로 교체한다. 

```haskell
        (randUnitVec, g1) = sampleUnitVecor g
        target = hitPoint hr + hitNormal hr + sampleUnitVec
```

![5](/assets/images/2021-05-29/c8/Untitled5.png)

이 두 난반사 방법의 차이를 말하기는 좀 어렵다. 두 구는 별다른 차이는 없어 보이지만, 시각적으로 중요한 차이가 있다: 

1. 적용후: 그림자가 덜 퍼졌음
2. 적용후: 구가 밝아짐 

더 적은 광선이 법선을 향해 뿌려지고, 밝은 광선이 uniform하게 뿌려져서 두변화가 생겼다. 이 의미는 난반사 물체는 카메라를 향해 더 많은 빛이 튕기기 때문에 *더 밝다*고 할수있다. 그림자의 경우, 더 적게 반사되며, 밝은 빛때문에 더 밝아보인다. 

## 난반사 공식의 대안

위에서 제시된 람버르티안 난반사는 부정확한 근사치가 입증되기 전까지 꽤 오랬동안 사용됐다. 문제를 오랬동안 풀지 못했던 이유는 다음과 같다:

1. 수학적으로 분포가 틀렸음을 증명하는 것 
2.  $cos(\phi)$ 가 왜 이상적인지 설명하는 것

일상생활의 물체는 완벽하게 반사가 이루어지고 있으므로, 우리가 만든 렌더링으로 빛을 쏘면 어색하다. 

접점에서 넓은 각도로 unifrom하게 뿌리는 방법은 법선의 각도와 관계가 없는 것이다. 많은 레이트레이싱 논문은 이 난반사 방법을 사용했다(람버르티안 난반사를 적용하기 전).

```haskell
sampleInHemisPhere :: RandomGen g => Vec3 Float -> g -> (Vec3 Float, g)
sampleInHemisPhere normal' g
  | 0 < vDot inUnitSphere normal' = (inUnitSphere, g')
  | otherwise = (-inUnitSphere, g')
  where
    (inUnitSphere, g') = sampleUnitVector g
```

```haskell
(randHemisphereVec, g1) = sampleInHemisPhere (hitNormal hr) g
target = hitPoint hr + randHemisphereVe
```

![6](/assets/images/2021-05-29/c8/Untitled6.png)