--- 
title: (RayTracing) 난반사 재질
tags: haskell graphics raytracing
mathjax: true
---

구의 재질을 만들어 볼 차례입니다. 난반사 재질로 시작해봅시다. 여기서 떠오르는 의문점은 물체과 재질을 분리(재질을 여러개의 물체에 할당) 할 것인지 묶어(재질과 물체가 연결된 절차적 물체에 유용) 볼것인지 입니다. 여기서는 분리- 대부분의 렌더링에서 사용하는-할 것이고, 각 방식에 장단점이 있음을 유의합시다. 

## 단순한 난반사 재질

난반사 물체는 빛을 발산하지 않는 대신에 고유의 색으로 조절한뒤 반사합니다. 난반사 표면에 반사되는 빛은 무작위 방향을 가집니다. 3개의 빛을 나란히 발사했지만 모두 각기 다른 방향을 향합니다.: 

![0](/assets/images/2021-06-20/c7/Untitled0.png)

난반사는 반사보다 흡수하는 성질을 가집니다. 어두운 표면일수록 더 흡수를 많이합니다.

표면위의 접점 **P**와 법선 **n**으로, 중점이 **(P + n)** 과 **(P - n)**입인 표면에 접하는 두 단위 구를 정의할 수 있습니다. 광선의 원점과 같은 방향인 단위 구  안에서 무작위 점 **S**를 고릅니다. 그리고 접점 **P**에서 무작위 점 **S**로 향하는**(S-P)** 광선을 만듭니다. 

![1](/assets/images/2021-06-20/c7/Untitled1.png)

-1 ~ 1의 범위를 가지는 **x**, **y**, **z** 좌표계 안의 무작위 점을 계속 만듭니다. 무작위 점이 구 밖을 벗어나지 않을 때 까지 반복합니다. 

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

위 코드에서는 부딪히기만 한다면 광선을 재귀적으로 거의 무한히 반복할 수도 있습니다. 아주아주 오랜시간-스택을 터트릴 만큼-이 걸리수도 있으니, 광선의 최대 깊이를 제한해야합니다.

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

![2](/assets/images/2021-06-20/c7/Untitled2.png)

## 감마 보정

위 결과 사진은 매우 어둡습니다. 광선이 부딪혀 튀길때 마다 50%의 에너지를 흡수하기 때문인데, 이를 밝게 고쳐봅시다(현실에서 밝은 회색정도). 현상이 발생하는 이유는 대부분의 이미지 뷰어는 감마 보정이 적용되었다고 추정하기 때문입니다. 근사값으로 "gamma 2: 색에 **1/gamma**만큼 제곱을 하는 것"를 사용합니다. 저희의 경우는 간단히 제곱근(`sqrt` )을 사용합니다. 

```haskell
vec2color :: (Integral b, Integral a) => a -> Vec3 Float -> Color b
vec2color spp =
  fmap (truncate . (* 256) . clamp 0 0.999 . sqrt . (/ toFloat spp))
  where
    clamp min' max' x = max min' . min max' $ x
```

![3](/assets/images/2021-06-20/c7/Untitled3.png)

## Shadow Acne

여기에 미묘한 버그가 있습니다. float값의 부동소수점 특성상 정확히 **t=0**이 되지 않고, **t=-0.000001** 혹은 **t=0.000001**이되는 것인데, `tMin`을 0에 근사하게 바꿔서 해결합니다.

```haskell
hr = hit objs r (HitRange 0.001 maxValue)
```

## 람버르티안 반사

현재 반사방법은 표면에 접하는 단위구에서 무작위 점을 만듭니다. 구체의 표면에서 반사되는 벡터를 고르는 것은 낮은 확률로 넓은 각도의 난반사를 보이고, 높은 확률로 법선과 유사한 결과를 보입니다.

Φ를 법선에서 반사되는 벡터까지의 각도라고 할때, 이 분포를 $cos^3(\phi)$ 로 scaling하면 빛이 얕고 넓게 퍼져 최종적인 색에 얕은 기여를 미칩니다.

분포는 좀더 균등한, $cos(\phi)$ 의 참 람버르티안은 법선에 가깝게 광선을 뿌립니다. 표면에 접한 단위구 위의 점을 고르는 것으로 해결할 수 있습니다. 이는 다시말해 `vUnit` 함수로 단위벡터로 만들면 됩니다.

```haskell
sampleUnitVector :: RandomGen g => g -> (Vec3 Float, g)
sampleUnitVector g =
  let sampled = sampleUnitSphere g
   in first vUnit sampled
```

![4](/assets/images/2021-06-20/c7/Untitled4.png)

`randomUnitSphere` 를 `randomUnitVector` 로 교체한다. 

```haskell
        (randUnitVec, g1) = sampleUnitVecor g
        target = hitPoint hr + hitNormal hr + sampleUnitVec
```

![5](/assets/images/2021-06-20/c7/Untitled5.png)

이 두 난반사 방법의 차이는 말하기 어렵지만, 시각적으로 중요한 차이가 있습니다: 

1. 적용후: 그림자가 덜 퍼졌음
2. 적용후: 구가 밝아짐 

법선을 향해 더 적은 광선이 뿌려지고, 밝은 광선이 uniform하게 뿌려져서 두 변화가 생겼습니다. 난반사 물체는 카메라를 향해 더 많은 빛이 튕기기 때문에 *더 밝습니다*. 그림자의 경우, 더 적게 반사되며, 밝은 빛때문에 더 밝아보입니다. 

## 난반사 공식의 대안

위에서 제시된 람버르티안 난반사는 부정확한 근사치가 입증되기 전까지 꽤 오랬동안 사용됐습니다. 문제를 오랬동안 풀지 못했던 이유는 다음과 같습니다:

1. 수학적으로 분포가 틀렸음을 증명하는 것 
2.  $cos(\phi)$ 가 왜 이상적인지 설명하는 것

일상생활의 물체는 완벽하게 반사가 이루어지고 있으므로, 우리가 만든 렌더링으로 빛을 쏘면 어색합니다. 

접점에서 넓은 각도로 unifrom하게 뿌리는 방법은 법선의 각도와 관계가 없는 것입니다. 많은 레이트레이싱 논문은 이 난반사 방법을 사용했습니다(람버르티안 난반사를 적용하기 전).

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

![6](/assets/images/2021-06-20/c7/Untitled6.png)