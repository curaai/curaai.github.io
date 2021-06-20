--- 
title: (RayTracing) 메탈
tags: haskell graphics raytracing
mathjax: true
---

다른 재질을 시도해볼 차례입니다. `Material` 추상 클래스를 이용해서 캡슐화할 겁니다. `Material`은 다음 두가지 기능이 필요합니다.

1. 광선 생산 
2. 반사된 광선 약화

```haskell
data Material =
  forall a. Scatterable a =>
            Material a

data Scattered =
  Scattered
    { scatteredRay     :: Ray
    , attenuationColor :: Vec3 Float
    }

class Scatterable a where
  scatter :: (RandomGen g) => a -> Ray -> HitRecord -> g -> (Maybe Scattered, g)
```

## Material and HitRecord

`hitRecord` 와 `Material` 은 서로를 참고하기 때문에, circular import problem이 발생할 수 있습니다. 하스켈에서는 같은 모듈안에 있기만 하다면 별다른 문제가 발생하지 않기 때문에 넘어가겠습니다.

광선이 물체의 표면에 부딪혔을 때, `hitRecord`에 재질을 저장합니다. `ray_color()` 함수에서 `Material` 을 알아낼 수 있습니다.

```haskell
data HitRecord =
  HitRecord
    { hitPoint     :: Point
    , hitNormal    :: Vec3 Float
    , hitT         :: Float
    , hitFrontFace :: Bool
    , hitMaterial  :: Material
    }

data Sphere =
  Sphere
    { sphereCenter   :: Point
    , sphereRadius   :: Float
    , sphereMaterial :: Material
    }
```

## 광선 발산과 반사 구상하기

Lambertian(난반사)는 이미 가지고 있습니다. 광선의 반사도 **R**와 **1 - R**로 광선을 흡수할 수도, 두 방법을 섞을 수도 있습니다. 구현은 아래와 같습니다. 

선택에 따라 반사율 **p**와, 반사도(attenuation)를 **albedo/p**로 둘 수 있습니다. 

```haskell
data Lambertian =
  Lambertian
    { albedo :: Color Float
    }

instance Scatterable Lambertian where
  scatter (Lambertian color) ray (HitRecord p normal _ _ _) g =
    (Just (Scattered (Ray p scatterDir) color), g')
    where
      (randUnitVec, g') = sampleUnitVector g
      scatterDir = v = normal + randUnitVec
```

 위 코드를 자세히 보면, 실수를 발견할 수 있습니다. 만약 무작위 단위벡터가 법선과 정반대라면 합이 0 이 되어서, Scatter 광선이 NaN에 빠질 수 있습니다. 이를 검사해야합니다. 

```haskell
scatterDir =
  let v = normal + randUnitVec
   in if vNearZero v
        then normal
        else v
```

## 거울 반사

매끄러운 메탈 재질은 광선을 무작위적으로 뿌리지 않습니다. 

![0](/assets/images/2021-06-20/c8/Untitled0.png)

반사되는 광선(빨간색): $v + 2b$, **n**은 단위벡터지만, **v**는 아마 아닙니다. **b**의 길이는 $v \cdot n$입니다. 

```haskell
vReflect :: Num a => Vec3 a -> Vec3 a -> Vec3 a
vReflect v n = v - pure (2 * vDot v n) * n
```

```haskell
data Metal =
  Metal
    { albedo :: Color Float
    }

instance Scatterable Metal where
  scatter (Metal color) ray (HitRecord p normal _ _ _) g
    | 0 < vDot (direction scatterRay) normal =
      (Just (Scattered scatterRay color), g)
    | otherwise = (Nothing, g)
    where
      reflected = vReflect (vUnit . direction $ ray) normal
      scatterRay = Ray p reflected
```

```haskell
ray2color ::
     (Ord t, RandomGen p, Num t, Hittable a)
  => a
  -> p
  -> t
  -> Ray
  -> (Vec3 Float, p)
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

## 화면에 구 그리기

```haskell
spheres =
      [ Sphere
          (Vec3 0 (-100.5) (-1))
          100
          (Material (Lambertian (Vec3 0.8 0.8 0)))
      , Sphere (Vec3 0 0 (-1)) 0.5 (Material (Lambertian (Vec3 0.7 0.3 0.3)))
      , Sphere (Vec3 (-1) 0 (-1)) 0.5 (Material (Metal (Vec3 0.8 0.8 0.8)))
      , Sphere (Vec3 1 0 (-1)) 0.5 (Material (Metal (Vec3 0.8 0.6 0.2)))
      ]
```

![1](/assets/images/2021-06-20/c8/Untitled1.png)

## 퍼지 반사

반사되는 방향에 작은 구를 만들어 그 안의 새로운점을 향하게 만듭니다. 이것으로 무작위 광선을 만들 수 있습니다. 

![2](/assets/images/2021-06-20/c8/Untitled2.png)

구가 더 커질수록, 더 무작위하게 튑니다. 이것은 fuzzniess값으로 조절할 수 있습니다. 

```haskell
data Metal =
  Metal
    { albedo :: Color Float
    , fuzz   :: Float
    }

instance Scatterable Metal where
  scatter (Metal color f) ray (HitRecord p normal _ _ _) g
    | 0 < vDot (direction scatterRay) normal =
      (Just (Scattered scatterRay color), g)
    | otherwise = (Nothing, g)
    where
      (randUnitSphere, g') = sampleUnitSphere g
      reflected = vReflect (vUnit . direction $ ray) normal
	      scatterRay = Ray p (reflected + (pure . min 1 $ f) * randUnitSphere)
```

```haskell
spheres =
      [ Sphere
          (Vec3 0 (-100.5) (-1))
          100
          (Material (Lambertian (Vec3 0.8 0.8 0)))
      , Sphere (Vec3 0 0 (-1)) 0.5 (Material (Lambertian (Vec3 0.7 0.3 0.3)))
      , Sphere (Vec3 (-1) 0 (-1)) 0.5 (Material (Metal (Vec3 0.8 0.8 0.8) 0.3))
      , Sphere (Vec3 1 0 (-1)) 0.5 (Material (Metal (Vec3 0.8 0.6 0.2) 1.0))
      ]
```