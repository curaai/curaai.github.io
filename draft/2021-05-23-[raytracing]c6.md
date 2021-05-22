---
title: (Ray Tracing in One Weekend) 6. Shading with Surface Normals
tags: haskell graphics ray-tracing
---

## Shading with Surface Normals

셰이딩을 사용할 수 있게 법선을 만들어 봅시다. 이 벡터는 표면의 교점에서 수직입니다. 법선을 만들때 두가지 사항을 고려할 수 있습니다. 첫번째는 법선이 단위 벡터인겁니다. 셰이딩을 만들때 편하긴 합니다만, 코드에서 강제하진 않을겁니다. 단위벡터를 사용할때 사소한 버그가 생길 수 있으니 알아두시고, 개인의 선호에 따르세요. 구를 예를들어 바깥 법선은  중심에서 접점을 뺀 방향입니다. 

![0](/assets/images/2021-05-23/Untitled0.png)

아직 빛이나 다른 요소를 추가하지 않았으니, 법선을 색으로 표현해봅시다. 법선벡터 → 단위벡터 → 0~1 → normalize → x/y/z ⇒ r/g/b  순서로 변환합니다. 법선을 사용하기 위해 부딪혔는지의 여부뿐만 아니라 접점에 대한 정보도 필요합니다. 아직은 화면에 구가 하나밖에 없고, 카메라 정면에 위치하니 아직은 t의 음수값에 대해 생각하진 않겠습니다. 

```haskell
ray2color :: Ray -> Vec3 Float
ray2color r =
  let t = hitSphere (Vec3 0 0 (-1)) 0.5 r
   in if 0 < t
        then (* 0.5) . (+ 1) . vUnit $ at r t - Vec3 0 0 (-1)
        else let t' = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
              in pure (1 - t') + pure t' * Vec3 0.5 0.7 1.0
  where
    hitSphere ctr radius ray =
      let discriminant = b * b - 4 * a * c
       in if discriminant < 0
            then -1
            else ((-b) - sqrt discriminant) / (2 * a)
      where
        oc = origin ray - ctr
        a = vDot (direction ray) (direction ray)
        b = vDot oc (direction ray) * 2
        c = vDot oc oc - radius ^ 2
```

![1](/assets/images/2021-05-23/Untitled1.png)

## Simplifying the Ray-Sphere Intersection Code

hitSphere 함수를 다시 살펴봅시다. 

```haskell
hitSphere ctr radius ray =
  let discriminant = b * b - 4 * a * c
   in if discriminant < 0
        then -1
        else ((-b) - sqrt discriminant) / (2 * a)
  where
    oc = origin ray - ctr
    a = vDot (direction ray) (direction ray)
    b = vDot oc (direction ray) * 2
    c = vDot oc oc - radius ^ 2
```

첫번째로 벡터 스스로 dot-product하는 것은 벡터 길이의 제곱근과 같습니다. 

두번째는 b의 방정식은 제곱이 들어가 있습니다. 2차방정식에서 b를 2h로 바꾸면 어떻게 될지 봅시다. 

$$\begin{align*} 
\frac{ -b \pm  \sqrt{ b^2 - 4ac} }  {2a}  
\end{align*}

\\

\begin{align*} 
= \frac{ -2h \pm  \sqrt{ (2h)^2 - 4ac} }  {2a}
\end{align*}

\\

\begin{align*} 
= \frac{ -2h \pm  2\sqrt{ h^2 - ac} }  {2a}
\end{align*}

\\

\begin{align*} 
= \frac{ -h \pm  \sqrt{ h^2 - ac} }  {a}
\end{align*}$$

위 수식으로 부터, 코드를 최적화할 수 있습니다. 

```haskell
hitSphere ctr radius ray =
  let discriminant = halfB ^ 2 - a * c
   in if discriminant < 0
        then -1
        else ((-halfB) - sqrt discriminant) / a
  where
    oc = origin ray - ctr
    a = vLengthSquared . direction $ ray
    halfB = vDot oc (direction ray)
    c = vLengthSquared oc - radius ^ 2
```

## An Abstraction for Hittable Objects

구 여러개는 어떨까? 구 여러개를 처리하기 위해 배열을 사용하는게 편하겠지만, "추상 클래스"를 사용해 공통적인 부분을 "Object Oriented Programming"로 처리하는 것도 좋은 방법이다. 

이 `hittable` 추상 클래스는 광선을 받는 hit 함수를 가지고 있다. 대부분의 레이트레이서는 $t_{min}$ 부터 $t_{max}$ 까지 유효한 구간을 정해,   $t_{min} < t < t_{max}$ 사이에서만 hit를 "센다". 구조상 고려해야 할점은 접점의 노말을 계산할지다. hit가 계속 될 때마다, 접점마다 이전 접점의 normal을 `hit_record` 구조체에 저장한다. 

```haskell
module Hittable.Hittable where

import           Ray
import           Vector

data HitRecord =
  HitRecord
    { hitPoint     :: Point
    , hitNormal    :: Vec3 Float
    , hitT         :: Float
    }
  deriving (Show)

data HitRange =
  HitRange
    { hitTMin :: Float
    , hitTMax :: Float
    }
  deriving (Show, Eq)

isInRange v range = hitTMin range < v && v < hitTMax range

class Hittable a where
  hit :: a -> Ray -> HitRange -> Maybe HitRecord
```

```haskell
module Hittable.Sphere where

import           Data.Maybe

import           Hittable.Hittable
import           Ray
import           Vector

data Sphere =
  Sphere
    { sphereCenter :: Point
    , sphereRadius :: Float
    }
  deriving (Show)

instance Hittable Sphere where
  hit sp ray hitRange hitRecord
    | discriminant < 0 = Nothing
    | isNothing nearestRoot = Nothing
    | otherwise = do
      let t = fromJust nearestRoot
      let p = at ray t
      let normal = (p - sphereCenter sp) / (pure . sphereRadius) sp
      return $ HitRecord p normal t
    where
      nearestRoot
        | isInRange rootMinus hitRange = Just rootMinus
        | isInRange rootPlus hitRange = Just rootPlus
        | otherwise = Nothing
        where
          rootMinus = ((-halfB) - sqrt discriminant) / a
          rootPlus = ((-halfB) + sqrt discriminant) / a
      (discriminant, halfB, a) = getRoot sp ray
      getRoot sp ray = (discriminant, halfB, a)
        where
          discriminant = halfB ^ 2 - a * c
          oc = origin ray - sphereCenter sp
          a = vLengthSquared . direction $ ray
          halfB = vDot oc (direction ray)
          c = vLengthSquared oc - sphereRadius sp ^ 2
```

## Front Faces Versus Back Faces

두번째 고려사항은 법선을 어느방향으로 나가게 할 것 인지다. 현재 법선은 중심에서 접점까지의 방향이다. 만약 광선이 구 밖에서 부딪힌다면, 법선은 광선의 반대방향이다. 만약 광선이 구 안에서 부딪힌다면, 법선은 광선의 방향과 같다. 대신에 법선을 항상 광선의 반대로 할 수 있는 방법이 있다. 만약 광선이 구 밖이라면 바깥방향으로, 광선이 구 안이라면 법선을 안으로 향하게 한다. 

![2](/assets/images/2021-05-23/Untitled2.png)

우리는 이 가능성을 고려해야 한다. 왜나하면 우리는 광선이 표면 어느 쪽에서 알아야 하기 때문이다. 양면을 가지거나, 유리공 처럼 안과 밖 둘다를 가지고 있을 때처럼 객체가 각 면마다 다르게 렌더링 될때 이점이 중요하다. 

 법선이 바깥을 향하기로 정했다면, 어느 쪽 광선으로 색칠을 할지 정해야한다. 법선과 광선을 비교하여, 광선과 법선이 같다면 물체 안쪽, 광선과 법선이 반대라면 광선은 물체 바깥이다. 

```haskell
data HitRecord =
  HitRecord
    { hitPoint     :: Point
    , hitNormal    :: Vec3 Float
    , hitT         :: Float
    , hitFrontFace :: Bool
    }
  deriving (Show)
```

```haskell
instance Hittable Sphere where
  hit sp ray hitRange
    | discriminant < 0 = Nothing
    | isNothing nearestRoot = Nothing
    | otherwise = do
      let t = fromJust nearestRoot
      let p = at ray t
      let normal = (p - sphereCenter sp) / (pure . sphereRadius) sp
      let frontFace = vDot (direction ray) normal < 0
      return $
        HitRecord
          p
          (if frontFace
             then normal
             else negate normal)
          t
          frontFace
```

`frontFace` 불린 타입을 `HitRecord` 에 추가해 법선의 방향을 기록한다. 

## HittableList Object

```haskell
module Hittable.HittableList where

import           Data.Maybe
import           Hittable.Hittable

instance (Hittable a) => Hittable [a] where
  hit a ray rr = foldl f Nothing a
    where
      f :: Hittable a => Maybe HitRecord -> a -> Maybe HitRecord
      f hr obj =
        let res = hit obj ray (rr' hr)
         in if isNothing res
              then hr
              else res
        where
          rr' hr = HitRange (hitTMin rr) (maybe (hitTMax rr) hitT hr)
```

### New Main

```haskell
render :: Hittable a => Camera -> a -> [(Float, Float)] -> [Vec3 Float]
render cam objs = map (ray2color objs . pos2ray cam)

ray2color :: Hittable a => a -> Ray -> Vec3 Float
ray2color objs r =
  let hr = hit objs r (HitRange 0 maxValue)
   in maybe backgroundRayColor ((* 0.5) . (+ 1) . hitNormal) hr
  where
    backgroundRayColor =
      let t' = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
       in pure (1 - t') + pure t' * Vec3 0.5 0.7 1.0
```

```haskell
main :: IO ()
main =
  C.writeFile "res.ppm" . C.pack $
  drawImg (Size 400 (truncate (400 / aspectRatio))) spheres
  where
    spheres = [Sphere (Vec3 0 0 (-1)) 0.5, Sphere (Vec3 0 (-100.5) (-1)) 100]
```

코드를 위와 같이 변경하여 다음 결과를 얻었다. 

![3](/assets/images/2021-05-23/Untitled3.png)