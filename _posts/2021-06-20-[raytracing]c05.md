--- 
title: (RayTracing) 법선 셰이딩과 Hittable 추상화
tags: haskell graphics raytracing
mathjax: true
---

## 법선으로 셰이딩 만들어보기

이제 물체의 여부 뿐만 아니라, 셰이딩을 적용해봅시다. 아직 빛이나 다른 요소를 추가하지 않았으니, 법선(normal vector)을 색으로 표현합니다. 법선은 교점에서 수직입니다. 외향법선(outward normal)은 구의 중심에서 접점을 뺀 방향을 가집니다. 법선을 만들때 두가지 고려사항이 있습니다.

첫번째 고려사항은 단위 법선 벡터입니다. 셰이딩에서 단위벡터를 사용하는 관습이 있긴하지만, 코드에 꼭 적용해야 하는 것은 아닙니다. 대부분의 구조가 그렇듯 개인의 자유를 따릅니다. 

![0](/assets/images/2021-06-20/c5/Untitled0.png)

법선벡터 → 단위벡터 → 0~1 → normalize → x/y/z ⇒ r/g/b  순서로 변환합니다. 

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

![1](/assets/images/2021-06-20/c5/Untitled1.png)

## Ray-Sphere 충돌 코드 최적화

hitSphere 함수는 레이 트레이서에서 가장 많은 연산을 필요로 하는 부분입니다. 근의 공식을 다시 살펴봅시다.

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

- 첫번째로 벡터 스스로 Dot Product 한것은 벡터 길이의 제곱근과 같습니다.
- 이차방정식에서 **b**를 **2h**로 바꿔 `(2*)` 연산을 줄일 수 있습니다.

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

위 수식을 따라 코드를 바꿔봅시다. 결과적으로 `(*)` 수식의 최적화를 통해 연산을 줄였습니다.

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

## Hittable Object 추상화

이젠 구 여러개를 world에 배치할 차례입니다. 여러개를 배열을 사용해도 되지만, "추상 클래스"를 사용해 공통적인 부분을 "Object Oriented Programming"로 처리하는 것도 좋은 방법입니다. 

이 `Hittable` 추상 클래스는 광선을 받는 `hit` 함수를 가지고 있습니다. 대부분의 레이트레이서는 $t_{min}$ 부터 $t_{max}$ 까지 방정식의 해가 유효한 구간을 정합니다. 

`HitRecord` 로 이전 접점의 정보와 광선을 가지고 있습니다.

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

## 앞 / 뒤 표면

두번째 고려사항은 법선의 방향입니다. 현재는 중심에서 접점까지의 방향입니다. 만약 광선이 구 밖에서 부딪힌다면, 법선은 광선의 반대방향입니다. 만약 광선이 구 안에서 부딪힌다면, 법선은 광선의 방향과 같습니다(항상 밖으로 향합니다). 대신 법선이 항상 광선의 반대로 할 수 있는 방법이 있습니다. 만약 광선이 구 밖이라면 바깥방향으로, 광선이 구 안이라면 법선을 안으로 향하게 합니다. 

![2](/assets/images/2021-06-20/c5/Untitled2.png)

광선이 어느 방향에서 날아와 표면에 부딪혔는지 고려해야합니다. 객체가 각 면마다 다르게 렌더링 될 수 있습니다. 양면을 가지거나, 유리공 처럼 안과 밖 둘다를 가지고 있을 때처럼 말이죠.

 법선이 바깥을 향하기로 정했다면, 어느 쪽 광선으로 색칠을 할지 정해야합니다. 법선과 광선을 비교하여, 광선과 법선이 같은방향이라면, 물체 안쪽에서 왔습니다; 광선과 법선이 반대방향인 경우 바깥쪽입니다. 두 벡터의 내적의 값이 양수 일경우 안쪽 방향입니다. 

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

`frontFace` 불린 타입을 `HitRecord` 에 추가해 법선의 방향을 기록합니다.

## HittableList Object

`Hittable` Object 여러개에서 `Ray` 의 충돌여부를 알 수 있도록 클래스를 만들어봅시다.

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

![3](/assets/images/2021-06-20/c5/Untitled3.png)