---
title: (Ray Tracing in One Weekend) 3. Vec3 클래스 
tags: haskell graphics ray-tracing
---

 이글은 번역 및 하스켈로 클론코딩 공부를 목적으로 진행합니다. 번역된 글과 코드에 문제가 있을 수 있음을 참고바랍니다.

<!--more-->

# Vec3 클래스

거의 모든 그래픽스 프로그램은 벡터와 컬러 클래스를 가지고 있습니다. 많은 시스템들이 4D(3D에 homogeneous coordinate를 더한 기하와 RGB에 alpha를 추가한 컬러)입니다. 강의의 목적으로는 3D 좌표계만으로도 충분합니다. 저희는 `vec3` 클래스로 컬러, 위치, 방향, 좌표 등등 여러 방도로 사용할겁니다. 몇몇 분들은 이 구조가 맘에 안들 수도 있습니다. location에 color를 더한다던가 말이죠. 좋은 지적입니다만, 저희는 틀리지 않다면 "적은 코드"로 진행하기를 중점에 두고 있습니다. 저희는 `vec3` 의 alias인 `point3`, `color` 를 선언합니다. 두 타입이 `vec3` 의 alias라고 해서 혼동할 걱정은 하지 마세요. 

## 구현

Vector.hs

```haskell
module Vector where

import           Control.Applicative

data Vec3 a =
  Vec3
    { _x :: a
    , _y :: a
    , _z :: a
    }
  deriving (Eq)

type Vec = Vec3 Float

type Point = Vec3 Float

type Color = Vec3

instance Show a => Show (Vec3 a) where
  show = foldMap ((++ " ") . show)

instance Functor Vec3 where
  fmap f (Vec3 a b c) = Vec3 (f a) (f b) (f c)

instance Foldable Vec3 where
  foldMap f (Vec3 a b c) = f a `mappend` f b `mappend` f c

instance Applicative Vec3 where
  pure a = Vec3 a a a
  Vec3 a b c <*> Vec3 d e f = Vec3 (a d) (b e) (c f)

instance Num a => Num (Vec3 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Vec3 a) where
  (/) = liftA2 (/)
  recip = fmap (1 /)
  fromRational = pure . fromRational

vDot :: Num a => Vec3 a -> Vec3 a -> a
vDot v1 v2 = sum $ v1 * v2

vLength :: Floating a => Vec3 a -> a
vLength v = sqrt $ vDot v v

vLengthSquared :: Floating a => Vec3 a -> a
vLengthSquared v = vDot v v

vUnit :: Floating b => Vec3 b -> Vec3 b
vUnit v = fmap (* k) v
  where
    k = recip . vLength $ v

vCross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
vCross v1 v2 = Vec3 x y z
  where
    x = (_y v1 * _z v2) - (_z v1 * _y v2)
    y = negate $ (_x v1 * _z v2) - (_z v1 * _x v2)
    z = (_x v1 * _y v2) - (_y v1 * _x v2)
```

Main.hs 

```haskell
module Main where

import qualified Data.ByteString.Char8 as C

import           Vector

data Size =
  Size
    { width  :: Int
    , height :: Int
    }

makeSimplePPM :: Size -> String
makeSimplePPM size =
  let coords = (,) <$> reverse [0 .. height size - 1] <*> [0 .. width size - 1]
   in unlines $ "P3" : size' : "255" : map (show . toColor) coords
  where
    size' = unwords . map show $ [width size, height size]
    toColor (y, x) =
      truncate . (* 255.999) <$>
      Vec3
        (toFloat x / ((+ (-1)) . toFloat . width) size)
        (toFloat y / ((+ (-1)) . toFloat . height) size)
        0.25
      where
        toFloat x = fromIntegral x :: Float

main :: IO ()
main = C.writeFile "res.ppm" . C.pack $ makeSimplePPM (Size 256 256)
```