--- 
title: (RayTracing) Ray, 카메라, 배경
tags: haskell graphics raytracing
mathjax: true
---

레이트레이서는 광선를 따라가 픽셀의 색을 추정합니다. 광선을 함수로 생각해보면 $P(t) = A + tB$입니다.  **P**는 3D 공간에서 선을 그리는 1차 방정식이고, **A**는 원점, **B**는 방향, 실수 **t**는 광선의 변수입니다. 변수 **t**에 따라 **P(t)**는 직선 위의 점을 움직입니다. **t**를 양수로 두면, **A**의 앞으로만 직진할 수 있습니다. 이를 반직선 혹은 광선이라고 부릅니다. 

![0](/assets/images/2021-06-20/c3/Untitled0.png)

함수 **P(t)**는 코드에서 `at ray t` 로 표현합니다. 

```haskell
module Ray where

import           Vector

data Ray =
  Ray
    { origin    :: Point
    , direction :: Vec
    }
  deriving (Show, Eq)

at :: Ray -> Float -> Point
at ray power = origin ray + direction ray * pure power
```

## 화면에 Ray 쏘기

 이제 레이 트레이서를 만들 때가 됐습니다. 레이 트레이서는 픽셀마다 빛를 쏘고 색을 결정합니다. 단계로 나눠보자면 아래와 같습니다. 

1. 눈에서 픽셀로 향하는 광선을 계산합니다. 
2. 광선과 부딪치는 물체를 찾습니다.
3. 부딪치는 지점의 색을 계산합니다. 

처음 레이 트레이서를 개발할때, 저는 항상 코드가 빠르고 간편하게 짤 수 있게, 카메라를 간단하게 만듭니다. `ray_color(ray)` 배경(그래디언트)의 색을 반환하는 함수를 만듭니다.

정사각형 이미지로 작업하다보면, x와 y가 서로 바뀌어 디버깅이 힘든 경우가 종종 있습니다. 그래서 저는 실제환경에서도 자주 사용되는 16:9  직사각형 이미지를 사용합니다. 

광선을 쏠 가상의 viewport가 필요합니다. 해상도가 깨지지 않게 viewport와 결과 이미지의 종횡비(aspect ratio)는 같아야 합니다.

높이의 두 유닛을 고릅니다. 투영 평면과 투영점 사이의 거리가 첫번째 유닛으로 초점 길이(focal length)라고 합니다. 

> 투영 평면과 초점 사이의 거리를 "초점 길이"라고 합니다.

초점 길이는 첫번째 유닛입니다. 초점이 맞는 피사체까지의 거리, 초점 거리(focusing distance), 와 헷깔리지 마십시오.

*초점 길이와 관련해서, 월드에서 가상의 카메라를 놓고 이미지를 찍어내는 것이 궁금하다면, "이미지 투영"을 키워드로 조사해보시길 바랍니다.*

- **eye**(카메라의 중앙): (0, 0, 0)
- x-axis: right
- y-axis: up
- z-axis: RHS(오른손 좌표 시스템)을 기준으로 screen의 반대방향

화면 좌상단 모서리에서 부터 화면 양측을 따라 광선을 발사합니다. 이때 광선의 방향은 꼭 단위벡터로 할 필요가 없습니다.

![1](/assets/images/2021-06-20/c3/Untitled1.png)

코드에 따라, 광선 `r` 이 픽셀 중앙과 차이날 수 있습니다. (어차피 antialiasing을 적용할테니, 정확도는 걱정안하셔도 됩니다) 

Camera.hs

```haskell
module Camera where

import           Ray
import           Vector

data Size a =
  Size
    { width  :: a
    , height :: a
    }
  deriving (Show, Eq)

data Camera =
  Camera
    { viewportSize :: Size Float
    , cameraPos    :: Vec3 Float
    , cameraDir    :: Vec3 Float
    , focalLength  :: Float
    }
  deriving (Show)

camHVec :: Camera -> Vec3 Float
camHVec cam = Vec3 (width . viewportSize $ cam) 0.0 0.0

camVVec :: Camera -> Vec3 Float
camVVec cam = Vec3 0 (height . viewportSize $ cam) 0.0

camLVec :: Camera -> Vec3 Float
camLVec cam = Vec3 0.0 0 (focalLength cam)

lowerLeftCorner :: Camera -> Vec3 Float
lowerLeftCorner cam =
  cameraPos cam - camHVec cam * 0.5 - camVVec cam * 0.5 - camLVec cam

render :: Camera -> [(Float, Float)] -> [Color Float]
render cam = map (ray2color . pos2ray)
  where
    ray2color r =
      let t = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
       in pure (1 - t) + pure t * Vec3 0.5 0.7 1.0
    pos2ray (u, v) =
      let llc = lowerLeftCorner cam
       in Ray
            (cameraPos cam)
            (llc + pure u * camHVec cam + pure v * camVVec cam - cameraPos cam)

vec2color :: (Floating c, RealFrac c, Integral a) => c -> Vec3 c -> Vec3 a
vec2color spp v = Vec3 (_x cv) (_y cv) (_z cv)
  where
    cv = fmap (truncate . (* 256) . (max 0 . min 0.999)) v
```

Main.hs

```haskell
module Main where

import qualified Data.ByteString.Char8 as C

import           Camera
import           Vector

aspectRatio = 16 / 9

drawImg :: (Show a, Integral a) => Size a -> String
drawImg size = unlines $ "P3" : size' : "255" : map (show . writeColor) arr
  where
    arr = render cam $ map relative coords
      where
        cam =
          Camera
            (Size viewportHeight (viewportHeight / aspectRatio))
            (pure 0)
            (Vec3 0 0 focalLength')
            focalLength'
          where
            viewportHeight = 2
            focalLength' = 1
        coords =
          (,) <$> reverse [0 .. height size - 1] <*> [0 .. width size - 1]
        relative (y, x) = (func width x, func height y)
          where
            func f v = toFloat v / ((+ (-1)) . toFloat . f) size
            toFloat x = fromIntegral x :: Float
    writeColor = fmap (truncate . (* 255.999))
    size' = unwords . map show $ [width size, height size]

main :: IO ()
main =
  C.writeFile "res.ppm" . C.pack $
  drawImg (Size 400 (truncate (400 / aspectRatio)))
```

`ray_color(ray)` 함수는 광선의 방향을 단위벡터화 한 `y` 값에 따라 선형 블렌딩했습니다. 

0 < `t` < 1로 스케일링 후, `t = 1`일때 파란색, `t = 0` 일때 흰색이 나옵니다. 이런 형식을 "linear blend", "linear interpolation, 선형보간법", 짧게는 "lerp"라고 합니다. 선형보간법은 항상 이런형식을 가집니다.

$$blendedValue = (1-t) \cdot startValue + t \cdot endValue$$

아래는 결과입니다. 

![Ray,%20%E1%84%8F%E1%85%A1%E1%84%86%E1%85%A6%E1%84%85%E1%85%A1,%20%E1%84%87%E1%85%A2%E1%84%80%E1%85%A7%E1%86%BC%20c87e26dcc3f6489b85a7fbf80376c97a/Untitled%202.png](Ray,%20%E1%84%8F%E1%85%A1%E1%84%86%E1%85%A6%E1%84%85%E1%85%A1,%20%E1%84%87%E1%85%A2%E1%84%80%E1%85%A7%E1%86%BC%20c87e26dcc3f6489b85a7fbf80376c97a/Untitled%202.png)