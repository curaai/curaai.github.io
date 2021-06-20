--- 
title: (RayTracing) 카메라 움직이기
tags: haskell graphics raytracing
mathjax: true
---

카메라는 유전체처럼 디버깅하기 힘듭니다. 그래서 나는 항상 천천히 기능을 추가하며 개발합니다. 첫번째로 시야(FOV, Field of View)를 조정가능하게 해봅시다. 저희 이미지는 정사각형이 아니기 때문에, 수평/수직으로 봤을 때 각각이 다릅니다.  저는 항상 수직 시야를 사용합니다. 생성자에 각도를 받아 사용자의 취향에 맞게 바꿀 수 있게 합니다. 

## 카메라 각도의 기하

난 항상 광선이 원점에서 시작해 `z = -1` 평면을 향하게 합니다. 거리 비율에 따라 h를 늘려 `z=-2` 평면이나 그 이상을 만들 수 있습니다. 

![0](/assets/images/2021-06-20/c10/Untitled0.png)

$h = tan ( \frac {\theta}{2})$로 우리 카메라를 바꿨습니다: 

```haskell
aspectRatio = 16 / 9

drawImg :: Hittable a2 => Size Int -> a2 -> String
drawImg size hittables = unlines $ "P3" : size' : "255" : map show arr
  where
    arr = render cam size 50 50 hittables
      where
        cam = mkCamera 90 aspectRatio
    size' = unwords . map show $ [width size, height size]

mkCamera vfov aspectRatio =
  Camera
    (Size (aspectRatio * viewportHeight) viewportHeight)
    (pure 0)
    (Vec3 0 0 1)
    1
  where
    degree2radian x = x * pi / 180
    theta = degree2radian vfov
    h = tan $ theta / 2
    viewportHeight = 2 * h

main :: IO ()
main =
  C.writeFile "res.ppm" . C.pack $
  drawImg (Size 400 (truncate (400 / aspectRatio))) spheres
  where
    r = cos $ pi / 4
    spheres =
      [ Sphere (Vec3 (-r) 0 (-1)) r (Material (Lambertian (Vec3 0 0 1))) -- left
      , Sphere (Vec3 r 0 (-1)) r (Material (Lambertian (Vec3 1 0 0))) -- right
      ]
```

## 카메라의 위치와 방향 조절하기

뷰포인트를 얻기위해 우리는 용어를 정리할 필요가 있습니다. 카메라의 위치를 "lookfrom", 카메라가 바라보는 곳을 "lookat"이라고 합시다.

또한 우리는 카메라의 각 각도에 대해서 정의할 필요가 있습니다. 우리는 "up" 벡터를 정의해야 합니다. 이 벡터는 보는 방향의 평면에 수직이어야 합니다. 

![1](/assets/images/2021-06-20/c10/Untitled1.png)

네이밍 컨벤션으로 "view up" **vup** 벡터로 부릅니다. 카메라의 방향과 **vup** 벡터의 외적으로 정규 직교 기저 (u, v, w)를 만들 수 있습니다. 

![2](/assets/images/2021-06-20/c10/Untitled2.png)

```haskell
data Camera =
  Camera
    { lookFrom        :: Point
    , horizontal      :: Vec3 Float
    , vertical        :: Vec3 Float
    , lowerLeftCorner :: Point
    }
  deriving (Show)

pos2ray :: Camera -> (Float, Float) -> Ray
pos2ray (Camera origin' horizontal' vertical' llc) (u, v) =
  Ray origin' (llc + pure u * horizontal' + pure v * vertical' - origin')
```

```haskell
mkCamera :: Point -> Point -> Vec3 Float -> Float -> Float -> Camera
mkCamera lookfrom lookat vup vfov aspectRatio =
  Camera
    lookfrom
    horizontal'
    vertical'
    (lookfrom - horizontal' / pure 2 - vertical' / pure 2 - w)
  where
    degree2radian x = x * pi / 180
    theta = degree2radian vfov
    h = tan $ theta / 2
    viewportHeight = 2 * h
    viewportWidth = aspectRatio * viewportHeight
    w = vUnit $ lookfrom - lookat
    u = vUnit $vCross vup w
    v = vCross w u
    horizontal' = pure viewportWidth * u
    vertical' = pure viewportHeight * v
```

```haskell
main :: IO ()
main =
  C.writeFile "res.ppm" . C.pack $
  drawImg (Size 400 (truncate (400 / aspectRatio))) spheres
  where
    -- r = cos $ pi / 4
    spheres =
      [ Sphere (Vec3 0 (-100.5) (-1)) 100 materialGround
      , Sphere (Vec3 0 0 (-1)) 0.5 materialCenter
      , Sphere (Vec3 (-1) 0 (-1)) 0.5 materialLeft
      , Sphere (Vec3 (-1) 0 (-1)) (-0.45) materialLeft
      , Sphere (Vec3 1 0 (-1)) 0.5 materialRight
      ]
    materialGround = Material (Lambertian (Vec3 0.8 0.8 0))
    materialCenter = Material (Lambertian (Vec3 0.1 0.2 0.5))
    materialLeft = Material (Dielectric 1.5)
    materialRight = Material (Metal (Vec3 0.8 0.6 0.2) 0)
```

![3](/assets/images/2021-06-20/c10/Untitled3.png)