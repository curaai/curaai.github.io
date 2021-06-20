--- 
title: (RayTracing) 흐린 초점
tags: haskell
mathjax: true
---

이제 마지막 챕터에 다다렀습니다: 흐린초점(defocus blur). 사진작가들은 "피사계 심도(depth of field)"라고도 하니 단어를 너무 신경쓰지마십시오. 

실제 카메라는 빛을 받을 큰 구멍(핀홀보다 큰)이 필요합니다. 이것이 초점을 흐리게 합니다. 하지만 구멍에 렌즈를 박으면, 특정 거리안에 모든 것의 초점을 잡을 수 있다. 렌즈를 이렇게 생각할 수 있는데: 초점 거리상의 특정 지점에서 오는 모든 빛은 렌즈를 부딪혀 이미지센서의 한 지점으로 구부러져 옵니다. 

우리는 투영점과 평면사이의 초점이 항상 맞는 거리를 "focus distance"라고 부릅니다. 초점 거리는 focal length와 같지 않다는 것을 명심하세요 - focal length는 투영 점과 이미지 평면 사이의 거리입니다.

실제 카메라에서, focus distance는 렌즈와 이미지 센서의 거리로 조종됩니다. 이것이 초점을 잡으려는 물체를 바꿀때 카메라를 상대로 렌즈를 움직이는 이유입니다(아마 핸드폰에서도 똑같은 움직임을 가지고 있습니다, 대신 센서가 움직이겠지만). 조리개는 구멍을 조절해 크게 할수록 렌즈에 빛을 효과적으로 전달합니다. 만약 빛이 더 필요하다면 조리개를 열면 됩니다. 그대신 흐린초점현상이 더 발생할 것입니다. 우리의 가상 카메라는 완벽한 센서를 가지고 있어, 절대 빛을 더 받을 필요가 없습니다. 그래서 흐린초점이 필요할때만 조리개를 사용하면 됩니다. 

## 얇은 렌즈 근사

실제 카메라는 복잡한 혼합 렌즈를 가지고 있습니다. 우리의 코드는 센서, 렌즈, 조리개 순으로 재현할 것 입니다. 계산한 뒤 광선을 보낸 곳에서 이미지를 뒤집을 것입니다.(위 과정을 거친 이미지는 실제 처럼 뒤집혀 있을 것 이기 때문에), 그래픽스를 다루는 사람은 자주 얇은 렌즈 근사를 사용합니다:

![0](/assets/images/2021-06-20/c11/Untitled0.png)

 카메라 안쪽까지 구현할 필요는 없습니다. 카메라 밖을 이미지로 렌더링하기 위함으로, 필요이상으로 복잡해질 수 있습니다. 대신에 초점이 완벽히 맞았을 때 렌즈에서 시작해 초점 평면을 향해 광선을 발사합니다(`focus_dist` 는 렌즈로부터의 거리입니다).

![1](/assets/images/2021-06-20/c11/Untitled1.png)

## Generating Sample Rays

일반적으로 모든 광선은 `lookfrom` 점에서 시작됩니다. 초점 흐림을 완성하기 위해서는,  `lookfrom` 점을 중심으로 무작위 광선 만들어 낼 필요가 없습니다. 

```haskell
sampleInUnitDisk :: RandomGen g => g -> (Vec3 Float, g)
sampleInUnitDisk g = find (Vec3 1 1 1, g)
  where
    find (v, g')
      | vLengthSquared v < 1 = (v, g2)
      | otherwise = find (Vec3 x' y' 0, g2)
      where
        sampleFloat' g = randomR (-1, 1) g
        (x', g1) = sampleFloat' g'
        (y', g2) = sampleFloat' g1
```

```haskell
drawImg :: Hittable a2 => Size Int -> a2 -> String
drawImg size hittables = unlines $ "P3" : size' : "255" : map show arr
  where
    arr = render cam size 50 50 hittables
      where
        lookfrom = Vec3 3 3 2
        lookat = Vec3 0 0 (-1)
        cam =
          mkCamera
            lookfrom
            lookat
            (Vec3 0 1 0)
            20
            aspectRatio
            2
            (vLength $ lookfrom - lookat)
    size' = unwords . map show $ [width size, height size]

mkCamera ::
     Point
  -> Vec3 Float
  -> Vec3 Float
  -> Float
  -> Float
  -> Float
  -> Float
  -> Camera
mkCamera lookfrom lookat vup vfov aspectRatio aperture focusDist =
  Camera
    lookfrom
    horizontal'
    vertical'
    (lookfrom - horizontal' / pure 2 - vertical' / pure 2 - pure focusDist * w)
    w
    u
    v
    (aperture / 2)
  where
    degree2radian x = x * pi / 180
    theta = degree2radian vfov
    h = tan $ theta / 2
    viewportHeight = 2 * h
    viewportWidth = aspectRatio * viewportHeight
    w = vUnit $ lookfrom - lookat
    u = vUnit $vCross vup w
    v = vCross w u
    horizontal' = pure (focusDist * viewportWidth) * u
    vertical' = pure (focusDist * viewportHeight) * v
```

```haskell
data Camera =
  Camera
    { lookFrom        :: Point
    , horizontal      :: Vec3 Float
    , vertical        :: Vec3 Float
    , lowerLeftCorner :: Point
    , camW            :: Vec3 Float
    , camU            :: Vec3 Float
    , camV            :: Vec3 Float
    , lensRaidus      :: Float
    }
  deriving (Show)

pos2ray :: RandomGen g => Camera -> (Float, Float) -> g -> (Ray, g)
pos2ray cam@(Camera origin' horizontal' vertical' llc cw cu cv lensRaidus') (u, v) g =
  ( Ray
      (origin' + offset)
      (llc + pure u * horizontal' + pure v * vertical' - origin' - offset)
  , g')
  where
    (randUnitDisk, g') = sampleInUnitDisk g
    rd = pure lensRaidus' * randUnitDisk
    offset = cu * (pure . _x) rd + cv * (pure . _y) rd
```