--- 
title: (RayTracing) 간단한 이미지 만들기
tags: haskell graphics raytracing
mathjax: true
---

## PPM 이미지 형식

 렌더러를 처음 만든다면, 이미지를 보여줄 방법이 필요합니다. 가장 간단한 방법은 파일로 저장하는 겁니다. 이미지 파일은 많은 형식이 있고, 복잡한 포맷인 경우도 많습니다. 여기서는 이미지를 만드는 가장 쉬운 방법인 text 만을 가지고 이미지를 만들 수 있는 ppm 형식으로 시작합니다. *자세한 것은 위키를 참고하세요*

![0](/assets/images/2021-06-20/c1/Untitled0.png)

저는 하스켈로 간단히 만들어 봤습니다. 

```haskell
module Main where

import qualified Data.ByteString.Char8 as C

data Color a =
  Color
    { red   :: a
    , green :: a
    , blue  :: a
    }

instance Functor Color where
  fmap f (Color r' g' b') = Color (f r') (f g') (f b')

instance Show a => Show (Color a) where
  show (Color r' g' b') = unwords . map show $ [r', g', b']

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
      Color
        (toFloat x / ((+ (-1)) . toFloat . width) size)
        (toFloat y / ((+ (-1)) . toFloat . height) size)
        0.25
      where
        toFloat x = fromIntegral x :: Float

main :: IO ()
main = C.writeFile "res.ppm" . C.pack $ makeSimplePPM (Size 256 256)
```

몇가지 노트입니다.

1. 한줄당 왼쪽에서 오른쪽으로 픽셀을 그립니다. 
2. 위에서 아래로 한줄씩 그립니다. 
3. 관례에 따라 red/green/blue는 0.0 ~ 1.0 범위를 가집니다. 
4. 빨간색은 왼쪽(검정)에서 오른쪽(밝은 빨강)으로 보간되고, 초록색은 아래(검정)에서 위(밝은 초록)로 보간됩니다. 빨간색과 초록색이 합쳐서 오른쪽 상단이 노란색이 나오는 것을 예상할 수 있습니다. 

## 이미지파일 만들기

프로그램의 결과는 stdout으로 출력되는데, 이를 이미지로 변환해야합니다. 일반적으로 cmd에서는 `>` redirection으로 가능합니다.

```bash
RayTracinginOneWeekend.exe > image.ppm
```

이는 윈도우의 방식이고, 맥이나 리눅스도 동일합니다.

```bash
RayTracinginOneWeekend > image.ppm
```

*저는 이미지 출력대신에 코드내에서 파일로 바로 저장하는 방식을 차용했습니다. stdout은 log message/debugging, stderr는 에러 감지를 염두에 뒀습니다.*

결과파일을 이미지뷰어로 실행시 아래 이미지가 나옵니다.

![1](/assets/images/2021-06-20/c1/Untitled1.png)

그래픽에서의 `"Hello World"` 를 만들었네요! 만약 결과가 같지 않다면, 텍스트 편집기로 비교해보세요. 

```
P3
256 256
255
0 255 63
1 255 63
2 255 63
3 255 63
4 255 63
5 255 63
6 255 63
7 255 63
8 255 63
9 255 63
...
```

만약 안된다면, 개행이나 비슷한 부분에서 이미지뷰어가 읽지 못하는 걸겁니다. 

PPM이 아닌 다른 이미지 형식으로 만들고 싶다면, 저는 `stb_image.h`  를 추천합니다. 헤더파일만으로 가능한 이미지 라이브러리 이며 [깃허브](https://github.com/nothings/stb)에서 참고 가능합니다.