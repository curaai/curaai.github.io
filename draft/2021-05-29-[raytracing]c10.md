---
title: (Ray Tracing in One Weekend) 10. 유전체
tags: haskell graphics ray-tracing
---

물, 유리, 다이아몬드 처럼 투명한 재질은 유전체다. 광선을 맞았을 때, 광선은 반사광과 굴절광으로 나뉜다. 둘중 무작위로 하나만 선택하게 두고, 유전체에 대한 광선을 처리하자. 

## 굴절

디버깅이 가장 어려운것은 굴절된 광선이기 때문에, 굴절된 광선먼저 진행해보자. 먼저 두 유리공을 놨는데, 이 결과를 얻었다(아직은 결과가 제대로 나온것인지 아닌지 알려주지 않겠다) 

![0](/assets/images/2021-05-29/c10/Untitled0.png)

## 스넬의 법칙

스넬의 법칙에서 굴절의 공식을 알 수 있다. 

$$\eta \cdot sin \theta = \eta' \cdot sin \theta'$$

θ와 θ'는 법선에서의 각도이다. η와 η'("에타"와 "에타 프라임")은 굴절률(공기=1.0, 유리=1.3-1.7,다이아몬드=2.4)이다. 

![1](/assets/images/2021-05-29/c10/Untitled1.png)

굴절된 각도를 얻기위해, $sin \theta'$를 풀어야한다:

$$sin \theta' = \frac {\eta} {\eta'} \cdot sin \theta$$

굴절각의 광선 R', 법선 n', 각도 θ'가 있다. 우리는 광선 R'를 직각인 부분과 평행한 부분으로 나눌수 있다:

$$R' = R'_{\perp} + R'_{\parallel}$$

$R'_{\perp}$와 $R'_{\parallel}$를 풀어보면 아래 식을 얻을 수 있다:

$$\begin{align*}

R'_{\perp} = \frac {\eta} {\eta'}(R + cos \theta n) 

\end{align*}

\\

\begin{align*}
R'_{\parallel} = -\sqrt{1-|R'_\perp|^2 }n

\end{align*}$$

만약 원한다면 위 증명을 할 수 있겠지만, 그냥 넘어가도 무방하다. 이후에 나오는 내용은 필수적으로 증명을 요구하지 않는다. 

우리는 $cos \theta$ 를 풀어야한다. 두 벡터의 내적은 아래와 같이 표현할 수 있다:

$$a \cdot b = |a||b| cos \theta $$

만약 벡터 a,b를 유닛벡터로 제한한다면: 

$$a \cdot b = cos \theta $$

$R'_{\perp}$를 위 항으로 다시 쓸 수 있다:

$$R'_{\perp} = \frac {\eta} {\eta'}(R + (-R \cdot n) n) 
$$

이제 R'을 계산하는 함수를 만들어보자: 

```haskell
newtype Dielectric =
  Dielectric
    { ir :: Float
    }

instance Scatterable Dielectric where
  scatter (Dielectric ir') ray (HitRecord p normal _ frontFace _) g =
    (Just (Scattered scattered attenuation), g)
    where
      scattered = Ray p refracted
      attenuation = Vec3 1 1 1
      refracted = refract unitDir normal refractionRatio
        where
          unitDir = vUnit . direction $ ray
          refractionRatio = bool recip id frontFace ir'
          refract uv n etaiOverEtat = outPerp + outParallel
            where
              cosTheta = min (vDot (-uv) n) 1
              outPerp = pure etaiOverEtat * (uv + pure cosTheta * n)
              outParallel =
                (* n) . pure . negate . sqrt . abs $ 1 - vLengthSquared outPerp
```

## Total Internal Reflection

결과가 뭔가 이상해 보인다. 재질이 높은 굴절율을 가졌을 때 광선에 문제가 생긴다. 이때 스넬의 법칙만으로는 해결할 수 없어, 굴절을 사용할 수가 없다. 다시 스넬의 법칙으로 돌아와 $sin \theta'$의 유도를 살펴보자:

$$sin \theta' = \frac {\eta} {\eta'} \cdot sin \theta$$

만약 광선이 안쪽이 유리, 바깥족이 공기라면 (η=1.5, η'=1.0)이다:

$$sin \theta' = \frac {1.5} {1.0} \cdot sin \theta$$

 $sin \theta'$의 값이 1보다 커질수 없다 그러므로: 

$$\frac {1.5} {1.0} \cdot sin \theta > 1.0$$

양변에 대해서 등호가 성립되지 않아, 해가 존재할 수 없다. 해가 없는 경우에는 굴절시키기 보다는 반사를 시킨다. 보통 내부가 고체인 물체는 모든 빛을 반사시키는데, 이를 "전반사(total internal reflection)"라고 한다. 이것이 물-공기의 경계가 마치 완벽한 거울처럼 되는 이유다.

$sin \theta$를 삼각함수로 풀어낼 수 있다: 

$$sin \theta = \sqrt {1 - cos^2 \theta}$$

그리고 

$$cos \theta = R \cdot n $$

```haskell
instance Scatterable Dielectric where
  scatter (Dielectric ir') ray (HitRecord p normal _ frontFace _) g =
    (Just (Scattered scattered attenuation), g')
    where
      (randFloat, g') = sampleFloat g
      scattered = Ray p refracted
      attenuation = Vec3 1 1 1
      refracted =
        if (let cosTheta = min (vDot (-unitDir) normal) 1
                sinTheta = sqrt $ 1 - cosTheta ^ 2
             in refractionRatio * sinTheta > 1)
          then vReflect unitDir normal
          else vRefract unitDir normal refractionRatio
        where
          unitDir = vUnit . direction $ ray
          refractionRatio = bool id recip frontFace ir'
```

유리는 아무것도 흡수하지 않기 때문에, `atteunation` 가 항상 1이다. 

![2](/assets/images/2021-05-29/c10/Untitled2.png)

## 슐릭 근사

현실의 유리는 각도에 따라 다양하게 반사된다 - 창문을 특정각도로 보면 거울이 되기도한다. 여기엔 아주 이상한 식이 숨어있다. 하지만 대부분은 'Christophe Schlick'이 만든 빠르고 정확한 다항 근사식을 쓴다. 

```haskell
instance Scatterable Dielectric where
  scatter (Dielectric ir') ray (HitRecord p normal _ frontFace _) g =
    (Just (Scattered scattered attenuation), g')
    where
      (randFloat, g') = sampleFloat g
      scattered = Ray p refracted
      attenuation = Vec3 1 1 1
      refracted =
        if (let cosTheta = min (vDot (-unitDir) normal) 1
                sinTheta = sqrt $ 1 - cosTheta ^ 2
             in refractionRatio * sinTheta > 1 ||
                reflectance cosTheta refractionRatio > randFloat)
          then vReflect unitDir normal
          else vRefract unitDir normal refractionRatio
        where
          unitDir = vUnit . direction $ ray
          refractionRatio = bool id recip frontFace ir'
          reflectance cosine refIdx =
            let r = (^ 2) $ (1 - refIdx) / (1 + refIdx)
             in r + (1 - r) * (1 - cosine) ^ 5
```