--- 
title: (RayTracing) 구 그리기
tags: haskell
mathjax: true
---

레이트레이서에 이제 물체 하나를 추가해봅시다. 광선이 구에 부딪혀 다양한 각도로 반사되기 때문에 레이트레이싱 예제에 적합합니다.

## 광선-구 교점

반지름이 R이고 원점이 중심인 구의 방정식은 $x^2 + y^2 + z^2 = R^2$입니다. 바꿔말해서 

- 점 $(x, y, z)$가 구 위: $x^2 + y^2 + z^2 = R^2$
- 점 $(x, y, z)$가 구 안: $x^2 + y^2 + z^2 < R^2$
- 점 $(x, y, z)$가 구 밖: $x^2 + y^2 + z^2 > R^2$

구의 중심이 $(C_x, C_y, C_z)$ 일때 방정식:

$$(x - C_x)^2 + (y - C_y)^2 + (z - C_z)^2 = r^2$$

그래픽스에서는 모든 벡터연산을 `vec3` 클래스에서 처리합니다. 점 $P = (x, y, z)$에서 중심 $C = (C_x, C_y, C_z)$ 까지의 거리는 $(P - C)$입니다. 

$$(P-C) \cdot (P-C) = (x - C_x)^2 + (y - C_y)^2 + (z - C_z)^2$$

구의 방정식을 벡터형식으로: 

$$(P-C) \cdot (P-C) = r^2$$

저희는 이걸로 "어느 점 **P**가 방정식을 만족한다면 구위에 있다"라는걸 알수있습니다. 광선 $P(t) = A + tB$가 구와 부딪치는지 알고 싶습니다. 만약 구에 부딪히면, 구의 방정식인 $P(t)$를 만족하는 t가 있는 것입니다.

$$(P(t) - C) \cdot (P(t) - C) = r^2$$

혹은 광선 P(t)를 풀수있습니다:

$$(A + tB - C) \cdot (A + tB - C) = r^2$$

위 식을 아래와 같이 전개할 수 있습니다: 

$$t^2B \cdot B + 2tB \cdot (A - C) + (A - C) \cdot (A - C) -r^2 = 0$$

벡터와 **r**은 모두 상수이며 알수있습니다. 모르는 값은 **t**이며, 이차방정식으로 고등학교 수학시간에 아마 이 방정식을 봤을겁니다. t를 풀어야하며, 제곱근의 값이 양수(해가 2개), 음수(해가 없음), 0(해가 하나)로 알수있습니다. 그래픽스에선 기하학과 아주 직접적으로 관계가 있습니다. 

![0](/assets/images/2021-06-20/c4/Untitled0.png)

## 첫 Raytraced 이미지 만들어보기

작은 구를 (0, 0, -1)에 배치한후, 구가 부딪혔을 때 빨간색을 반환하도록 했습니다.

```haskell
ray2color :: Ray -> Vec3 Float
ray2color r =
  if hitSphere (Vec3 0 0 (-1)) 0.5 r
    then Vec3 1 0 0
    else let t = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
          in pure (1 - t) + pure t * Vec3 0.5 0.7 1.0
  where
    hitSphere ctr radius ray = (b * b - 4 * a * c) > 0
      where
        oc = origin ray - ctr
        a = vDot (direction ray) (direction ray)
        b = vDot oc (direction ray) * 2
        c = vDot oc oc - radius ^ 2
```

광선에서 색으로 변환하는 코드를 조금 수정했습니다.

![1](/assets/images/2021-06-20/c4/Untitled1.png)