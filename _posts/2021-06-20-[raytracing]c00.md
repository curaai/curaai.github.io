--- 
title: (RayTracing) Intro
tags: haskell graphics raytracing
mathjax: true
---

이 강의를 진행하는 피터 교수님은 수년동안 많은 그래픽스 강의를 해오셨습니다. 레이트레이싱(ray tracing)은 API 없이도 그래픽스 이미지를 만드는 것이 가능합니다. 강의은 빠르고 멋진 프로그램을 만드는 것에 중점을 둡니다.

 "레이 트레이싱"은 다양한 의미를 가집니다. 여기서 언급되는 것은 엄밀히 "패스 트레이서"입니다. 교수님는 C++로 개발하지만, 따라야할 필요는 없습니다. 하지만 빠르고, 간단하며, 대부분은 C++로 작성된 렌더러를 사용한다는 점에서 추천합니다. *저는 언어를 haskell로 개발하며, 언어를 익히기 위함을 목적으로 했습니다.* 

 이 강의는 벡터 연산(dot product, vector addition) 등에 익숙하다고 가정합니다. 만약 처음 접한다거나 공부가 필요하다면, Marschner님과 피터 교수님의 그래픽스 책이나 Foley, Van Dam, McGuire의 'Graphics Codex'를 참고하시길 바랍니다. 

 만약 문제를 맞닥드리거나, 누군가한테 보여줄 만큼 멋있게 만들었다면 부디 교수님께 이메일을 보내주세요, ptrshrl@gmail.com 

 책과 관련된 자료들도 블로그에서 참고가능합니다. [https://in1weekend.blogspot.com/](https://in1weekend.blogspot.com/)

## Follow Guide

튜토리얼을 번역하면서 불필요한 문장이나, 단어는 의역을 마음대로 진행했습니다. 보기 불편하시다면 원문을 보시는 것을 추천합니다. 또한 code example도 하스켈로 진행했습니다. 이후 추가적으로 코드 분석이나, 최적화, 테스트 케이스 등등 하스켈과 관련된 컨텐츠도 다룰 예정입니다. 

아래는 번역 스타일입니다.

- 기울임체(italic)로 된 문장은 *원글에 제가 덧붙인 글(부연설명 등등)입니다.*
- 굵음(bold)으로 된 문장 혹은 단어와 $equation$는 수식에 사용된 것을 글에서 표현할 때 사용합니다.
- "쌍따옴표"로 묶인 단어는 강조시에 사용합니다.

> 인용구에 적힌 문장은 중요 단어를 정의시에 사용합니다.

`word` 는 주로 코드에 사용된 것을 가르킵니다.