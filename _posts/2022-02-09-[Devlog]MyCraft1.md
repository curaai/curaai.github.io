---
title: (MyCraft Devlog) 1. MineCraft 
tags: devlog unity
---

<!--more-->

 들어가기에 앞서 이글은 개발과정 중 배운 것과 프로그래밍하면서 생긴 고찰들을 적었습니다. MineCraft를 클론코딩하는 이 프로젝트는 [Code a Game Like Minecraft in Unity](https://youtube.com/playlist?list=PLVsTSlfj0qsWEJ-5eMtXsYp03Y9yF1dEn)의 튜토리얼을 많이 따라하였습니다. 

[Github Repository](https://github.com/curaai/MyCraft)의 첫번째 마일스톤을 끝내고 쓰는 데브로그입니다. 

## 개발

 개발한 것 자체를 간단히 요약해서 정리합니다. 

- `Chunk` : n * n * height개의 `Block`을 가진 월드의 일부입니다. 블록은 **월드가 아닌 청크에서 관리/렌더링**됩니다. 청크는 바깥 쪽 만의 블록만 별도로 렌더링하여 최적화 했습니다.
- `World` : 여러개의 청크를 들고 있으며, 플레이어/청크 시스템이 Block의 생성/확인 요청을 받아들입니다.
- `Player` : 카메라와 입력을 받는 객체입니다.

## 고민 & 고려

 개발 과정 중에서 특히 신경이 쓰였던 점을 작성합니다. 
- 원래 튜토리얼에서는 `byte World.GetBlock(Vector3)` 로 블럭의 타입을 단순하게 처리했습니다.
    - byte 대신에 Block 클래스
    - 확장된 기능의 여러 블록 추가로 생겼을 때 고려
    - 텍스쳐 로딩의 기능을 Block 스스로가 책임을 가지고 값을 반환
- `AdjacentChunkRenderComponent` & `MoveControlComponent`
    - `AdjacentChunkRenderComponent` : 플레이어 주위의 N개의 청크만 렌더링합니다.
    - `MoveControlComponent` : 입력으로 플레이어를 컨트롤합니다.
        - 추후에 동물 AI, 좀비 등의 Entity가 만들어졌을 때는 기능을 더 잘게 쪼개고 추상화시켜서 컴포넌트를 추가할 것 입니다.
    - 기능을 컴포넌트로 분리하고 여러 기능들의 조합이 플레이어를 만들도록 고려했습니다.
- 개발 전 작업할 내용을 간단히 Issue로서 정의하고, Epic/Milestone을 사용하여 프로젝트의 완성도와 안정성을 높였습니다.

## 새로 배운 내용

- `MeshRenderer` , `MeshFilter`: Graphics Engine에서 사용하는 Verticles, Triangles, UV를 구성하고 렌더링합니다.
- `MoveControlComponent`
    - 일정 속도로 가속도가 붙어 추락하는 기능을 사용하기 위해 Update 대신에 FixedUpdate를 사용하여 구현했습니다.
    - 유니티 / ProjectSetting / Input에서 name과 버튼을 별도로 설정하여 Input클래스에서 사용합니다.
- 단순한 버전의 Noise 알고리즘으로 무작위 지형을 구성하고 임계값을 여러개 설정하여 다른 블록을 만들었습니다.

## 회고

 유튜브를 보면서 클론코딩을 하고있지만, 따라하지 않고 자신의 것으로 만들려고 노력했다. 그래서 코드로도 작성을 하지만 문서를 쓰며 자신의 것으로 덮어씌우려고 했다. 튜토리얼에 나왔던 간단한 내용도 다르게 구조를 잡거나 설계했다. 클래스/변수/함수의 이름도 신경써서 정의했다. 

 작업내용을 정의하고 따라하는게 쉽지 않았다. 유니티의 경험이 많지 않아서 만들어야할 목표는 있지만 어떤식으로 구현을 하고 적용을 할지 머리속에 잘 떠오르지 않았다. 더군다나 그것을 Issue로서 미리 정의를 하고 추후의 기능을 고려하기까지 했어야하니 어려웠다. 

 지금 당장의 문제를 풀려고 하기보다, 미래의 문제 나중에 만들 기능(확장성)을 너무 고려하다보니 이번 마일스톤에서는 진도가 늦게나갔다. 다음 마일스톤에는 이 점을 확실히 줄여야겠다.
