---
title: (MyCraft Devlog) 2. Assets, Create&Destory Blocks
tags: devlog unity
---

 들어가기에 앞서 이글은 개발과정 중 배운 것과 프로그래밍하면서 생긴 고찰들을 적었습니다. MineCraft를 클론코딩하는 이 프로젝트는 [Code a Game Like Minecraft in Unity](https://youtube.com/playlist?list=PLVsTSlfj0qsWEJ-5eMtXsYp03Y9yF1dEn)의 튜토리얼을 많이 따라하였습니다. 

[Github Repository](https://github.com/curaai/MyCraft)의 두번째 마일스톤을 끝내고 쓰는 데브로그입니다.

 구현된 기능은 [Youtube](https://www.youtube.com/watch?v=9l0zOXfSmB0)에서 확인할 수 있습니다.

<!--more-->

## 배운점

### 유니티 essential

gameObject의 여러 Component는 Inspector의 순서와 상관이 없다.

- 당연히 Insepctor의 순서에 따라 Component의 Start가 정해지는 줄 알았다.
- Inspector Order 상 상위의 Cmpt[A]를 후위의 Cmpt[B]가 참조하는 식으로 코딩했다가 에러가 계속 나서 Debug해보니 A의 property가 null이었다.
- A의 property를 B의 getter property로 계속 참조하게 해놨는데, operation level에서 비효율적이지 않나? 라는 아쉬움을 남겨뒀다.

### 코드관리

namespace 관리

- 게임 특성상 여러 컴포넌트와 클래스가 난무할때 namespace로 중복된 이름, 명시를 확실히 할 수 있다. [commit](https://github.com/curaai/MyCraft/commit/c5708692fc37ff9cccde01b054e43a668c1978b6)
- 단순한 이름보다 (Helper, Holder 등등) Responsibilty를 스스로 가지지 않는 클래스의 이름을 변경한다.  [commit](https://github.com/curaai/MyCraft/commit/c5708692fc37ff9cccde01b054e43a668c1978b6)

특정 에디터와 extension에서만 동작하는 csharp의 region은 불필요하다. 코드 읽기의 흐름을 방해한다. [lines](https://github.com/curaai/MyCraft/commit/24bfcf907211b3162b3f1855cbab9e51811ad846#diff-ecbd766312cfdf7b58c0c66cda5e1d39d6657d4fceec5edca0d0230e26912977L7-L33)

- 특정 파일에 많은 기능이 몰릴 경우 그때에만 사용하는 것이 좋다.
- region을 사용하는 대신에 비슷한 변수명을 그룹화해서 라인단위로 정리한다.
- [CSharp Unity Convention](https://www.gamedeveloper.com/disciplines/coding-guideline-for-unity-c-) 에서 변수/함수 순서를 되도록 따라 가독성을 높였다.

CSharp 잘 활용하기 [commit](https://github.com/curaai/MyCraft/commit/24bfcf907211b3162b3f1855cbab9e51811ad846)

- in: 변하지 않는 변수로서 참조만 될때는 신경써서 사용한다.
- Expression-bodied: 간단한 함수 & get property는 한줄로 묶는 대신 변수명이 충분히 표현되도록 바꾼다.
- 간단한 Warning 바로바로 수정했다. 다소 시간이 필요한 작업은 바로 처리하지 않지만 가급적으로 작업속도의 영향을 주기전에 빠르게 처리하도록 명심했다.
- [SerializeField]는 바로바로 붙이자, 해당 변수가 언제 할당이되고 시작되는지 알아보기 어렵다.

---

## 반성

무분별한 Github Issue 생성 및 Task 관리. Task를 생성하고 관리하는 시도 자체는 좋았으나 issue를 무분별하게 너무 많이 만들어서 Task tracking이 오히려 더 불편(난잡)해졌다. Github/Zenhub를 사용하긴 하되, Epic과 Story를 적절히 나눠서 사용하자. 

---

## 고려한점

프로그램의 특성상 헷갈릴 수 있는 난해한 이름을 조정했다. Block, Cube, Voxel 등 한번 더 난잡하게 쓰이며, 함수밖에서 인자값의 변수명/메소드명에서 헷갈릴 수 있던 것을 정렬했다. [commit](https://github.com/curaai/MyCraft/commit/24bfcf907211b3162b3f1855cbab9e51811ad846)

- Voxel: 3D상의 좌표계를 나타낼때 사용한다.
- Block: 월드상에서 Voxel의 기능을 수행하거나 블록의 ID로서 인식할때 사용한다.
- Cube: 지금 당장은 사용하지 않으나 Item 레벨에서 사용될 여지가 있다.

해당 내용을 문서로는 어떻게 남겨야 할지 고민이다. 문서없이 코드만 봤을 때 바로 읽히게 하고 싶다. 

Gitmoji을 그만 사용했다. 혼자하는 project라 gitmoji를 사용했었으나, 다른 사람과의 협엽이 진행되거나 코드 리뷰를 할경우 리딩을 방해하고 난해해지기 때문에 과감히 버렸다. *혼자서 사용했던 이유는 주로 쓰던 gitmoji들은 의미를 함축적으로 사용하고 있어 commit message의 detail을 집중할 수 있다고 생각했기 때문이다.* 

최적화를 위해 Vector3, Vector3Int 고르게 사용했다. World, Chunk에 voxel이 있는지 검사를 특히 많이하는데 Vector3Int를 대신 사용하려 하고, Vector3Int에서만 돌아가는 경우는 Vector3 오버로딩을 지원하는 식으로 했다. 너무 많이 오버로딩을 하기보다 자주사용하는 곳만 오버로딩을 하고 나머지는 명시적으로 Vector3Int만 남겨둬 코드레벨에서 인지하도록 했다. [commit](https://github.com/curaai/MyCraft/commit/4e3e0898b57dc1091726e2ea301a92ecebc9264c)

에셋을 직접만들기보다 minecraft에서 사용되는 texture와 model을 최대한 사용하려고 했다.