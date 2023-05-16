---
title: (Tips for Beginner Game Programmers) +/- 프로그래밍 법칙
tags: Tips for Beginner Game Programmers
---

 학원 및 학부를 나와 취업을 했지만, 신입 게임 개발 프로그래머로서 실수하고 있는 사람과 스스로를 위해 쓰는 글입니다. 

<!--more-->

---

## 서문

게임 프로그래밍은 다른 분야에 비해 생성/제거하는 객체양과 각각의 상호작용이 정말정말 많다. 이때 신입 게임 클라이언트 프로그래머가 놓칠만한 점을 짚어보겠다. 이때까지 책을 읽고 공부한 것을 스스로 정의한 법칙이라 당연히 이미 알수도 있고(개발서적을 많이본 독자라면), 이미 숨쉬듯이 하고 있었지만, 글 읽고 인지할 수도 있다. 

## 법칙

| +/- 프로그래밍 법칙은 아주 사소하면서 단순하다. **“+코드를 적는다면, 반드시 - 코드를 구현해라.”** 

+코드는 아주아주 추상적이기에 독자가 의미를 부여하기 나름이다. 먼저 아래 예시를 들어본다면; 

- 크게는 객체의 생성과 제거
- 클래스속 생성자와 소멸자 (Unity의 Awake/OnDestroy, OnEnable/OnDisable)
- 리스트 요소 추가/제거(리스트 초기화)
- 클래스 멤버(`int entityId`, `bool invisible`, `float velocity` , 등등)

## 예시

거창하게 법칙이라고 했지만, 위 예시를 보면 아주아주 당연한 얘기다. 하지만 순간의 안일함(귀찮음)과 실수로 버그를 만들 수 있다. 위 사례를 가지고 몇가지 예시를 들어보겠다. 

유저의 유효한 입력이 들어왔을 때, `SkillHolder` 가 skill을 실행하도록 `SkillAreaInputReceiver`가 callback를 설정하는 코드다. `onSuccessToExecuteCb` 는 콜백이 초기화 되지 않아, action을 호출할 때 cost가 콜백의 개수만큼 더 소모되었다. 

```jsx
public void RequestToExecute(UnityAction onSuccessToExecute)
{
    skill.Prepare();
    onSuccessToExecuteCb += onSuccessToExecute;
}
```

유닛이 어떤 상황으로 인해 invisible=true가 인채 공격을 받아 InActive상태가 되었다. 원래는 전혀 문제 없는 상황이지만, 최적화를 위해 ObjectPool을 사용할 떄 문제가 된다. OnDisable에서 Unit의 상태를 초기화 하지 않은 상태로 Pool에서 꺼내졌을 때 invisible한 상태로 spawn된다. 이렇게 생성된 유닛은 로그상 에러가 보이지 않지만(발생할 수도 있음) 문제를 내포한 상태다. 

## 마무리 

새로운 코드를 추가할때 이를 명심하면, 코드 작성 중 발생할 수 있는 버그를 줄일 수 있다. 일단은 2가지 예시를 들었지만, 객체가 많고 서로간 참조, 접근이 많으면 많을 수록 처리되지 않은 상태에 대해 연계되는 버그를 수많이 파생시킬 수 있으니 조심하자.
