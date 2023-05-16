---
title: (Tips for Beginner Game Programmers) 접근 한정자
tags: Tips-for-Beginner-Game-Programmers
---

 접근 한정자의 의미를 이해하고, 안전함을 제공하는 방법을 알려주는 글입니다. 

<!--more-->

---

## 서문

게임의 코어부터 시작해서 특별한 기능을 구현할 때는 새로운 클래스와 인터페이스를 만들기보다, 기존 클래스를 재활용(상속)하는 일이 빈번다. 이는 기존 개체간의 시스템 코어 로직의 변경과 버그 가능성을 줄이면서 빠르게 기능을 구현할 수 있기 때문이다. 이때 `private` , `protected` , `virtual` , `abstract` 를 무시한 채 ‘*기능을 구현한다*’라는 단순한 사고로 작업을 시작한다면, 개발자가 아닌 버그 자판기가 된다.

## 제약과 흐름 통제

객체의 역할을 이해하고 접근 제한을 통해 안전한 프로그래밍을 하고 있는 동료는 이 접근 한정자를 굳이 붙여가면서 당신의 실수를 저지한다. `private`, `protected` 와 같은 접근 한정자의 역할은 단순히 부모 클래스의 멤버 소유가 다가 아니다. `protected` 로 된 멤버의 상태 흐름 조절 역할을 포함하고 있다. 이는 이전 포스팅인 [+/- 법칙](https://blog.curaai.dev/2023/05/08/Tips-for-Beginner-Game-Programmers-+-%ED%94%84%EB%A1%9C%EA%B7%B8%EB%9E%98%EB%B0%8D-%EB%B2%95%EC%B9%99.html)과도 이어지는데 부모 클래스의 멤버는 부모 클래스에서 +/- 가 통제되며, 이를 상속받은 자식 클래스에서 강제로 변경하면 안된다.

## 예시

단편적인 예로 A 컴포넌트의 기능 중 하나가 특정 위치까지 도달한다고 해보자. 아래에서 `DestinationPos` 멤버의 접근은 `get` , 쓰기는 `private` 으로 제한되어 있으며 시작과 종료시에 null로 초기화하고 있다. 시작과 종료시에 굳이 초기화를 하는 이유 중 하나는 도착위치가 null에서 시작돼 A 클래스 안에서 값이 확인 및 재설정되며 흐름이 조절된다.

```csharp
class A : MonoBehaviour
{
	public Vector3? DestinationPos {get; private set;} 
	public UnityAction OnDestinationSetCb = null;

	private void OnEnable()
	{
	    DestinationPos = null;
	}

	private void Update()
	{
		if(DestinationPos.HasValue == false && ConditionA)
		{
			DestinationPos = new Vector(x, y, z);
			OnDestinationCb?.Invoke();
			SomethingElseFunction();
		}
	}
	
	private void OnDisable()
	{
	    DestinationPos = null;
	}
}
```

위 기능을 무시한채 `private set` property를 `protected set` 으로 바꾼후 자식 클래스에서 `base.OnEnable(); DestinationPos = new Vector(x, y, z);` 강제로 변경할 경우 A 내 null에서 값을 할당하고 콜백/추가함수 실행 흐름이 A클래스를 설계한대로 흘러가지 않아 버그를 맞닥드릴 수도 있다. 

## 마무리

객체의 역할에 따라 ‘*이 멤버는 여기서 조절되어야 해*’, ‘*저 멤버는 상속에서 건드려 질수도 있어*’를 설계시 신경써보자. 신경쓴 코드를 마주할 때 ‘*어 이거 바꿔서 쓰면 되겠다*’를 한번더 생각해보자. 몇글자의 키워드 만으로 앞으로 마주하게 될 몇 버그들을 미연에 방지할 수도 있으니 말이다.