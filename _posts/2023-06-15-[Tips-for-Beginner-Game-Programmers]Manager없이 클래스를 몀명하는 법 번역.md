---
title: (Tips for Beginner Game Programmers) [번역] ‘Manager’없이 클래스를 명명하는 법
tags: Tips-for-Beginner-Game-Programmers translate
---

[원본](https://gist.github.com/hurricane-voronin/9ceccace0fd530bbf17c83b059c86eb7)을 읽고 한국어로 변역한 글입니다.

<!--more-->

---

## SomethingManager

`SomethingManager` 만큼 모호한 이름이 없습니다. **`Manager` 단어를 피하자.** ‘Alan Green’ 은 블로그에서 나쁜이름의 대안을 제안했습니다. 아티클을 읽으면 이름을 짓는 것에 대해 직관을 조금이나마 얻을겁니다. 객체와 클래스를 표현하는 직관적인 이름을 짓는것은 쉽지 않습니다. 그래서 Steve McConnell은 좋은 이름을 짓는 방법을 제안했습니다. 

1. **동작하는 모든 것을 서술해라**. 이름이 기가 찰 정도로 길더라도 루틴일 뿐이니 일단 무시하자.
2. **의미없거나, 모호하고, 선명하지 않은 동사를 피해라.** 예)`UrlManager`, `HandleOutput`, `PerformServices`. 명확히 무엇을 하는가? 간결한 답이 나오지 않는다면, 가능할 때까지 리팩토링해라.
3. **숫자를 피해라.** 팀원에 작성한 코드에서 `OutputUser1` , `OutputUser2` 가 있다면, 코딩을 할때 하늘의 도움이 필요할 것이다(의역, 전지전능한 신만 이해할 수 있다).
4. **필요한 만큼 이름을 길게 만들어라.** McConnell에 따르면, 최적의 길이는 0~15다; 기능이 복잡하고 길어질만 하다면, 이해가능 하도록 아주 길게 만들어도 된다.
5. **함수의 경우, 결과를 이름에 포함시켜봐라.** 간단한 법칙이다. 예를들어 `printer.IsReady(), pen.CurrentColor()` , 기타 등등
6. **반대적 표현을 정확히 써라.**
    - 모든 `Open`에는 반드시 `Close`가 있어야 한다.
    - `Insert <=> Delete`
    - `Start <=> Stop`
7. **공동 작업 중 Convention을 정해라.**

클래스와 변수의 이름을 재정의 하는 것은 가장 자주하는 리팩토링 방법입니다. [좋은 이름을 짓는건 어렵다](https://martinfowler.com/bliki/TwoHardThings.html), *하지만 어려워야 합니다*, 대단한 이름은 그저 한-두 단어로 본질을 내포하기 때문입니다.

작업을 다 끝내기 전까지 무슨 이름이어야 하는지 알기어렵습니다. 코드처럼, 절대 끝은 없습니다, 그래서 시간을 거듭할 수록 이름은 계속 바뀝니다. 하지만 이름에 따라 쓰레기코드/짜고싶은 코드로 결정되기도 합니다.

- (원래 자료: [*음 어떤매니저라고 불러야 하나?*](https://blog.codinghorror.com/i-shall-call-it-somethingmanager/))

## 디자인 패턴

디자인 패턴은 소프트웨어 설계 중 발생하는 문제를 해결하는 일반적인 방법입니다. 정답이 아닌 청사진으로, 코드 속 상황에 따라 가져와 커스터마이징 할 수 있습니다. 

- (원래 자료: [디자인 패턴](https://refactoring.guru/design-patterns), [위키피디아](https://en.wikipedia.org/wiki/Design_Patterns))

## 클래스 이름 제안 리스트

| Class 접미 | 설명  |
| --- | --- |
| Storage | 미래에 사용할 무언가를 저장합니다.  |
| Repository | https://martinfowler.com/eaaCatalog/repository.html는 데이터베이스 접근 코드를 분리하고, Scene뒤에서 동작을 수행합니다. 객체들을 캡슐화 및 분리하여 도메인 접근을 제한합니다.  |
| (Data)Mapper | Mapper는 객체와 데이터베이스의 독립성은 유지한채로 데이터를 옮깁니다. |
| (Data)Provider | 서비스와 데이터 소스를 분리하여 제공합니다. 일례로 드라이버가 하드웨어 디바이스를 추상화하는 것과 같습니다. |
| Reader | resource에서 데이터를 읽습니다. |
| Writer | resource에서 데이터를 씁니다. |
| Importer |  |
| Exporter |  |
| Builder(Creator) | Builder는 생성 디자인 패턴입니다. 같은 빌드 과정으로 타입과 표현이 다른 객체를 생산합니다. |
| Fetcher | 각기 다른 자원에서 데이터를 회수합니다. 예) rss |
| Splitter | 합성된 데이터를 분열된 데이터의 집합으로 만듭니다.  |
| Agregator | 분열된 데이터들을 합성 데이터 하나로 합칩니다. |
| Serializer | 데이터나 객체를 저장될 수 있는 형태(파일,메모리,네트워크)로 변환합니다. |
| Transformer | Transfomer는 한 속성만 수정합니다. 그래서 Basic Type으로 보면 결과는 같지만, 다른 capacity와 강도를 가집니다.  |
| Adaptor/Wrapper/Translator | 다른 클래스 객체가 사용할 수 있도록 클래스의 인터페이스를 구현합니다.  |
| Formatter | 데이터를 용도에 따라 전환합니다. 예) data ≤> json ≤> xml |
| Normalizer | 각기 다른 형태의 데이터를 공통 규격으로 일반화합니다.  |
| Converter | 기존 데이터와 다른 형태와 상태를 가진 대안 객체를 만듭니다. |
| (Event,Form)Handler | Handler의 주된 역할은 아래 것들을 처리하는 것 입니다: data(parsing, 검증, 저장 및 회수. state(상태 전환 action 처리).process(process를 가능한 방식으로 처리).>|
| Activator | Activate design pattern은 여러 객체가 실행할 수 있는 (on-demand) 서비스의 활성화를 제어합니다  |
| Checker |  |
| Validator | 자세 사항) https://jcp.org/en/jsr/detail?id=303 |
| Parser | Parser는 데이터의 구조를 분석하거나 변환할 때 사용합니다. 예) txt ≤> json ≤> xml |
| Matcher |  |
| Uploader |  |
| Copier |  |
| Anaylzer |  |
| Calculator |  |
| Refunder |  |
| Susbscriber |  |
| Unsubscriber |  |
| Navigator |  |
| Visitor | Visitor는 행동 디자인 패턴으로 객체의 제어를 visitor로 넘기면서, 알고리즘을 객체 구조에서 분리한다. |
| Authenticator | Authenticator는 서버와 클라이언트 간 ID 인증을 제공하고 구현합니다. |
| Initializer |  |
| Synchronizer |  |
| Encryptor |  |
| Decryptor |  |
| Iterator | Iterator는 행동 디자인 패턴으로 객체 집합을 lazy하게 접근합니다.  |
| Collection | 객체를 특정 규칙에 맞춰 저장합니다.  |
| Generator |  |
| Schedular |  |
| Runner | exception을 발생시킬 수 있는 Task를 수행합니다.  |
| Executor | 값을 반환하지 않고, exception을 발생시키지 않는 task를 수행합니다.  |
| Delegator | 멤버(property/method) 수행을 다른 객체(Receiver)에게 위임합니다. |
| Invoker |  |
| Logger | 자세 사항) http://www.php-fig.org/psr/psr-3/ / https://tools.ietf.org/html/rfc5424 |

[source-code-wordle.de](http://source-code-wordle.de/)을 보는 것 또한 추천드립니다. 많은 코드에서 단어를 추출한뒤 이름을 제안해줍니다. 구현하며 특정 이름과 서비스를 정할때 도움이 될수도 있습니다.

## 역자

저는 명명하는 것을 중요하게 생각합니다. 특히나 클래스의 이름은요. 코드를 읽을 때는 이름으로 기능을 유추하며, 구현할 때는 이름으로 기능을 제약/확장하기도 합니다. 프로젝트를 진행하며 모호한 이름을 가진 클래스는 본래의도를 벗어나 부가적인 코드(책임)이 생겨납니다. **이 문제를 방지하는 첫번째 보완방법이 저는 좋은 이름짓기라고 생각합니다.**

글을 쓰며, 첫번째 목표는 원본을 똑같이 옮기는 것이 었습니다. 여러 아티클을 읽으며, 좋은 이름 짓기는 많이 봐왔지만, 기능별 테이블까지 제공한 친절함(수고스러움)은 오랜만에 느꼈습니다. 저도 신입일때 아는 키워드의 pool자체가 작을 때 *‘좋은 이름은 도대체 뭔데?’* 라는 질문이 떠올랐기 떄문이겠죠. 그래서 이 시리즈를 진행하던 와중 변역할 필요성을 느꼈습니다. 아티클을 보고 들어왔던 신입분들은 테이블의 모르는 키워드만 찾아서 디자인 패턴을 공부하기만해도 많은 도움이 될거라 생각합니다. 

시리즈를 어느 정도 진행한 후 테이블의 빈공간과 저만의 추가내용을 보태려 합니다; *아쉽게도 테이블의 빈공간들이 많은 것은 원본을 그대로 가져왔기 때문입니다.*  

## 참고

- [https://gist.github.com/hurricane-voronin/9ceccace0fd530bbf17c83b059c86eb7#design-patterns](https://gist.github.com/hurricane-voronin/9ceccace0fd530bbf17c83b059c86eb7#design-patterns)
- [https://wiki.c2.com/?DontNameClassesObjectManagerHandlerOrData](https://wiki.c2.com/?DontNameClassesObjectManagerHandlerOrData)
- [https://blog.codinghorror.com/i-shall-call-it-somethingmanager/](https://blog.codinghorror.com/i-shall-call-it-somethingmanager/)
- [https://afsy.fr/avent/2019/04-ne-me-parlez-plus-de-manager](https://afsy.fr/avent/2019/04-ne-me-parlez-plus-de-manager)