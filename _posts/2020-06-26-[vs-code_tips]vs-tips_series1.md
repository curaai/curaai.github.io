---
title: (VS-Code Tips) 1. VS-Code tips Series 1
tags: vs-code-tips
mathjax: true
---

 VS-Code를 사용하면서 얻은 꿀팁들을 공유합니다. 스스로 혹은 친구, posting, youtube 등등에서 얻은 것은 출처를 밝힙니다.

 editor마다 setting이 다르고 shorcut 또한 다르기에 제시한 shortcut이 작동하지 않은다면, `User Setting`/`Keyboard Shortcuts`에서 tip name을 찾아 shortcut을 변경하거나 찾길 바랍니다.

 본 시리즈는 VS-Code 1.46.1 version을 기준으로 작성되었으며, 다른 버전에서는 shorcut이 다르거나 없음이 있을 수 있습니다.

<!--more-->
---

## Symbol

 workspace에서 파일과 symbol을 넘나들며 이동한다. [VS Code Document](https://code.visualstudio.com/docs/editor/editingevolved)

### Quick File Navigation

 현재 workspace 내 파일 검색, 최신으로 검색한 순서로 리스트 정렬

> 단축키 `ctrl + p`

![go-to-file](/assets/images/2020-06-26/go-to-file.png)

### Open symbol by name

 현재 workspace 내 symbol을 검색한다. `enter`시 `ctrl`를 눌러 `:Open to the Side`로 실행할 수 있다.

> 단축키 `ctrl + t`

![open-symbol](/assets/images/2020-06-26/open-symbol.png)

### Go to Symbol

 현재 editor에서 열려있는 파일의 symbol을 `:peeking`하며 이동한다. class, variable, method 등등 다양한 symbol을 지원한다.

> 단축키 `ctrl + shift + o`

![go-to-symbol](/assets/images/2020-06-26/go-to-symbol.png)

## Multicursor

 cursor를 여러개 만들어 동시에 입력한다. `: command palette > add cursor below/above` 또는 단축키로 사용할 수 있다.

> 단축키 `ctrl + alt + up_arrow/down_arrow`

![multi-cursor1](/assets/images/2020-06-26/multicursor1.gif)
![multi-cursor2](/assets/images/2020-06-26/multicursor2.gif)

## Search Editor

 `:Activity Bar`의 돋보기 아이콘을 통해 workspace내 검색을 하곤하는데, vs-code의 editor 특성상 검색 결과에 대해서 시각화가 잘 이루어지지 않았다.

 `:Open in editor` button으로 검색창을 editor에서 열수있다. 검색결과에서 열린 페이지 또한 editor에 속해있기 때문에 `: Find Reference`, `: Peek Reference` 등등 editor로서의 기본기능도 사용가능하다.

 ![search-editor-open](/assets/images/2020-06-26/search_editor_open.png)

 아쉽게도 search result는 원래 코드와 분리되어 search editor에서 하나하나 수정하는 것은 일반적인 방식으론 불가능하다. 그래서 reference의 guider는 `[Extension]search-editor-apply-changes`를 추천해주었다.

## Hover Show

 editor에서 함수나 변수에 마우스를 올리면 관련 정보가 뜨곤한다. object위에 마우스를 잠깐 올리는 행위를 `: hover`라고 한다. 이때 `ctrl`을 누른채로 `: hover`를 하게될 경우 `: Show Definition Preview Hover`로 더 자세한 정보를 알 수 있다.

 아래는 `ctrl`을 누르지 않은 상태와 누른상태 비교다.

![hover-diff](/assets/images/2020-06-26/hover.png)

 필자의 경우 `: [Setting]Preferences/Keyboard Shortcuts`에서 키보드로도 hover를 발동시킬 수 있도록 shortcut을 커스터마이징하였다.

![hover-custom setting](/assets/images/2020-06-26/hoversetting.png)

## Smart Selection

 일반적인 에디터에서 cursor가 선택하고 싶은 영역 혹은 단어의 중간에 있을 경우, `ctrl`로 선택하고싶은 영역의 처음으로 가서 `shift`를 눌러 selection을 하곤한다.

 `: Smart Selection`은 현재 커서를 기준으로 선택영역을 *smart*하게 늘린다. `right/left_arrow`로 움직이는데, 확장/축소의 개념이라 `left_arrow`로 selection이 되지 않는다.

> string 단어 중간 > 단어 > 괄호 안 > 괄호 포함 > line 처음-끝 > line 전체 > struct 중괄호 안 > struct 중괄호 포함 > 문단 -> 파일 전체
 ![smart-selection](/assets/images/2020-06-26/smart_selection.gif)


> Expand Selection: `shift + alt + left_arrow` <br>
> Shrink Selection: `shift + alt + right_arorw`

## Reference

 본글은 아래 링크를 참고하여 재정리한 글 임을 밝힙니다.

- [Microsoft Build 2020 / Visual Studio Code tips and tricks by Matt Bierner](https://channel9.msdn.com/Events/Build/2020/BOD103?ocid=AID3012654&WT.mc_id=build2020-azuredevtips-micrum)