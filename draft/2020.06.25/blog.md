## Symbol

 workspace에서 파일과 symbol을 넘나들며 이동한다. [VS Code Document](https://code.visualstudio.com/docs/editor/editingevolved)

### Quick File Navigation

 현재 workspace 내 파일 검색, 최신으로 검색한 순서로 리스트 정렬

> 단축키 `ctrl + p`

![go-to-file](rsc/go-to-file.png)

### Open symbol by name

 현재 workspace 내 symbol을 검색한다. `enter`시 `ctrl`를 눌러 `:Open to the Side`로 실행할 수 있다.

> 단축키 `ctrl + t`

![open-symbol](rsc/open-symbol.png)

### Go to Symbol

 현재 editor에서 열려있는 파일의 symbol을 `:peeking`하며 이동한다. class, variable, method 등등 다양한 symbol을 지원한다.

> 단축키 `ctrl + shift + o`

![go-to-symbol](rsc/go-to-symbol.png)

## Multicursor

 cursor를 여러개 만들어 동시에 입력한다. `: command palette > add cursor below/above` 또는 단축키로 사용할 수 있다.

> 단축키 `ctrl + alt + up_arrow/down_arrow`

![multi-cursor1](rsc/multicursor1.gif)
![multi-cursor2](rsc/multicursor2.gif)

## Search Editor

 `:Activity Bar`의 돋보기 아이콘을 통해 workspace내 검색을 하곤하는데, vs-code의 editor 특성상 검색 결과에 대해서 시각화가 잘 이루어지지 않았다.

 `:Open in editor` button으로 검색창을 editor에서 열수있다. 검색결과에서 열린 페이지 또한 editor에 속해있기 때문에 `: Find Reference`, `: Peek Reference` 등등 editor로서의 기본기능도 사용가능하다.

 ![search-editor-open](rsc/search_editor_open.png)

 아쉽게도 search result는 원래 코드와 분리되어 search editor에서 하나하나 수정하는 것은 일반적인 방식으론 불가능하다. 그래서 reference의 guider는 `[Extension]search-editor-apply-changes`를 추천해주었다.

## Hover Show

 editor에서 함수나 변수에 마우스를 올리면 관련 정보가 뜨곤한다. object위에 마우스를 잠깐 올리는 행위를 `: hover`라고 한다. 이때 `ctrl`을 누른채로 `: hover`를 하게될 경우 `: Show Definition Preview Hover`로 더 자세한 정보를 알 수 있다.

 아래는 `ctrl`을 누르지 않은 상태와 누른상태 비교다.

![hover-diff](rsc/hover.png)

 필자의 경우 `: [Setting]Preferences/Keyboard Shortcuts`에서 키보드로도 hover를 발동시킬 수 있도록 shortcut을 커스터마이징하였다.

![hover-custom setting](rsc/hoversetting.png)