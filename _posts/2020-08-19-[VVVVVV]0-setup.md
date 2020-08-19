---
title: (VVVVVV) 0. Setup
tags: project-review game
mathjax: true
---

 [VVVVVV](https://ko.wikipedia.org/wiki/VVVVVV)라는 게임을 아이워너비더보시(IWBTB)에서 스테이지로 나와서 기억을 하고 있었는데, 예전에 github dashboard에서 소스코드가 무료로 공개된 것을 봤었다. 그래서 애착이 가는 인디게임이기도 하니 프로젝트를 리뷰하면서 리팩토링, 기능추가 등등을 여러 지식을 사용하여 진행할 것 이다.

<!--more-->
---

 포스트의 setup은 Windows 10 64bit, VS 2015 compiler, Visual Studio code cmake, VVVVVV desktop version을 기준으로 진행됩니다.  

 소스코드가 공개된 원본링크 [original VVVVVV](https://github.com/TerryCavanagh/VVVVVV)에서 clone 혹은 fork, [SDL2](https://www.libsdl.org/download-2.0.php), [SDL2_mixer](https://www.libsdl.org/projects/SDL_mixer/) development package, [resource](https://thelettervsixtim.es/makeandplay/)파일 전부 다운받아 아래 tree와 같이 세팅한다.

```bash
.
├── data.zip
├── desktop_version
│   └── src
├── third_party
│   ├── SDL2-2.0.12
│   │   ├── docs
│   │   ├── include
│   │   └── lib
│   │       ├── x64
│   │       └── x86
│   ├── SDL2_mixer-2.0.4
│   │   ├── include
│   │   └── lib
│   │       ├── x64
│   │       └── x86
│   ├── lodepng
│   ├── physfs
│   ├── tinyxml2
│   └── utfcpp
│       └── source
│           └── utf8
└── tools
```

 `SDL2`을 제외한 라이브러리는 빌드시 static으로 다 빌드하게 세팅이 되어있으니 신경쓰지 않아도 된다.

## Make Runnable

 desktop_version을 열어 cmake 설정을 한다.  

 kit을 선택하고 SDL에 사용될 include, libraries를 cmake argument로 전달한다.

![select_kit](/assets/images/2020-08-19/select_kit.png)

 cmake argument는 아래 이미지와 option을 참고해 자신의 절대경로에 맞춰 설정하면 된다.
![cmake_setting](/assets/images/2020-08-19/cmake_setting.png)

```bash
-DSDL2_INCLUDE_DIRS=C:/VVVVVV/third_party/SDL2-2.0.12/include;C:/VVVVVV/third_party/SDL2_mixer-2.0.4/include
-DSDL2_LIBRARIES=C:/VVVVVV/third_party/SDL2-2.0.12/lib/x86/SDL2.lib;C:/VVVVVV/third_party/SDL2-2.0.12/lib/x86/SDL2main.lib;C:/VVVVVV/third_party/SDL2_mixer-2.0.4/lib/x86/SDL2_mixer.lib
```

 빌드를 진행한 후에 exe옆에 data.zip을 배치하면 성공적으로 프로그램이 실행된다.

![VVVVVV](/assets/images/2020-08-19/VVVVVV.png)