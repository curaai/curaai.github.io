---
title: (CMake 튜토리얼) 0. VisualStudio Code cmake 세팅
tags: cmake visual-studio-code
---

 이글은 필자가 Mac OS X를 사용하며 cpp를 하고싶어 vs-code에 cmake를 사용하여 프로젝트에서 사용한 tech 기록을 목적으로 하고있습니다. 

 이글을 쓰면서 처음 해보는 것이기 때문에 미숙한 점은 양해바라며, 피드백은 **적극** 환영입니다.
<!--more-->

---

 맥유저라면 당연히 XCode는 깔려있을테니 XCode를 설치하여, clang을 활성화한다. 개발자라면 당연히 VisualStudio Code는 깔려있겠지... 

## Prepare

 extension tab에서 C/C++, CMake Tools를 다운로드 받습니다.
 ![img2](/assets/images/2020-02-16/img2.png)

## CMake Code 작성


```cmake
CMAKE_MINIMUM_REQUIRED (VERSION 3.15 )

# project setting
SET ( PROJ_NAME cmake-test )
PROJECT ( ${PROJ_NAME} )
SET( EXECUTABLE_OUTPUT_PATH ${CMAKE_SOURCE_DIR} )

# C++ settings
SET(CMAKE_CXX_STANDARD 11)

# set source and headers
SET (SRC_FILES
    src/main.cpp
)

# build 
ADD_EXECUTABLE( ${PROJ_NAME} ${SRC_FILES} )
```

## CMake 함수 사용법

#### 1: CMAKE_MINIMUM_REQUIRED( <version> )
 cmake에서 사용되는 최소 버전을 명시한다. version에 따라 사용되는 문법이 달라질수 있기 때문에 당연지사.

#### 4: SET( <변수이름> <값> )
 cmake file 내에서 사용되는 변수를 지정할 수 있다. 예제같은 경우는 PROJ_NAME란 이름의 변수 값을 `cmake-test`로 지정하였다.

 사용법은 `${변수이름}`으로 사용하면된다.

#### 5: PROJECT( <프로젝트이름> )
 프#로젝트의 이름을 괄호 안의 값으로 설정한다. 이때 위에서 할당한 변수를 사용하여 PROJECT 이름을 설정하였다. 

#### 6. SET( <prefix_variable> <값> )
 위의 경우에선 SET으로 변수를 만들어서 값을 할당한 경우지만, CMake 내부에서 기본적으로 정의된 값이 있다. 빌드된 파일이 어디로 추출될 것인지 설정하는 변수다.

 위의 경우에는 EXECUTABLE_EXPORT_PATH, CMAKE_SOURCE_DIR 이라는 변수가 2개 있는 것이다.

 이 포스트의 예정되로라면 build라는 폴더가 생기며, 거기안에 실행파일이 생기는데 프로젝트 바로 아래 생기도록 변수를 설정해주었다.

#### 9: CMAKE_CXX_STANDARD
 척보면 알겠지만 C++11을 사용한다고 지정하였다.

#### 12: SET( <변수이름> <가변인자> )
 set을 `whitespace`으로 구분되는 가변 인자를 사용할수 있다.

 추후에는 SRC_FILES에 여러 파일들을 추가할 수 있을 것.

#### 17: ADD_EXECUTABLE( <실행파일명> <소스파일> )
 최종적으로 빌드할 파일들을 모아 실행파일로 만든다. 일반적으로는 프로젝트 명을 실행 파일명으로서 사용한다.

## 빌드 & 디버깅 & 실행

 위 CMake파일을 base로한 코드와 파일 구조는 다음과 같다.
```bash
.
├── CMakeLists.txt
├── build
└── src
    └── main.cpp
```

```cpp
#include <iostream>
using namespace std;

int main(int argc, char* argv[])
{
    cout << "Hello World!" << endl;
    return 0;
}
```

 여기서 vs-code `Command Palette`를 열면 `CMake: build` 항목이 보일 것 이다.

 엔터를 누르고 결과를 확인하면
 CMakeTest라는 폴더 밑에 생긴것을 확인할 수 있다.

```bash
[main] 폴더를 빌드하는 중: CMakeTest 
[build] 빌드를 시작하는 중
[proc] 명령 실행 중: /usr/local/bin/cmake --build /Users/user/workspace/proj/CMakeTest/build --config Debug --target all -- -j 10
[build] -- Configuring done
[build] -- Generating done
[build] -- Build files have been written to: /Users/user/workspace/proj/CMakeTest/build
[build] [ 50%] Linking CXX executable ../cmake-test
[build] [100%] Built target cmake-test
[build] 빌드가 완료됨(종료 코드: 0)
```
 
 11번째 라인을 "program": "${workspaceFolder}/cmake-test"과 같이 실행파일 이름으로 바꾼다.

![img1](/assets/images/2020-02-16/img1.png)


 `F5`를 눌러 Debug를 눌러 실행파일을 설정해주면, 성공적으로 간단한? cpp file을 cmake를 통해 visual-studio code에서 실행해볼 수 있다.
