---
title: (CMake 튜토리얼) 2. GTest 사용하기
tags: cmake google-test
---

 GTest를 사용하며 프로젝트 구조를 변경합니다. 변경된 구조를 가지고 VS Code에서 테스트 검증 및 빌드를 하는 방법을 서술하였습니다.
<!--more-->

---
 
 CMake를 사용하여 `gtest(google-test)`를 설치 및 사용하여 TDD 개발환경을 구축한다.

## 프로젝트 구조 변경

 이전까지의 예제를 진행하였다면, 아래와 같이 프로젝트가 이루어져 있을 것 이다. 현재 CMakeLists.txt는 프로젝트에 대한 cmake setting이 있는 것 뿐만 아니라 src를 빌드하는 script 또한 포함되어 있다.

```bash
.
├── CMakeLists.txt
├── build
└── src
    └── main.cpp
```
 
 이를 src 밑에 CMakeLists.txt를 추가하여 executable CMakeLists.txt와 프로젝트 cmake, 테스트 cmake를 분리한다. 예제같은 경우 편하게 필자의 닉네임을 사용하였다. 최종적으로 나오게 되는 output은 `curaai_run/curaai_tst/libcuraai_lib.a`이다.

```bash 
.
├── CMakeLists.txt
├── curaai_run
├── curaai_tst
├── libcuraai_lib.a
├── README.md
├── build
├── extern
│   └── googletest
├── src
│   ├── CMakeLists.txt
│   ├── test.cpp
│   ├── main.cpp
└── tests
    ├── CMakeLists.txt
    ├── main.cpp
    └── testa.cpp
```

 포인트는 **프로젝트 구조 변경**이다. `extern/tests/src/build`로 폴더를 분리하여, test 코드는 `tests` 안에, third_party로 사용하는 파일들은 `extern`, build 관련 파일은 `build`, 최종적으로 나오는 output은 `cmake source dir`로 제역할에 따른 곳에 위치하며, 이와 맞춰 CMakeLists.txt도 organize한다.

## GTest 설치
 
 의외로 GTest를 설치하는 것은 하나도 어렵지 않았더라...

 `git clone https://github.com/google/googletest/ extern/googletest`


## 프로젝트 cmake 설정

 이전에는 하나의 cmake 파일을 가지고 빌드를 했지만 이제는 계층적으로 cmake 파일을 생성한다.

### CMakeLists.txt

 프로젝트 최상층에 있는 cmake파일은 프로젝트 전반에 대한 설정과 구조를 담당한다. subdirectory로 googletest를 지정해주면 unittest를 사용할 수 있다.

```cmake 
CMAKE_MINIMUM_REQUIRED(VERSION 3.15.0)

SET(proj_name "CMakeTuto")
PROJECT( ${proj_name} )

# C++ settings
SET(CMAKE_CXX_STANDARD 11)

add_subdirectory(src)
add_subdirectory(tests)
add_subdirectory(extern/googletest)
```

### src/CMakeLists.txt

 실질적으로 우리가 사용할 program에 대한 cmake파일이다. 이전과는 다르게 executable 뿐만 아니라 코드를 이용한 라이브러리도 생성하는데, 이는 test sub project에서 main source를 가져와 사용해야하기 때문이다.

 또한 library/executable output 경로를 프로젝트 루트로 설정해주었다.

```cmake 
CMAKE_MINIMUM_REQUIRED (VERSION 3.15 )
# project setting
SET( BINARY ${CMAKE_PROJECT_NAME} )
SET( EXECUTABLE_OUTPUT_PATH ${CMAKE_SOURCE_DIR} )
SET( LIBRARY_OUTPUT_PATH ${CMAKE_SOURCE_DIR} )

# loading SDL2 package 
FIND_PACKAGE(SDL2 REQUIRED)
INCLUDE_DIRECTORIES(${SDL2_INCLUDE_DIRS}) 

# set source and headers
SET (SRC_FILES
    main.cpp 
    utils.cpp 

    utils.h
    )

# build 
ADD_EXECUTABLE( ${BINARY}_run  ${SRC_FILES} )
ADD_LIBRARY( ${BINARY}_lib STATIC ${SRC_FILES} )
TARGET_LINK_LIBRARIES( ${BINARY}_run  ${SDL2_LIBRARIES})
TARGET_LINK_LIBRARIES( ${BINARY}_lib  ${SDL2_LIBRARIES})
```

### tests/CMakeLists.txt

 실제로 우리가 test로 사용할 cmake 파일이다.

 `file(GLOB_RECURSE TEST_SOURCES LIST_DIRECTORIES false *.h *.cpp)`이라는 새로운 함수가 등장하였는데, 이는 현재 cmake가 있는 폴더를 기준으로 모든 `.h/.cpp` 확장자를 가진 파일을 `$TEST_SOURCES` 변수에 저장한다는 의미이다.

 그리고 `target_link_libraries`로 `lib`와 `gtest`를 linking한다.

```cmake
SET(BINARY ${CMAKE_PROJECT_NAME}_tst)
SET(EXECUTABLE_OUTPUT_PATH ${CMAKE_SOURCE_DIR} )

file(GLOB_RECURSE TEST_SOURCES LIST_DIRECTORIES false *.h *.cpp)
SET(SOURCES ${TEST_SOURCES})

add_executable(${BINARY} ${TEST_SOURCES})
add_test(NAME ${BINARY} COMMAND ${BINARY})
target_link_libraries(${BINARY} PUBLIC ${CMAKE_PROJECT_NAME}_lib gtest)
```

## test codes 

 cmake 설정은 다 끝났으니 테스트할 간단한 코드를 만들어보자.

### main.cpp

```cpp
#include "gtest/gtest.h"

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

### testa.cpp

```cpp
#include "gtest/gtest.h"

TEST(testA, test1) {
    EXPECT_EQ (0, 0);
    EXPECT_EQ (true, true);
    EXPECT_NE (true, false);
}
```
## Result 

 위 예시대로 문제없이 따라했다면 최종적으로 output이 아래와 같이 나올것이다. terminal에서 `./curaai_tst` 이런식으로 실행해도 성공적으로 `gtest`를 실행할 수 있다.

```bash 
├── curaai_run
├── curaai_tst
├── libcuraai_lib.a
```


### launch.json 

 아직 visual studio code 자체는 잘 몰라서 활용을 못하지만, 간단하게 configuration에 program을 바꿔주면서 `exectuable_tst`가 실행되도록 설정해놨다.

```json
{
    // Use IntelliSense to learn about possible attributes.
    // Hover to view descriptions of existing attributes.
    // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
    "version": "0.2.0",
    "configurations": [
        {
            "name": "run binary debug",
            "type": "cppdbg",
            "request": "launch",
            "program": "${workspaceFolder}/curaai_run",
            "args": [],
            "stopAtEntry": false,
            "cwd": "${workspaceFolder}",
            "environment": [],
            "externalConsole": false,
            "MIMode": "lldb"
        },
        {
            "name": "run gtest",
            "type": "cppdbg",
            "request": "launch",
            "program": "${workspaceFolder}/curaai_tst",
            "args": [],
            "stopAtEntry": false,
            "cwd": "${workspaceFolder}",
            "environment": [],
            "externalConsole": false,
            "MIMode": "lldb"
        }
    ]
}
```

### GoogleTest Adapter extension

 필자의 경우에는 VSCode extension인 `GoogleTest Adapter`를 설치하였다.

![adapter](/assets/images/2020-02-28/gtest_adapter.png)

 adapter를 사용하기 위해서는 먼저 launch.json에 configuration에 gtest가 들어가 있어야 하니 참고하자. 아래 사진은 위에 있는 예제코드를 adapter로 돌린 결과이다.

 아직 더 알아보진 않아서 모르지만, 더 좋은 test tool이나 extension이 있다면, 업데이트 하겠다.

![adapter_result](/assets/images/2020-02-28/adapter_result.png)


## Reference

참고 사이트

- https://raymii.org/s/tutorials/Cpp_project_setup_with_cmake_and_unit_tests.html