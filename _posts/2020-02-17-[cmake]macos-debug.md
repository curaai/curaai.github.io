---
title: (CMake 튜토리얼) 0a. MacOS X VisualStudio Code debug 세팅
tags: cmake, visual-studio-code
---

 MacOS X Catalina 이상의 버전에서 디버깅이 안되는 삽질을 경험한 후 정리하는 글입니다.
<!--more-->

---
 
 본격적으로 개발을 할려는데 실행은 분명히 잘되지만 debugging이 안된다. cmake-tools, visual studio code document등등을 뒤지며 똑같이 따라했지만 하나도 동작하지 않았다.

 하지만 정답은 MS가 아니라 vscode-cpptools 즉 code-ext에 있었다...  ~~에휴 내시간 삽질 엄청헀네;;;~~ 

## Problem

 요상하게 debug를 할려고만 하면 lldb라는 mac debugger로 잘넘어가는데 실행만 안되는 것이었다.
 ![img2](/assets/images/2020-02-17/1.png)

## Solution

 여러 구글링을 하며 뒤져본 결과 해답은 [여기](https://github.com/microsoft/vscode-cpptools/issues/3829) 있었다. 필자가 영어 읽어가며 삽질을 이미 했기 때문에 이글만 따라오면 편하게 할 수 있을것이다.

 포인트는 vscode-extension `CodeLLDB` 설치와 launch.json 수정이다.

### LLDB 설치 

 아래 이미지와 같이 CodeLLDB를 설치해주자.

![img2](/assets/images/2020-02-17/2.png)

### launch.json 수정

 launch.json은 이전 글을 봤다면 필자와 같이 아래와 같을텐데, type을 `cppdeb`가 아닌 `lldb`로 바꿔줘야한다.

```json
{
    // Use IntelliSense to learn about possible attributes.
    // Hover to view descriptions of existing attributes.
    // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
    "version": "0.2.0",
    "configurations": [
        {
            "name": "clang++ build and debug active file",
            "type": "cppdbg",
            "request": "launch",
            "program": "${workspaceFolder}/cmake-test",
            "args": [],
            "stopAtEntry": false,
            "cwd": "${workspaceFolder}",
            "environment": [],
            "externalConsole": false,
            "MIMode": "lldb",
            "preLaunchTask": "clang++ build active file"
        }
    ]
}
```

 아마도 `cppdbg`는  `C/C++` extension에 딸려오는 디버거인것같다. 하지만 우리는 Mac이기 때문에 launch.json을 아래스크립트로 바꿔준다. 물론 중간에 섞여있는 `program` property는 알아서 바꾸도록 하자.

```json
{
    // Use IntelliSense to learn about possible attributes.
    // Hover to view descriptions of existing attributes.
    // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
    "version": "0.2.0",
    "configurations": [
        {
            "name": "(lldb) 시작",
            "type": "lldb",
            "request": "launch",
            "program": "${workspaceFolder}/cmake-test",
            "args": [],
            "cwd": "${workspaceFolder}"
        }
    ]
}
```

## 삽질 성공 

 아마 코딩을 갓배운 사람도 이정돈 할수있겠지. 끝이다. 성공적으로 breaking point가 걸린모습이다. 후후 

![img4](/assets/images/2020-02-17/4.png)