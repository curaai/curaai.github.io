---
title: (VVVVVV) 1. Refactoring Start
tags: project-review game
mathjax: true
---

 `VVVVVV`를 개발하기 위한 환경을 setup하고 나서, 실행도 해보고 이것지것 리팩토링 시도도 해보았다. 잘되지는 않았지만, 가장 큰 문제는 test code의 부재, code안에 적혀있는 data script, 복잡하게 얽힌 스파게티 코드이다. 이를 풀어나가는 글의 첫번쨰 시리즈이다.

<!--more-->
---

## Data script in code

 가장 먼저 발견한 것은 여러 코드에 rendering되는 data script가 무분별하게 쓰여있는 것 이다. 자주 등장하는 단어도 아닐뿐더러 많은 수의 script가 들어가있다보니 파일의 크기와 line은 엄청 늘어나 있었다. 이처럼 `str`형식의 data가 들어가있는 경우도 있고, 특정상황에서 쓰는 setting table같은 것 들이 일제 없었다.

 그래서 여러 코드가 몇천줄이 넘어가 가독성을 해칠 뿐 더러 해당파일이 나타내는 의미를 흐트리고 있었다.

`cf53de: desktop_version/src/Render.cpp Ln 93 ~ 121`
```cpp
switch (game.currentmenuoption)
 {
  case 0:
      graphics.bigprint( -1, 30, "Accessibility", tr, tg, tb, true);
      graphics.Print( -1, 65, "Disable screen effects, enable", tr, tg, tb, true);
      graphics.Print( -1, 75, "slowdown modes or invincibility", tr, tg, tb, true);
      break;
  case 1:
      graphics.bigprint( -1, 30, "Advanced Options", tr, tg, tb, true);
      graphics.Print( -1, 65, "Hide the mouse cursor, remove", tr, tg, tb, true);
      graphics.Print( -1, 75, "the loading screen, turn on", tr, tg, tb, true);
      graphics.Print( -1, 85, "glitchrunner mode and more", tr, tg, tb, true);
      break;
  case 2:
#if !defined(MAKEANDPLAY)
    if (game.ingame_titlemode && game.unlock[18])
#endif
    {
      graphics.bigprint( -1, 30, "Flip Mode", tr, tg, tb, true);
      graphics.Print( -1, 65, "Flip the entire game vertically.", tr, tg, tb, true);
      if (graphics.setflipmode)
      {
          graphics.Print( -1, 85, "Currently ENABLED!", tr, tg, tb, true);
      }
      else
      {
          graphics.Print( -1, 85, "Currently Disabled.", tr/2, tg/2, tb/2, true);
      }
    }
 }

```

## Seperate Menu part 

 첫번째로 시작한 것은 Render.cpp와 menu의 수정이다. 게임을 시작하면 가장 먼저 우리가 만날 수 있는 부분이기도 하다. 이것이 제일 큰 이유인데, `VVVVVV`의 코드는 매우 많이 얽혀있고 복잡하다. 그래서 게임을 시작하면 가장 먼저 볼 수 있는 menu를 제일 먼저 선정하였다. 이는 debugging하기도 쉽고, 복잡하게 얽혀있는 것 중 가벼운 편이기도하다. 그래서 menu를 시작으로 차차 하나씩 분리해나갈 예정이다.
 
 일단은 위에서 언급했듯 data script를 code에서 제일 먼저 분리하는 것을 목표로 하였다. 첫번째 주의할 점은 함부로 code를 수정하다가 기존의 기능과 똑같이 동작하지 못하는 것이다. 그래서 함부로 수정할 수 없는 것은 legacy로서 남겨두고 차차 refactoring을 진행하며 나중에 수정하기로 하였다.

### SimpleMenu Class

 가장 먼저 menu를 `game class`에서 분리했다. menu와 관련된 member를 SimpleMenu 클래스를 만들어서 분리하였다. 프로그램이 `extern` keyword를 통해 모든 파일에서 `game`, `graphics`, `map`과 같은 글로벌 전역변수를 공유하여 사용했기 때문에 프로젝트 전체 찾기로 변수의 이름만 바꿔주면 됬기 때문에 어렵지 않았다. 아직은 legacy를 유지하기 위해서 game의 member로 `menu_`라는 이름의 refactoring된 menu 변수를 뒀다.

## Message Class

### Load data w/o data.zip & Parse Json 

 본격적으로 data script를 code에서 떼어내기위해 3rd_party에 nlohmann이 만든 json을 추가했다. 기존에 사용하던 xml을 버리고 요즘 통상적으로 많이 쓰는 json을 도입했다. 하지만 프로그램은 **data.zip을 load하여 내부적으로 data를 parse**하고 있는 방식이어서 우리가 만든 custom data를 추가하기엔 다소 제한이 있었다. _기존의 방식을 유지하려면 개발할때마다 data.zip에 custom json을 추가하고 재압축하는 방식으로 했어야 했다._ directory에서도 읽을수 있는 코드를 추가하였다. 다행히도 filesystemutils에 간단한 코드 몇줄만 추가하면 사용할 수 있었다.
 
### Print Menu codes 

 render.cpp의 menurender 함수를 보니 여러 형태로 나눌 수 있었다. 

1. bigprint, print로 이루어진 경우
2. if로 제어를 해서 상황에 다르게 보여줘야 하는 경우 
3. switch등을 통해 다중 if가 들어간 경우 
4. bigprint, print, imagecol등 여러 함수를 쓰는 경우 
5. 프로그램에서 사용되는 변수가 formatting되어 들어가는 경우

 등등이 있었다. 2a4206 ~ a7286d 동안 1~3을 `Message`, `ToggleMessage`, `ListMessage`라는 class를 만들었다. ToggleMessage와 ListMessage는 Message를 상속받아 json을 생성자의 argument로 받아 field를 로딩하는 식으로 했다.

```cpp
class Message
{
public:
    Message() {}
    Message(std::string text, int x, int y)
        : _text(text)
        , x(x)
        , y(y)
    {}
    Message(const nlohmann::json& _json)
    {
        if (_json.contains("x"))
            x = _json["x"].get<int>();
        if (_json.contains("y"))
            y = _json["y"].get<int>();
        if (_json.contains("msg"))
            _text = _json["msg"].get<std::string>();
        if (_json.contains("color"))
            _color = RGBA{ _json["color"].get<std::string>() };
        if (_json.contains("type"))
            _type = _json["type"].get<std::string>();
    }

    virtual std::string text(void) const { return _text; }
    virtual RGBA color(void) const { return _color; }
    std::string type(void) const { return _type; }

    int x = -1;
    int y = -1;

protected:
    RGBA _color;
    std::string _type = "simple";
    std::string _text;
};

class ToggleMessage : public Message
{
public:
    ToggleMessage(const nlohmann::json& _json)
        : Message(_json)
    {
        _text_sub = _json["msg_sub"].get<std::string>();
        if (_json.contains("color_sub"))
            _color = RGBA{ _json["color_sub"].get<std::string>() };
    }

    std::string text(void) const override
    {
        return toggle_on ? _text : _text_sub;
    }
    RGBA color(void) const override { return toggle_on ? _color : _color_sub; }
    void set_toggle(bool _toggle_on) { toggle_on = _toggle_on; }

protected:
    bool toggle_on = true;
    std::string _text_sub;
    RGBA _color_sub;
};

class ListMessage : public Message
{
public:
    ListMessage(const nlohmann::json& _json)
        : Message(_json)
    {
        for (auto str : _json["msg_list"])
            _text_list.push_back(str.get<std::string>());
    }

    std::string text(void) const override { return _text_list[idx]; }
    void set_index(int _idx) { idx = _idx; }

protected:
    std::vector<std::string> _text_list;
    int idx;
};
```

 추후에 바뀔수 있겠지만 가장 중요하게 볼 것은 virtual로 지정된 color와 text함수다. virtual로 지정하여 상속받는 class의 기능에 따라 다르게 출력되게 수정하였다.
