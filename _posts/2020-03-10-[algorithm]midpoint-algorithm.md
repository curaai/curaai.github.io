---
title: Midpoint Circle Algorithm
tags: algorithm
mathjax: true
---

 원을 최적화하여 그리는 알고리즘을 소개합니다.
<!--more-->

---

 사실 원을 그리고 싶으면 삼각함수를 이용하면 충분히 그릴 수 있다. 하지만 한 프레임마다 여러개의 원을 그리고, 최대의 FPS를 뽑아내야 하는 게임/GUI 프로그램의 경우, 이러한 graphics요소들을 최적화하여 렌더링하는 것이 중요하다.

 우리의 목표는 연산을 최적화하여 원을 그리는 것이다. 삼각함수를 이용하여 원을 그린다면 충분히 그릴수있지만, float연산으로 인해 느리다. 또한 컴퓨터 좌표계에선 float인 좌표를 나타낼 수 없다. 

## 원의 방정식
 
 중심이 원점이고 반지름이 $r$인 원의 방정식은 아래와 같다.

$$ d = x^2 + y^2 - r^2$$ 

 임의의 한점이 주어졌을 때 방정식의 값에 따라 원안, 원위, 원밖을 구분할수 있다. 원의 방정식을 $f(x, y)$라 할때 아래와 같이 알수있다.
- $0 > d$: 원안 
- $0 = d$: 원위
- $0 < d$: 원밖

## MidPoint Circle Algorithm

 0도부터 45도까지의 컴퓨터 좌표상의 x, y의 값을 구해 부호와 x,y의 값을 바꿔가며 원 전체를 그릴수 있다.

 시작을 원위 **A**라 할때. 다음으로 그릴점의 위치는 **B** 또는 **C**은 두 점의 중심 $y+0.5$의 $f(x_k+1, y_k-0.5)$에 따라 달라진다. 이를 수식으로 나타내면 아래와 같다.

$$\begin{align}

d_k &= (x_k+1)^2 + (y_k+0.5)^2 - r^2 \\
&= x_k^2 +2x_k+1 + y_k^2-y_k+1/4 - r^2 
\end{align}$$

![coord](/assets/images/2020-03-10/polar.png)

 위 방법으로 $k+1$번째 점이 **D**/**E** 인지 알수있다. 이때 $d_k$의 값을 이용한다.

$$ \begin{align}
d_{k+1} &= (x_k+2)^2 + (y_{k+1}-0.5)^2 - r^2 \\
&= x_k^2 +4x_k+4 + y_{k+1}^2-y_{k+1}+1/4 - r^2  \\
&= d_k + 2x_k + 3 + (y_{k+1}^2 - y_k^2) - (y_{k+1} - y_k)
\end{align}$$

 하지만 $y_{k+1}$의 값은 $d_k$의 값에 따라 달라지므로, $d_{k+1}$의 값도 점화식으로 재정리한다.

$$ y_{k+1} =\begin{cases}
y_k, & \text{if $d_k$ < 0} \\
y_k-1, & \text{if $d_k$ $\geq$ 0 }
\end{cases}$$

$$ d_{k+1} = \begin{cases}
d_k + 2x_{k+1} + 1 , & \text{if $d_k$ $\geq$ 0 } \\
d_k + 2x_{k+1} - 2y_{k+1} + 1, & \text{if $d_k$ < 0}
\end{cases} $$

## C 코드 구현

 코드 상으로는 실제 좌표계가 아닌 컴퓨터 좌표계여서, 아래부터 그리기 시작한다. 하지만 결과적으로 45도만 돌면 원전체를 채울수있기 때문에 문제는 없다. _45 * 8 = 360_

```cpp
int main(int argc, char* argv[])
{
	// 반지름
	const int r = 10;

	bool arr[21][21] = {0};

	// decision parameter
	int d;
	int x=0, y=10;
	int cx=10, cy=10;

	auto draw = [&](int ax, int ay) 
	{
		arr[cy+ay][cx+ax] = true;
		arr[cy+ax][cx+ay] = true;
		arr[cy-ay][cx+ax] = true;
		arr[cy-ax][cx+ay] = true;
		arr[cy+ay][cx-ax] = true;
		arr[cy+ax][cx-ay] = true;
		arr[cy-ay][cx-ax] = true;
		arr[cy-ax][cx-ay] = true;
	};

	d = 1 - r;	
	while(x < y) 
	{
		if(d < 0)
			d += x + x + 1;
		else  {
			d += x + x - y - y + 1;
			y--;
		}
		draw(x, y);
		x++;
	}

	for (int i=0; i<2*r+1; i++) {
		for (int j=0; j<2*r+1; j++) {
			const char* a = arr[i][j] ? "1 " : "0 ";
			cout << a;
		}
		cout << endl;
	}

	return 0;
}
```

## 결과

 위 테스트 코드의 결과이다. 필자의 경우 반지름이 10인 원으로 했으나, 알아서 잘 wrapping해서 가져가길 바란다.
```bash 
                ■ ■ ■ ■ ■                 
          ■ ■ ■           ■ ■ ■           
        ■                       ■         
      ■                           ■       
    ■                               ■     
  ■                                   ■   
  ■                                   ■   
  ■                                   ■   
■                                       ■ 
■                                       ■ 
■                                       ■ 
■                                       ■ 
■                                       ■ 
  ■                                   ■   
  ■                                   ■   
  ■                                   ■   
    ■                               ■     
      ■                           ■       
        ■                       ■         
          ■ ■ ■           ■ ■ ■           
                ■ ■ ■ ■ ■                 
```