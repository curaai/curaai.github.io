---
-layout: post
-date:   2018-10-26 10:45:35 +0900
-tags: Image Recognition
---


 분류문제에서 softmax를 통해 나온 확률을 confidence라고 부른다. confidence의 의미는 네트워크가 예측한 predict가 실제로 해당 class일 확률이다. 그래서 우리는 이 confidence를 기반으로 network를 분석한다. 하지만 실제로 confidence가 overfitting됐다.

<!--more-->

---
## Calibration
 네트워크의 confidence값이 잘못됐다는 것을 검증하기 위해, 일단 calibration의 정의를 해보자. 사전에서는 {calibrate: 보정하다, 정밀하게 측정하다}란 의미이다. 여기서 우리는 네트워크의 confidence의 값이 실제로 맞는지를 검증한다.
  
 Input $ x \in X$, k개의 class를 가진 label $y \in Y = \{1, \dots, K\}$라고 표현할 때 neural network $h$는 $h(x) = (\hat{y}, \hat{p})$. class prediction $\hat{y}$와 그에 해당하는 confidence $\hat{p}$, 즉 해당 class가 맞을 확률을 예측한다. 

 우리는 network가 예측한 confidence를 검증하기 위해 예측결과를 $M$개의 그룹으로 나누고, 각 그룹의 confidence와 accuracy를 수식적으로 정의한다. m번째 그룹에는 $I_m =  (\frac{m-1}{M}, \frac{m}{M}]$ 구간에 해당하는 값들이 들어가 있다. 이때 우리는 accuracy와 confidence가 같다면 well-calibrated, 다르다면 mis-calibrated라고 부른다.

 $$ acc(B_m) = \frac{1}{|B_m|} \sum\limits_{i \in B_m} \textbf{1} (\hat{y}_i = y_i)$$

 m번째 그룹의 accuracy는 label을 맞춘 개수 / 그룹의 크기 이다. 각 bin에서 예측한 label과 실제 label이 같은 비율이 accuracy이다.

 $$conf(B_m) = \frac{1}{|B_m|} \sum\limits_{i \in B_m} \hat{p}_i$$

 각 bin의 confidence의 평균이다.

## Calibration Error 

 accuracy와 confidence의 distribution 차이(Expected Calibration Error)를 다음과 같이 정의한다. _n은 set의 크기이다._ 이때 각 그룹의 차이 값을 gap으로 표현한다.

 $$ \textbf{ECE} = \sum\limits_{m=1}^M \frac{|B_m|}{n} \big| acc(B_m) - conf(B_m) \big|$$

 confidence가 high-risk한 경우에는 또다른 measure가 필요하다. 가장 큰 gap을 minimize하기 위해 Maximum Calibration Error를 정의한다.

 $$ \textbf{MCE} = \underset{m \in {1, \dots, M}}{\max} |acc(B_m) - conf(B_m)|$$

## Mis-Calibration

 ![Fig. 1](/assets/images/Calibration/Figure1.png)

 옛날에 비해 요즘 networks는 deep & wide해졌다. 또한 성능또한 deep한 만큼 더 잘나온다. 하지만 예측한 confidence가 실제로 맞지 않다면 어떨까. neural network의 결과값이 해당 data에 대한 confidence가 적절하게 나오는 것이 아닌, 높은 confidence값이 나오도록 학습이 된것이다.

 1998년의 LeNet과 현재 많이 쓰이는 ResNet을 비교해보자. LeNet은 confidence도 0~1까지 고르게 분포되어 있는 반면, ResNet은 0.9~1에 60%이상이 분포되어 있다. 

 ![Fig. 3](/assets/images/Calibration/Figure3.png)

 ResNet-110이 CIFAR-100을 학습할 때의 그래프이다. learning rate는 250 epoch에서 감소하였다. 그뒤 360 epoch쯤에서 다시 loss와 error가 떨어졌다. 하지만 이때 Negative Log Likelihood는 training set에 overfitting되기 시작하면서, error는 낮지만 높은 confidence값을 내기위해 학습하였다.

 ![Fig. 2](/assets/images/Calibration/Figure2.png)

 보시다시피, model의 성능을 향상시키는 것들이 오히려 calibration에는 악영향을 주고있다. 

## Temperature scaling

 모든 class에 대해 single scalar parameter $T$를 사용하여 logit vector $z_i$에 나눠준다. 

 $$ \hat q_i = \underset{k}{\max} \sigma_{softmax}(z_i/T)^k$$

 T로 나눠주면 전체적으로 scaling이 되면서, softmax값이 우리가 원하는 confidence값이 나오도로 조정이 될것이다. 또한 temperature scaling은 model에 직접적인 영향을 주지 않기 때문에, validation set에서 자유롭게 학습시킬 수 있다.
