---
-layout: post
-date:   2018-10-23 23:45:35 +0900
-tags: Visualization
---


기본적으로 Deep Neural Network는 black box이다. network의 forwarding 과정을 모른다. _그래서 우리는 통계적인 방법을 이용하여 분석을 한다. precision, recall, acucracy, confidence, loss 등등 다양한 meausre를 통해 결과를 분석하여 부족한 점을 알아낸다._ 하지만 이것 말고도,앞으로 설명할 network visualization을 통해 convolution networks를 해석 할 것이다.   

<!--more-->

---

## Class Activation Mapping 

 처음에는 classification model을 시각화했다. _Convolution Network의 마지막 layer feature는 model이 인식한 object의 spatial information을 가지고 있다. 하지만 이 정보는 fully-connected layer를 거치며 사라진다._

 하지만 object의 activation region을 사용하기 때문에 fc layer에서 정보가 사라지면 안됀다. 그래서 마지막 convolution layer에서 global average pooling을 실행한다. **global average pooling은 activation된 각 channel의 값(정보량)의 평균이다.** 이 다음 fully connected layer를 연결한다.
 
 classification network의 forwarding은 다음과 같다.
 1. $f_k(x, y)$는 feature에서 $k$번째 channel의 $(x, y)$값이다. => $
 F_k = \sum_{x, y} f_k(x, y) $
 2. class $c$의 softmax input은 각 channel의 평균 * weight $w^c_k$의 합이다. => $
 S_c = \sum_k w^c_k F_k $
 3. softmax의 probability output $P_c$는 Eqn(3)과 같다. => $
 P_c = \frac{exp(S_c)}{\sum_c exp(S_c)} $
 마지막 convolution layer feature에서 위 연산을 한다. 이때 bias term은 무시한다.

 ![Cam Fig. 1](/assets/images/Grad-CAM/CAM_Figure1.png){:.border.rounded}

 $$
 M_c(x,y) = \sum_k w^c_k f_k(x,y) \notag
 $$

 **이때 class $c$에 대한 channel $k$의 중요도는 weight $w^c_k$이다. 그렇다면 각 channel의 activation feautre map에 중요도 $w^c_k$를 곱하여 모두 합치면, class $c$의 Class Activation Map $M_c$가 완성된다.**

---
## Guided Back Propagation 

 딥러닝의 네트워크는 결국엔 activation을 곱하고 곱하여 output을 내는 것이다. model이 원하는 output을 내도록 weight를 어떠한 최적값으로 수렴시키는 것이 학습방법인데, 그때 우리가 원하는 값과 output의 차이를 loss로 표현한다. loss를 back propagation하여, 각 weight를 미분하여 weight를 update한다. 

 ![Guide Fig. 1](/assets/images/Grad-CAM/Guide_Figure1.png){:.border.rounded}

 이때 activation이 클수록 output에 대해 확률이 크다고 예상할 수 있는데, 이때 output과 target이 다르면 back-prop되는 gradient가 크다. **우리는 visualizing할 class에 대해서는 loss를 의도적으로 만들어 버리고, 다른 class에 대해서는 loss를 0으로 만들어 gradient를 없애버린다. 그 다음 모든 gradient로 image를 만들었을 때, network가 어느 부분에서 activation이 됐는지 추정할 수 있다.**
 
 <img src="/assets/images/Grad-CAM/Guide_Figure2.png" width="600px" height="350px"/>

 _또한 positive gradient만 visualizing하여, 다른 class에 negative로 생기는 gradient는 아예 없애 버렸다._ 

 --- 
## Grad-CAM 

 CAM은 좋은 기능이지만, 치명적인 단점이 있다. **CAM을 사용하기 위해서는 Global Average Pooling과 fully connected layer가 필요하고, 기존 모델은 architecture 변경이 필요할 수도 있다.**  

 해결방안이 Grad-CAM이다. **Deep Learning의 특징이 있다. 바로 gradient이다. 이 gradient를 CAM의 $w^c_k$와 GAP를 대체한다.**

 ![Grad-CAM Fig. 1](/assets/images/Grad-CAM/Grad-CAM_Figure1.png){:.border.rounded}

 Grad-CAM의 과정을 수식으로 나타내면 아래와 같다.
1. visualizing하고 싶은 layer의 feature map $A^k$의 gradient를 평균낸다. => $ \alpha^c_k = \overbrace{\frac{1}{Z} \sum_i \sum_j}^{\text{global average pooling}}\underbrace{\frac{\partial y^c}{\partial A^k_{ij}}}_{\text {gradient via backprop}} \notag $
2. feature map에서 $\alpha^c_k$를 $k$번째 channel의 중요도로서 feature map에 선형결합하여, CAM에서 $w^c_k$의 역할을 대신한다. => $ M^c_{Grad-CAM} = ReLU \underbrace{\bigg( \sum_k \alpha^c_kA^k \bigg)}_{\text{linear combination}} \notag $

 _위 과정에서 ReLU를 사용하는 이유는 feature의 positive influence만 특정 classs에 대해 activation한 것이고, negative pixel은 다른 classes에 속해 있기 때문이다. 또한 Grad-CAM에서는 weight마다 특정 class로 특성화시킬 수 없기 때문에 guided back prop처럼 보고싶은 class에 대해서만 loss를 만들어 gradient를 발생시킨다._

 _구현팁: network가 깊어질수록 feature map은 더 작아지므로, Grad-CAM을 input image에 mapping할 때는 bilinear interpolation resize로 이미지 사이즈를 키운다._

### Guided Grad-CAM

 ![Guide Grad-CAM Fig. 1](/assets/images/Grad-CAM/Guide Grad-CAM_Figure1.png){:.border.rounded}

 **Guided back propagation에서는 gradient가 된 곳은 다 activation되기 때문에 가장 activation이 크게 일어난 곳은 알수없다.** 또한 Guided back prop의 map은 위의 문제점 때문에 background나 자잘한 edge에서도 몇몇 activation이 일어나기 일쑤였다. **그래서 Grad-CAM에 Guided Back Prop Map을 단순히 elementwise product하면 Guided Grad-CAM이 완성된다.**
