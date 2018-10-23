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

 ![Cam Fig. 1](/assets/images/Grad-CAM/CAM_Figure1.png)

 $$
 M_c(x,y) = \sum_k w^c_k f_k(x,y) \notag
 $$

 **이때 class $c$에 대한 channel $k$의 중요도는 weight $w^c_k$이다. 그렇다면 각 channel의 activation feautre map에 중요도 $w^c_k$를 곱하여 모두 합치면, class $c$의 Class Activation Map $M_c$가 완성된다.**

 

---

Grad-CAM 

CAM은 좋은 성능을 보여줬지만, deep neural network는 CNN classification model만 있는 것이 아니다. CNN + RNN, object detection, face recognition 등등 cnn이 wide하게 사용된다. 하지만 CAM은 가중치 w^c_k를 softmax와 연결된 fully-connected에서 왔지만, 일반적인 CNN은 fully connected를 사용하지 않는 경우가 많다. 이때는 CAM을 사용하지 못한다. 또한 classification model에서도 마지막 fully connected layer를 교체하여 재학습 해야한다. CAM은 general 하게 사용할 수 없다.  

그 해결방안이 Grad-CAM이다. Deep Learning을 이용한 neural network는 한가지 공통점이 있다. 바로 gradient이다. 모든 neural network는 gradient를 back propagation해서 학습을 하기 때문에, 이 공통적인 gradient를 visualizing하고 싶은 Layer의 feature에 가중치로서 사용한다. 

\alpha^c_k = \overbrace{\frac{1}{Z} \sum_i \sum_j}^{\text{global average pooling}}\underbrace{\frac{\partial y^c}{\partial A^k_{ij}}}_{\text {gradient via backprop}}

weight \alpha ^c_k는 neuron importance를 나타낸다. 여기에 k번째 unit의 feature map A^k와 \alpha ^c_k를 weighted combination한뒤, gradient이다 보니 다른 class에 대한 gradient가 포함될수 있는데, 이때 positive한 gradient는 class c이고, negative gradient는 다른 class이므로, ReLU 연산을 통해 class c에 대한 Gradient-Class Activation Map 을 만든다.  

M^c_{Grad-CAM} = ReLU \bigg ( \sum_k \alpha ^c_k A^k \bigg )

수식으로 표현하자면 Eqn (3)과 같다.



Guided Back Propagation은 positive gradient만을 visualizing 한 것이다.Grad-CAM은 feature 사이즈 이므로 bilinear interpolation을 사용하여 source image로 resize한다.



위의 그림은 Visual Question Answering에서 사용한 Grad-CAM이다. 
