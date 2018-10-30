---
-layout: post
-date:   2018-10-29 23:45:35 +0900
-tags: Image Recognition
---

 CNN은 현재 Computer Vision 분야에서 아주 중요한 역할을 맏고있다. 그래픽카드의 성능이 향상되고, 여러 최적화 기법이 떠오르면서 자연스럽게 네트워크의 크기는 커졌다. **네트워크의 depth(stacekd layers)와 width(number of channel)이 커지면 커질수록 성능또한 상승한다는 것은 자명하다. 하지만 막상 그 크기에는 한계가 있는데 이를 어떻게 해결했는지 살펴보자.**

 <p align="center">
 <img src="/assets/images/ResNet/Figure1.png" width="450" height="250"/>
 </p>
  
 위 그림은 plain network에서 CIFAR-10 dataset의 성능을 평가한 것이다. _아무런 technique 없이 단순히 Convolution을 쌓은 네트워크를 "plain network"라고 한다._

<!--more-->
 
## Deep Residual Learning

 $H(x)$는 기존의 네트워크이고, $x$는 image input이다. 이때 $H(x)$는 여러개의 비선형 layer로 이루어져 인간이 이해할 수 없는 복잡한 함수(_classification, regression 등등_)에 천천히 근사한다.
 위 과정을 training이라고 하는데, 아쉽게도 우리가 원하는 것만큼 잘 학습이 되지는 않는다. 네트워크가 깊어질수록 성능이 올라가지만 어느이상부터는 **vanising gradient** 문제[^1]때문에, 발생되는 gradient가 너무 작아져 버려, 학습이 되지 않는 것이다.

### Identity mapping by Shortcuts 

 Residual Learning은 네트워크의 크기에 비해 **gradient**가 생기지 않는 degradation 문제를 해결한다. 
 $H(x)$라는 식을 조금 변경하여 $F(x) := H(x) - x$과 같이 바꿔준다. 그러면 $F(x) + x$는 $H(x)$에 근사하게 될텐데, 이 방법을 **Shortcut Connection**이라고 하고, 이때 **$x$를 더하는 것을 Identity Mapping[^2]이라고 한다.** short connection 형태를 한 network block을 **Residual Block**, 학습 방법을 **Residual Learning**이라고 한다.

 <p align="center">
 <img src="/assets/images/ResNet/Figure2.png" width="500" height="300"/>
 </p>
 
 shortcut connection을 하게 되면 identity mapping이 생기면서 $x$를 미분하면 gradient 1이 생기면서 residual block을 미분하면 최소 gradient가 1이상이 되어 back propagation을 할때 네트워크가 아무리 깊어져도 gradient가 소실되지 않기 때문에 학습이 잘되는 것이다.

### Residual Block 

 $$ 
 y = F(x, {W_i}) + x \\
 F(x, {W_i}) = W_2 \sigma(W_1 x)
 $$

 basic residual block은 위와 같이 생겼고, 일반적인 경우에는 $x$와 $F(x, {W_i})$의 dimension이 똑같지만, dimension이 바뀔 경우에는 Linear Projection weight $W_s$를 사용한다. $$ y = F(x, {W_i}) + W_s x $$

### Bottleneck Architecture 

 <p align="center">
 <img src="/assets/images/ResNet/Figure4.png" width="600" height="300"/>
 </p>

ResNet-50/101/152부터는 Network를 더 deep하게 쌓기 위하여 1x1, 3x3, 1x1 convolution을 하는 bottleneck architecture로 변경하였다. bottleneck architecture에서 feature의 dimension이 바뀔 때는 projection을 사용하여, dimension이 바뀌더라도 좀더 효율적인 성능을 내도록 디자인됐다. 

## Network Architecture

 <p align="center">
 <img src="/assets/images/ResNet/Figure3.png" width="600" height="1200"/>
 </p>

 convolution layer(34개), global average pooling, 1000-way fully connected layer, softmax를 순차적으로 사용한다. 놀랍게도 ResNet-34는 VGG의 연산량의 18%밖에 들지 않는다. _(ResNet: 3.6 billion FLOPs, VGG: 19.6 billion FLOPs)_

 오른쪽 라인이 ResNet-34이다. 검은색 실선이 shortcut connection을 나타내고, 실선은 dimension이 커질 때로 두가지 경우가 있다.
 - identity shortcut connection으로 identity mapping을 진행하되, dimension을 증가시키기 위해 zero padding을 한다. 
 - projection shortcut connection은 1x1 conv로 dimension을 늘인다.

### Implementation 

 모델은 ImageNet Challenge에 사용했고, fully convolutional architecture를 참고했다고 한다. 네트워크의 implementation tip은 다음과 같다. 
 - weight decay 0.00001 & momentum 0.9
 - drop out [x]
 - image는 256, 480 중 가까운 값으로 resize된다.
 - per-pixel mean substraction과 함께 224x224로 랜덤 cropping을 한다. 
 - batch norm을 conv 후, actvation 전에 사용한다.
 - batch size 256으로 SGD solver를 사용했다. 
 - learning rate는 0.1부터 시작해서, local minimum을 만나거나 loss가 진동하는 경우 1/10씩 감소시켰다. 
 - iteration은 60 * 10 ^ 4이다.

## Experiment 

 <p align="center">
 <img src="/assets/images/ResNet/Figure5.png" width="800" height="350"/>
 </p>

 실제로 ResNet을 사용했더니, network의 depth가 커져도 성능이 떨어지지 않고 향상된다.

 <p align="center">
 <img src="/assets/images/ResNet/Figure6.png" width="800" height="450"/>
 </p>

 그 당시 State of the Art의 성능을 ResNet-152가 이뤄냈다. 

[^1]: [Wikipedia: Vanishing Gradient Problem](https://en.wikipedia.org/wiki/Vanishing_gradient_problem)  
[^2]: [Arxiv: Identity Mapping in Deep Residual Networks](https://arxiv.org/abs/1603.05027)
