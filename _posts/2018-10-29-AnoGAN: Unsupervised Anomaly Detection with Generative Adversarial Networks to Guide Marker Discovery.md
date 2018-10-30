---
-layout: post
-date:   2018-10-29 23:45:35 +0900
-tags: Anomaly Detection
---

 <p align="center">
 <img src="/assets/images/AnoGAN/Figure2-b.png" width="250" height="250"/>
 </p>
 
 기존의 anomaly detection은 확률과 통계에 기반하여 이미지의 비정상부분을 탐지하는 방법이었다. feature가 normal의 distribution과는 차이가 많을 때, 어떠한 지점을 기점으로 anomaly라고 판단한다. 하지만 사람이 직접 threshold를 정해야 하며 이에 따라 risk또한 크다. 이를 GAN을 사용한 Deep Learning으로 data labeling없이 해결할 수 있다. 

<!--more-->

### Anomaly Detection on Medical Image

 Anomaly Detection은 다양하게 사용될 수 있다. 이 논문에서는 의학 이미지에서 비정상적인 부분을 찾는 것을 base로 두고 접근한다. 이는 사람의 생명과 직결되어 민감할 수 있는데, Deep Learning으로 자동화하고, statistic한 접근이 아닌 GAN을 통해 문제를 해결한다. 

### Generative Adversarial Networks
 
 GAN을 기본적으로 알고 있다고 설정하고, 간단히 설명하고 넘어가겠다. 

 <p align="center">
 <img src="/assets/images/AnoGAN/Figure2-a.png" width="400" height="200" />
 </p>

 GAN[^1]은 generated image인지 real image인지 구별하는 classifier, Discriminator와 진짜 같은 fake image를 만들어내는 Generator로 이루어져있다. D(Discriminator)는 진짜 이미지를 잘 구별하도록 학습하며, G(Generator)는 진짜 같은 이미지를 만들도록 학습하며 서로 경쟁한다. G는 D를 통해 network를 update하며, G는 real image의 distribution을 D를 통해 학습한다. 
 
 $$
 \underset{G}{min} \underset{D}{max} V(D, G) = \mathbb{E}_{x \sim p_{data}(x)}[log D(x)] + \mathbb{E}_{z \sim p_z(z)}[log(1 - D(G(z)))]
 $$

## Unsupervised Manifold Learning of Normal Anatomical Variability 

 - set of images => $M$
 - image => $I_m$
 - $I_m$에서 $c \times c$ size의 image patch $K$개를 뽑아냄 => $ \textbf x = x_{k,m} \in X$, with $k = 1, 2, \dots, K$ 

 Generative model은 manifold X => training images의 variability를 학습한다.
 test시에는 $<y_n, l_n>$이 주어지며, test dataset $J$에서 ${0, 1}$로 이루어진 binary image-wise ground-truth label이 $ c \times c$ size의 patch image $y_n$로 주어진다. label은 성능 테스트시 사용된다. 

### Mapping new Images to the Latent Space

 adversarial training이 끝났을 때, generator는 latent space random vector $z$에서 image $x$로 매핑하는 것을 학습 완료한다. 하지만 GAN은 역으로 $x$에서 $z$로 mapping은 할수 없다.

 **그래서 우리는 query image $x$가 주어졌을 때, $z$로 근처의 2개의 image를 random sampling한다. sampled image 중 $x$와 가까운 쪽으로 $z$를 update하며, $G(z)$에 해당하는 point $z$를 찾는다.** 
 
 최적의 $z$를 찾기 위해 random sampling된 $z_1$에서  생성된 이미지 $G(z_1)$으로 loss function을 정의하여 $z$를 update한다. gradient descent를 진행하며, data distribution $p_g$에서 back prop iteration $\gamma = 1, 2, \dots, \Gamma$으로 query image $x$와 가장 비슷한 $G(z_{\Gamma})$를 찾는다. _DCGAN의 walking on latent space 참고[^2]_

### Loss function

 residual loss와 discrimination loss를 정의한다. 
 residual loss는 query image $x$와 generated image $G(z)$의 **visual similarity(pixel wise)를 비교한다.**
 discrimination loss는 generated image $G(z)$와 query image $x$를 discriminator로 **feature의 거리를 loss로 정의한다. 두 이미지의 data distribution(manifold)에서의 거리를 측정한다.** 
#### Resiudal loss 

 $$
 L_R(z_{\gamma}) = \sum |x - G(z_{\gamma})|
 $$

 residual loss는 query image $x$와 $G(z)$의 visual dissimilarity를 비교한다. _만약 query image $x$가 정상이 이미지이라면, residual loss는 0에 가까운 될것이다._
#### Discrimination Loss

 $$
 L_D(z_{\gamma}) = \sum |f(x) - f(G(z_{\gamma}))|
 $$

 이떄 $f(\cdot)$는 feature extractor로서 D에서 signle sigmoid value를 내기 전 layer나, D의 output이 single value가 아니라면 마지막 layer가 될것이다.  
 discrimination loss로, 단순히 진짜같은 이미지를 만들어내는 것 뿐만아니라, **이미지를 잘 만들어 data distribution에 가까워 지는 것에 초점을 맞췄다.**  
#### Overall Loss 

 $$
 L_(z_{\gamma}) = (1 - \lambda) * L_R(z_{\gamma}) + \lambda * L_D(z_{\gamma})
 $$

 latent space로 mapping하기 위한 전체 loss를 두 요소의 weighted sum으로 정의한다.  
 z만을 back propagation으로 학습하며, generator와 discriminator의 parameter는 고정된 상태로 학습한다. 

## Detect anomalies 
 
 $$ 
L_(z_{\Gamma}) = (1 - \lambda) * L_R(z_{\Gamma}) + \lambda * L_D(z_{\Gamma})
 $$

 $G(z_{\Gamma})$는 normal distribution안에서 가장 anomal image와 비슷한 이미지이다. 그래서 두 이미지의 차이는 abnormal region에 해당한다.

 <p align="center">
 <img src="/assets/images/AnoGAN/Figure3.png" width="500" height="275"/>
 </p>

 첫번째 row는 real image(test image), 두번째 row는 $G(z_{\Gamma})$, 세번째 row는 anomaly score를 real image에 overlay 한것이다. 네번째 row는 anomaly score중에서 threshold를 넘은 값들을 visualizing한 것이다.

 <p align="center">
 <img src="/assets/images/AnoGAN/Figure4.png" width="350" height="350"/>
 </p>

 GAN이라 성능이 잘안나올 것 같지만 의외로 잘나온다고 한다. 

[^1]: [Arxiv: Generative Adversarial Networks](https://arxiv.org/abs/1406.2661)  
[^2]: [Arxiv: Unsupervised Representation Learning with Deep Convolutional Generative Adversarial Networks](https://arxiv.org/abs/1511.06434)