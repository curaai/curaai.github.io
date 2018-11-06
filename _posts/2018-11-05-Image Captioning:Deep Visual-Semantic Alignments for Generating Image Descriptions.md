---
-layout: post
-date:   2018-11-05 12:20:35 +0900
-tags: Image Captioning
---


 오늘 리뷰할 논문은 _Deep Visual-Semantic Alignments for Generating Image Descriptions_ 이다. image captioning이라는 분야로 input image에 대해 description을 만드는 것을 제시한 논문이다. 
 여기서는 이미지와 자연어를 합친 multimodal embedding image captioning을 선보인다.

<!--more-->

## Introduction
 
 기존의 image description 혹은 image captioning은 hard coding되어, variety하지 않았다. 복잡한 이미지를 한 문장 혹은 한 단어로 줄이는 것이 기존의 방식이다. 그래서 기존에는 coffee 이미지를 coffee라고 표현하는 반면 image captioning을 이용한다면 _a cup of coffee, a woman drinks coffee_ 등등 다양한 방식으로 표현가능하다.

 <p align="center">
 <img src="/assets/images/ImageCaptioning/Figure1.png" width="800" height="450"/>
 </p>

 image captioning을 재정의한다면, image의 content에 대해 충분한 설명을 NLP domain에서 표현하는 것이다. 


## Learning to align visual and language data 

 모델의 궁극적인 목표는 이미지에 대한 description을 생성해내는 것이다. 먼저, mutlibodal embedding을 통해 visual region에 대해 align sentence(간단한 문장 혹은 snippet)으로 표현한다. 그 다음 RNN으로 image description을 만들어낸다.

### Representing images

 RCNN을 사용해 이미지에서 top 19개의 box를 detection하고 이미지 전체로 총 20개를 input으로 한다. 각각의 이미지는 CNN과 embedding matrix $W_m$을 통해 $h$-dimensional vector $ \{v_i \| i = 1, \dots, 20 \} $으로 표현된다. 

 $$
 v = W_m[CNN_{\theta}(I_b)] + b_m
 $$
 
 CNN은 RCNN의 network를 가져와서 bounding box image $I_b$를 classifier 전의 4096-dimensional fully connected로 변환된다. weight matrix $W_m$는 $h \times  4096$으로 이루어져 있고, $h$는 multimodal embedding space($h$의 범위는 1000~1600)의 size이다. _CNN은 ImageNet에서 pre-trained되어, ImageNet Detection Challenge 200 class로 finetuning되었다._

### Representing sentences

 sentence의 space도 image와 맞추기 위해 단순히 projection해도 되지만, 이러면 sentence의 word와 ordering information은 손실이 크다. 그래서 Bidirectional Recurrent Neural Network를 사용한다.

 $$
 \begin{align*}
 x_t &= W_w \mathbb{I}_t \tag2 \\
 e_t &= f(W_ex_t + b_e) \tag3 \\
 h^f_t &= f(e_t + W_f h^f_{t-1} + b_f) \tag4 \\
 h^b_t &= f(e_t + W_b h^b_{t+1} + b_b) \tag5 \\
 s_t &= f(W_d(h^f_t + h^b_t) + b_d) \tag6 \\
 \end{align*}
 $$
 
 $\mathbb {I}_t$는 column vector 중 word vocabulary에서 $t$번째 단어의 index에 해당하는 값이 1로 채워져있다. W_w는 word2vec weight로서 300-dimensional이다. $h^f_t, h^b_t$는 time sequence에서 앞뒤로 움직이는 RNN이다. sentence의 각 word는hidden state s_t로 encoding되어 표현된다.

### Alignment objective 

 우리는 image와 sentence를 h-dimensional space에 mapping했으므로, image-sentence score를 각 region에 대해 만들어야 한다. 직관적으로 단어가 이미지에 대해 높은 연관성을 가지고 있다면, 높은 점수가 나와야 한다. 

 <p align="center">
 <img src="/assets/images/ImageCaptioning/Figure3.png" width="800" height="450"/>
 </p>

 20개의 이미지중 $i$번째 region에 대한 전체 문장의 $t$번째 단어의 정보량은 $v_i^{\top}$와 $s_t$의 dot product로 해석할수 있다. $v_i$는 $h$-dimensional image feature이고, $s_t$는 h-dimensional word feature이다. 

 $$
 S_{kl} = \sum_{t \in g_l} max_{i \in g_k} v_i^{\top} s_t \tag 7
 $$

 $g_k$는 이미지 $k$의 region set이고, $g_l$은 전체 sentence $l$에 대해 snippet set이다. Eqn. (7)에서 조금 변경한 것이 Eqn. (8)이다. Eqn. (8)의 목적은 이미지 region과 단어 word가 가장 큰 것을 고른다. 

 $$
 C(\theta) = \sum_k [ \sum_l max(0, S_{kl} - S_{kk} + 1) + \sum_l max(0, S_{lk} - S_{kk} + 1) ]. \tag 8
 $$

 $k = l$이 image에 해당하는 sentence pair이고, $S_{kk}$가 정답이다. 우리는 $S_kk$를 최대화 시켜 $C(\theta)$ loss를 minimize하는 것이 목표이다. 
 - $\sum_l max(0, S_{kl} - S_{kk} + 1)$ term은 sentence를 변경시키며, 이미지에 해당하는 sentence를 찾는다.
 - $\sum_l max(0, S_{lk} - S_{kk} + 1)$ term은 image를 변경하며, sentence에 해당하는 image를 찾는다. 

### Decoding text segment alignments to images 

 이미지를 학습할 떄, 각 이미지는 한장의 단어로 표현하는 것이 아니라 여러 단어로 표현할 수 있어야 한다.  latent alignment variable $a_j \in \{ 1, \dots, M\} for j = 1, \dots, N$를 Markov Random Field란 방법으로 최적화한다. M은 bounding box의 개수, N은 word의 개수이다.

 $$
 \begin{align*}
 E{\textbf {a}} &= \sum_{j = 1, \dots, N} \psi^U_j(a_j) +  \sum_{j = 1, \dots, N-1} \psi^B_j(a_j, a_{j+1})  \tag 9 \\ 
 \phi^U_j &= (a_j = t) = v^{\top}_j \tag {10} \\
 \phi^B_j(a_j, a_{j+1}) = \beta \textbf {1}[a_j = a_{j+1}] \tag {11} \\
 \end{align*}
 $$

 best alignment $a$를 찾기위해 energy를 maximize하는 방향으로 dynamic programming을 통해 학습시키며, hyper parameter $\beta$로 여러개의 단어가 같은 box를 가르키도록 학습한다. 

## Multimodal Recurrent Neural Network for generating descriptions 

 이 섹터에서는 input image의 textual description을 만들어내는 것을 알아볼 것이다. 그건 전체이미지가 될수도 있고, 이미지의 작은 패치가 될수도있다. 이 challenge의 핵심은 input image에 대해 다양한 size의 sequence가 나올 수 있다는 점이다.

 input vector $(x_1, \dots, x_T)$ sequence와 이미지 $I$로 hidden state sequence $h_1, \dots, h_t$를 예측한다. 

 $$
 \begin{align*}
 b_v &= W_{hi} [ CNN_{\theta_c}(I)] \tag {12} \\
 h_t &= f(W_{hx} x_t + W_{hh} h_{t-1} + b_h + \textbf {1}(t = 1) \odot b_v) \tag {13} \\
 y_t &= softmax(W_{oh} h_t + b_o) \tag {14} \\
 \end{align*}
 $$


### Training 

 <p align="center">
 <img src="/assets/images/ImageCaptioning/Figure4.png" width="800" height="450"/>
 </p>
 RNN network는 word $(w_t)$와 previous context $(h_{t-1})$로 다음단어 $(y_t)$를 예측하기 위해 학습한다. RNN은 iteration이 처음 시작할 때 network에 image의 정보를 준다. 이미지에 대한 backprop은 RNN의 first step의 bias term으로 학습한다.
 
 test시에는 $h_0 = 0$으로 설정하여 input으로 START vector를 준다.