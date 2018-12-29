---
-layout: post
-date:   2018-11-24 17:00:35 +0900
-tags: Machine Translation
---

 
 
<!--more-->

---

## Introduction
 
 RNN은 매 타임스텝마다, $h_{t-1}$와 input으로 time $t$의 hidden state $h_t$를 생산한다. 이 연속적인 성질은 training을 병렬화하지 못하게 한다는 단점이 있다. 이 단점은 sequence length가 커질때 더욱더 심각해진다.

 Attention mechanism은 input과 output의 distance의 영향을 받지 않고 modeling을 할수 있게 한다. 그래서 sequence적인 task에서 통합기능으로서 활용한다.

 그래서 위의 단점을 보완한 transformer를 제안한다. model archtiecture는 recurrence를 피하고, input과 output의 global dependency를 attention으로 해결한다.

## Model Architecture

 대부분의 sequence한 변환 모델은 encoder-decoder 구조를 가지고 있다. encoder는 input sequence를 연속적인 data space에 mapping하고 decoder는 output sequence로 해독한다. 

 self-attention과 point-wise를 쌓아 fully connected layer를 encoder와 decoder에 동시에 사용한다. 

 ![Fig. 1]

### Encoder

 Encoder는 동일한 layer를 $N = 6$개 쌓았다. 각 layer는 두개의 sub-layer로 이루어져있다. 하나는 multi-head self-attention mechanism이고, 나머지 하나는 간단한 position wise fully connected layer이다.