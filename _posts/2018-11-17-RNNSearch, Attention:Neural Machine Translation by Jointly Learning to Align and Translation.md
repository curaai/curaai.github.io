---
-layout: post
-date:   2018-11-17 17:00:35 +0900
-tags: Machine Translation
---

 Attention Mechanism이 처음으로 소개된 논문이다. 논문 내에서는 attention대신 soft-alignment라는 용어를 통해 mechanism을 설명한다. 기계번역에서 attention을 이용해 network가 문장의 alignment를 학습하여, 기존의 기계번역보다 더 좋은 성능을 보여준다. 오늘은 Attention mechanism을 살펴보자.

<!--more-->

---

## Annotation

 이 포스트와 논문에서 사용하는 용어들을 정리하고 시작하자.
 - **Soft-alignment(attention)**: source word => target의 alignment를 학습하는 방법이다.
 - **Hard-alignment**: source sentence의 word를 label값을 통해 지정한다. *Ex)'I am hungry.', '나는 배고프다.'를 I=>나, am => 는, hungry => 배고프다*
 - **context vector $c$**: 문장의 context(의미)를 압축한 fixed length vector이다.
 - **Annotation**: Encoder의 hidden state를 $annotation$이라고 부른다. 수식에선 $h$로 표현한다.
 - **hidden state**: 언급 없이 hidden state라고 나와있을 경우 Decoder의 hidden state를 말하며 수식에서는 $s$로 표현한다.
 - **source sentence $x_t$, target sentence $y_t$**: 길이가 $t$인 input, target sequence이다. 여기서 sequence는 word vector의 sequence를 말한다. $T_x$는 $x$의 길이, $T_y$는 $y$의 길이이다.

 $$
 \textbf x= (x_1, \cdots, x_{T_x})
 $$

## Background

 딥러닝 기계번역은 방식은 $x$가 주어졌을 때, **Neural Network(Model)**로 conditional probability를 최대화하는 $y$를 찾는다. Model은 Encoder-Decoder 방식에 base를 두고 있다. Encoder로 입력 문작을 context vector $c$로 압축하고,  Decoder는 $c$와 이전의 hidden state로 다른 언어로 번역(변환)한다. RNN, LSTM, GRU 등등 다양한 RNN Cell이 있는데, RNN을 사용할 경우 기존의 방식보다 이점이 많다. 

 1. input & output length에 제한이 없다. 
 2. sequential한 data에 대해 순서(alignment)를 학습할 수 있다. => *단어를 1:1번역하여 문장을 만들지 않고. 문장 전체를 읽고, 각 단어의 의미를 파악하여 다른언어로 변환한다.* 
 3. 예외 상황에 강하다. => *dictionary을 이용한 알고리즘은 dictionary 외의 단어나 입력이 들어오면 정확도가 떨어지지만, 딥러닝의 경우 Dataset의 분포가 다양하다면 작은 예외 상황에 대해서는 유연하다.*

 Basic RNN Encoder-Decoder는 아래 수식으로 나타낼 수 있다.

 $$
 \begin{align}
  h_t = f(x_t, h_{t-1}) \label{eq:1} \tag{2} \\
  c = q(\{h_1, \cdots, h_{T_x}\}) \label{eq:2} \tag 3 \\
  p(y_t | \{ y_1, \cdots, y_{t-1}\}, c) = g(y_{t-1}, s_t, c) \label{eq:3} \tag 4 \\
  p(\textbf {y}) = \prod_{t=1}^T p(y_t | \{ y_1, \cdots, y_{t-1}\}, c) \label{eq:4} \tag 5 \\
 \end{align}
 $$

 (1),(2)는 Encoder part, (3),(4)는 Decoder part다.

 1. nonlinear function인 LSTM을 $f$로 사용한다. $t-1$번째 annotation과 input으로 현재 annotation을 구한다.
 2. Annotations sequence(*각각의 annotation $h_t$는 $x_t$의 정보를 가지고 있기 때문에 정보가 encoding된 sequence라고 봐도 무방하다.*)는 $q$함수를 사용하여 fixed length vector $c$로 압축한다. *여기서는 마지막 annotation, $h_T$가 $c$가 된다.*
 3. ($c$, $y_{t-1}$, $s_t$)으로 다음으로 올 단어의 확률을 예측한다.
 4. 1부터 $T$까지 1~3을 반복하면 문장하나를 번역할 수 있다.

 <p align="center">
 <img src="/assets/images/RNNSearch/Related1.png" width="700" height="450"/>
 </p>
 
 수식과정을 그림으로 나타내면 위와 같다.

## Attention, Learning to Align
 
 아래의 그림은 위 base model에서 조금 확장한 모델이다. Encoder의 annotation, Decoder의 context vector를 압축하는 부분이 조금다르다. 이 모델을 RNNSearch라고 부른다.
 <p align="center">
 <img src="/assets/images/RNNSearch/Figure1.png" width="300" height="550"/>
 </p>

### Encoder
 
 언어 모델에서는 한 단어의 의미가 문맥에 따라 달라질 수 있다. 또한 번역하는 과정에서, 어순이 다른 경우도 많다. 일반적인 RNN에서는 과거의 $h$만 이용한다. 하지만 우리는 과거의 단어뿐만 아니라 미래의 단어도 사용해야 한다. 그래서 **Bidirectional RNN**을 사용한다. 
 
 Bidirectional RNN은 $1 -> T$방향의 RNN과 $T -> 1$방향의 RNN을 합친(concatenate) 구조다. 그래서 Encoder의 output은 $h_t = [\overrightarrow{h}_t; \overleftarrow{h}_t]$로 일반적인 RNN output의 2배가 된다.

#### Decoder

 Decoder는 context vector $c$와 이전의 hidden state $s_{t-1}$로 번역된 단어 $y_t$를 예측한다. 여기서 attention을 이용해서 input의 alignment까지 학습을 한다(여기서 alignment를 학습한다는 의미는 $t$번째 단어를 예측할 때, encoding된 단어중 중요한 단어의 정보를 더 활용한다는 것이다). 
 그래서 annotation을 만들때 단순히 마지막 annotation을 가지고 오는 것이 아니라, 1부터 T까지의 annotation을 전부 합친다. 
 
 $$
 c_i = \sum_{j=1}^{T_X} \alpha _{ij}h_j 
 $$

 어떻게 합치냐 하면은 합치는 과정에서 atttenion mechanism이 들어간다. 각 $h_j$을 attention 가중치 $\alpha_{ij}$와 weight sum해서 context vector $c$를 만든다. _이때 $i$는 Decoder time stamp를 말하고,$j$는 Annotation time stamp를 말한다._ 가중치 $\alpha$는 어떻게 만드냐!

 $$
 \alpha_{ij} = \frac{exp(e_{ij})}{\sum_{k=1}^{T_x}exp(e_ik)}
 $$ 
 
 $\alpha$는 time stamp $i$번째의 energy의 softmax로 구한다. energy는 이전 $s_{i-1}$와 현재 $h_j$로 구할수 있다. 

 $$
 e_{ij} = a(s_{i-1}, h_j) = v_{a}^{\top} tanh(W_a s_{i-1} + U_a h_j)
 $$

 energy는 function $a$로 만들어지며, 이때 Weight Matrix $v_a \in \mathbb{R}^n$, $W_a \in \mathbb{R}^{n \times n}$, $U_a \in \mathbb{R}^{n \times 2n}$로 생성된다. 

 그래서 각 energy는 $s_i$에 대한 $h_j$의 정보량을 나타낸다. $i$번째 단어를 예측할 때 encoding시 어느 단어가 중요한지 가중치 $\alpha$로 나타내 annotation과 곱해준다. 그렇게 만들어진 context vector는 중요한 단어의 값은 크고, 중요하지 않은 단어는 값이 적게 들어가 있을것이다. 이 과정을 label없이 네트워크 스스로 학습하여 판단한다.  
 
 예로
 > '나는 라면을 먹는다.' => 'I eat ramen' 일때, ramen을 예측한다면,
 >
 > context vector $c$ = 0.1($\alpha_1$) * ([나는]의 hidden state) + 0.7($\alpha_2$) * ([라면을]의 hidden state) + 0.2($\alpha_3$) * ([먹는다]의 hidden state)
 
 과 같이 나타낼 수 있다.

## Analytics

 attention, soft-alignment의 장점은
 1. 사람의 word2word labelling이 필요없다.
 2. matrix이기 때문에 네트워크로서 학습할 수 있다.(모델의 component로서 추가적인 처리가 필요없다. gradient를 공유한다.)
 3. 언어모델에 대해 적합한 특성을 가지고 있다.
   > Soft-alignment가 Hard-alignment와 다르게 좋은점은, 언어는 유연하다는 점 때문이다. 예를들어 한국어와 영어를 예를 들어보자면  "My grandfather have dinner."이 문장하나를 한국어로 바꿔보면
   >
   >- "우리 할아버지가 저녁을 먹는다"
   >- "우리 할아버지께서 저녁을 먹는다"
   >- "우리 할아버지가 저녁을 드신다"
   >- "우리 할아버지가 밥을 먹는다"
   >- "우리 할아버지가 진지를 드신다"
   >- "할아버지가 저녁을 먹는다"
   >
   >등 언어별로 문법이나 번역 형태가 다르다. network는 예외 상황에 대해 스스로 학습한다. 또한 x와 y의 길이가 달라 hard alignment가 불가능해도 soft-alignment는 가능하다.

 <p align="center">
 <img src="/assets/images/RNNSearch/Figure3.png" width="600" height="600"/>
 </p>

 그림은 attention(annotation weight $\alpha_{ij}$)을 visualization한 것이다(_0: black, 1:white_). $x$축은 source sentence(English)이고 $y$축은 예측번 번역 문장이다(French). 보다시피 matrices의 대각값들이 대부분 강한 weight를 띄고 있다. 

 (b),(c),(d)는 test set에서 sample로 뽑힌 setences이다.