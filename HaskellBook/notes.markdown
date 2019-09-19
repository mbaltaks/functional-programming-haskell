# Haskell Book

## Chapter 1

### Equivalence Exercises

1. 𝜆𝑥𝑦.𝑥𝑧
   Answer b) 𝜆𝑚𝑛.𝑚𝑧

2. 𝜆𝑥𝑦.𝑥𝑥𝑦
   Answer c) 𝜆𝑎.(𝜆𝑏.𝑎𝑎𝑏)

3. 𝜆𝑥𝑦𝑧.𝑧𝑥
   Answer b) 𝜆𝑡𝑜𝑠.𝑠𝑡

Checked, these are correct, phew.

### Combinators

1. 𝜆𝑥.𝑥𝑥𝑥 - is a combinator

2. 𝜆𝑥𝑦.𝑧𝑥 - not a combinator

3. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥) - is a combinator

4. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥𝑦) - is a combinator

5. 𝜆𝑥𝑦.𝑥𝑦(𝑧𝑥𝑦) - not a combinator

All correct.

### Normal form or diverge?

1. 𝜆𝑥.𝑥𝑥𝑥 - will diverge. No. 𝜆𝑥.𝑥𝑥𝑥 doesn’t diverge, has no further reduction steps. If it had been applied to itself, it would diverge, but by itself does not as it is already in normal form.

2. (𝜆𝑧.𝑧𝑧)(𝜆𝑦.𝑦𝑦) - will diverge

3. (𝜆𝑥.𝑥𝑥𝑥)𝑧 - will converge to 𝑧𝑧𝑧

One wrong.

### Beta reduce

1. (𝜆𝑎𝑏𝑐.𝑐𝑏𝑎)𝑧𝑧(𝜆𝑤𝑣.𝑤)
   (𝜆[𝑎:=𝑧][𝑏:=𝑧][𝑐:=(𝜆𝑤𝑣.𝑤)].𝑐𝑏𝑎)
   (𝜆𝑤𝑣.𝑤)𝑧𝑧
   (𝜆[𝑤:=𝑧][𝑣:=𝑧].𝑤)
   𝑧

2. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑎.𝑎)𝑏
   (𝜆[𝑥:=(𝜆𝑎.𝑎)].𝜆[𝑦:=𝑏].𝑥𝑦𝑦)
   (𝜆[𝑦:=𝑏].(𝜆𝑎.𝑎)𝑦𝑦)
   (𝜆𝑎.𝑎)𝑏𝑏
   (𝜆[𝑎:=𝑏𝑏].𝑎)
   𝑏𝑏

3. (𝜆𝑦.𝑦)(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)
   (𝜆[𝑦=:(𝜆𝑥.𝑥𝑥)].𝑦)(𝜆𝑧.𝑧𝑞)
   (𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)
   (𝜆[𝑥:=(𝜆𝑧.𝑧𝑞)].𝑥𝑥)
   (𝜆𝑧.𝑧𝑞)(𝜆𝑧.𝑧𝑞)
   (𝜆[𝑧:=(𝜆𝑧.𝑧𝑞)].𝑧𝑞)
   (𝜆𝑧.𝑧𝑞)𝑞
   (𝜆[𝑧:=𝑞].𝑧𝑞)
   𝑞𝑞

4. (𝜆𝑧.𝑧)(𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦)
   (𝜆[𝑧:=(𝜆𝑧.𝑧𝑧)].𝑧)(𝜆𝑧.𝑧𝑦)
   (𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦)
   (𝜆[𝑧:=(𝜆𝑧.𝑧𝑦)].𝑧𝑧)
   (𝜆𝑧.𝑧𝑦)(𝜆𝑧.𝑧𝑦)
   (𝜆[𝑧:=(𝜆𝑧.𝑧𝑦)].𝑧𝑦)
   (𝜆𝑧.𝑧𝑦)𝑦
   (𝜆[𝑧:=𝑦].𝑧𝑦)
   𝑦𝑦

5. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑦.𝑦)𝑦
   (𝜆[𝑥:=(𝜆𝑦.𝑦)].𝜆𝑦.𝑥𝑦𝑦)𝑦
   (𝜆𝑦.(𝜆𝑦.𝑦)𝑦𝑦)𝑦
   (𝜆𝑦.𝑦𝑦)𝑦
   𝑦𝑦

6. (𝜆𝑎.𝑎𝑎)(𝜆𝑏.𝑏𝑎)𝑐
   (𝜆[𝑎:=(𝜆𝑏.𝑏𝑎)].𝑎𝑎)𝑐
   ((𝜆𝑏.𝑏𝑎)(𝜆𝑏.𝑏𝑎))𝑐
   ((𝜆[𝑏:=(𝜆𝑏.𝑏𝑎)].𝑏𝑎))𝑐
   ((𝜆𝑏.𝑏𝑎)𝑎)𝑐
   ((𝜆[𝑏:=𝑎].𝑏𝑎))𝑐
   𝑎𝑎𝑐

7. (𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧)(𝜆𝑥.𝑎)
   (𝜆𝑥𝑦𝑧1.𝑥𝑧1(𝑦𝑧1))(𝜆𝑥.𝑧2)(𝜆𝑥.𝑎)
   (𝜆𝑥.𝜆𝑦.𝜆𝑧1.𝑥𝑧1(𝑦𝑧1))(𝜆𝑥.𝑧2)(𝜆𝑥.𝑎)
   (𝜆[𝑥:=(𝜆𝑥.𝑧2)].𝜆𝑦.𝜆𝑧1.𝑥𝑧1(𝑦𝑧1))(𝜆𝑥.𝑎)
   (𝜆𝑦.𝜆𝑧1.(𝜆𝑥.𝑧2)𝑧1(𝑦𝑧1))(𝜆𝑥.𝑎)
   (𝜆𝑦.𝜆𝑧1.𝑧2(𝑦𝑧1))(𝜆𝑥.𝑎)
   (𝜆[𝑦:=(𝜆𝑥.𝑎)].𝜆𝑧1.𝑧2(𝑦𝑧1))
   𝜆𝑧1.𝑧2((𝜆𝑥.𝑎)𝑧1)
   𝜆𝑧1.𝑧2(𝑎)
   𝑧2

Had to revisit answers as I went through - 2 and 5, but having seen my mistake, went back and worked through again to get the correct answer. With 7 I've just stopped because of diminishing returns.

## Chapter 2

### 2.5 Exercises

1. let half x = x / 2 (only if GHCi before 8.0.1)

2. areaOfCircleWithRadius radius = 3.14 _ (radius _ radius)

3. areaOfCircleWithRadius radius = pi _ (radius _ radius)
