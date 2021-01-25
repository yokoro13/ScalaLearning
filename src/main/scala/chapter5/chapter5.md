# 正格と遅延
ループの自動融合は，非正格性(non-strictness)を使って実現する. 遅延性(laziness)とも言える．
## 正格性と非正格性
非正格性は関数の特性．非正格関数では，その引数の1つ以上を評価しないという選択が可能．
逆に，正格関数では，引数が常に評価される．

e.g.

```scala
// 正格関数である．
def square(x: Double): Double = x * x
```

```scala
// 非正格(if はすべての引数を評価しない)
val result = if (input.isEmpty) sys.error("empty") else input
```