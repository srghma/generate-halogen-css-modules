# having

```css
.myButton {
  color: green;
}

.myButton > a {
  color: green;
}

.myButton2 > a {
  color: green;
}

#myButton3 > a {
  color: green;
}

#myButton4 > a {
  color: green;
}

a > .myButton5 {
  color: green;
}

@media print {
  * {
    text-shadow: none !important;
    color: #000 !important;
  }

  a, a:visited { text-decoration: underline; }

  .myButton3 > a {
    color: green;
  }
}
```

# it will output css

```css
._1q4xGYDoNUp3VfaYH6iDRF {
  color: green;
}

._1q4xGYDoNUp3VfaYH6iDRF > a {
  color: green;
}

._2ixUl7vO7H0pcjZIr9XkSs > a {
  color: green;
}

#_36gXjI_ZTl27dWxUAWwE5M > a {
  color: green;
}

#sZtPfR5vu7WFxuLMC_DiC > a {
  color: green;
}

a > ._2l5rkeA8AsKK3nCxuXSNkB {
  color: green;
}

@media print {
  * {
    text-shadow: none !important;
    color: #000 !important;
  }

  a, a:visited { text-decoration: underline; }

  ._36gXjI_ZTl27dWxUAWwE5M > a {
    color: green;
  }
}
```

# and output js

```js
{
  myButton: '_1q4xGYDoNUp3VfaYH6iDRF',
  myButton2: '_2ixUl7vO7H0pcjZIr9XkSs',
  myButton3: '_36gXjI_ZTl27dWxUAWwE5M',
  myButton4: 'sZtPfR5vu7WFxuLMC_DiC',
  myButton5: '_2l5rkeA8AsKK3nCxuXSNkB'
}
```
