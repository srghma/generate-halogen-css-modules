# How it works

having in `/home/srghma/projects/purescript-halogen-nextjs/app/Nextjs/Pages/Buttons/CSS.module.css`

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
```

With command `generate-halogen-css-modules --directory /home/srghma/projects/purescript-halogen-nextjs/app`

it will generate 2 files

1. `/home/srghma/projects/purescript-halogen-nextjs/app/Nextjs/Pages/Buttons/CSS.js`

```js
exports.styles = require('./CSS.module.css')
```

2. `/home/srghma/projects/purescript-halogen-nextjs/app/Nextjs/Pages/Buttons/CSS.purs`

```purs
module Nextjs.Pages.Buttons.CSS (styles) where

import Halogen.HTML (ClassName)

foreign import styles ::
  { myButton :: ClassName
  , myButton2 :: ClassName
  }
```
