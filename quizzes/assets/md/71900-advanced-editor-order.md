# Using the Advanced Editor to create and edit filters{#advanced-editor}

![NsNieBjXgE](_assets/NsNieBjXgE.gif){width=650}

The commands are applied one by one in sequence, in a pipeline of commands, such that after each command, such as each command starts with the map defined by the previous line and produces a new one.

The order of the lines may matter! You can move lines up and down by pressing Alt+UpArrow or Alt+DownArrow.

This text can also be freely edited and typed independently of using the buttons. You don't need to fear doing anything wrong because there's nothing to break and commands which don't fit the intended syntax patterns will just be ignored.

When you add a filter using the buttons, the filter replaces any similar filters already in the text window. Occasionally, you might want two different versions of the same filter. For example, you might want to filter using `find statements field=text value=women operator=contains` and also `find statements field=#SourceID value=mkf-8 operator=contains`. In this case, uncheck `Replace existing similar filters?` before applying the filter.

The operators work as you'd expect, so you can for example have a filter like `find statements field=text value=women OR girls operator=notcontains` to find statements whose text does not contain either 'women' or 'girls'.

To get an AND rather than an OR, just put one filter after another. To find statements containing women AND girls:

> find statements field=text value=women operator=contains

> find statements field=text value=girls operator=contains

Read about them all at: [CausalMapFunctions](https://stevepowell99.github.io/CausalMapFunctions/reference/index.html).