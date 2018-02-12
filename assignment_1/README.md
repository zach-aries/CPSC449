# CPSC 449 Assignment 1

## Compile and Run instructions

To compile in terminal, run from the project root dir:
**$ ghc -o assign2 src/assign1.hs **

To run from terminal execute the command:
**./assign1**

## Assignment Overview

### Sample Solution Length:
• Less than 100 lines to reach the A- level, including some comments
• Approximately 130 lines with the fill color being influenced by the position within the image,
including some comments

### Background:
Piet Mondrian (March 7, 1872 – February 1, 1944) was a Dutch painter who created numerous famous
paintings in the early half of the previous century that consisted of a white background, prominent black
horizontal and vertical lines, and regions colored with red, yellow and blue.

### Assignment Task:
Your task is to write a Haskell program that uses recursion to generate pseudo-random “art” in a
Mondrian style. Your program’s output will be an HTML document that contains rectangle primitives
(perhaps among others) within an SVG tag. The following general strategy will be used to generate art
in a Mondrian style:

If the region is wider than half the initial canvas size and the region is taller than half the initial
canvas height:
> Use recursion to split the region into 4 smaller regions (a vertical split and a horizontal split) with both split locations chosen randomly.
> Else if the region is wider than half the initial canvas size:
>>  Use recursion to split the region into 2 smaller regions using a vertical line with the split location chosen randomly.
> Else if the region is taller than half the initial canvas size:
> > Use recursion to split the region into 2 smaller regions using a horizontal line with the split location chosen randomly.
> Else if the region is big enough to split both horizontally and vertically, and both a horizontal and vertical split are randomly selected:
> > Use recursion to split the region into 4 smaller regions (a vertical split and a horizontal split) with both split locations chosen randomly.
> Else if the region is wide enough to split horizontally, and a horizontal split is randomly selected:
> > Use recursion to split the region into 2 smaller regions using a vertical line with the split location chosen randomly.
> Else if the region is tall enough to split vertically, a vertical split is randomly selected:
> > Use recursion to split the region into 2 smaller regions using a horizontal line with the split location chosen randomly.
> Else:
> > Fill the current region (randomly, either white or colored, and if colored, with a random determination of red, blue or yellow).

Use the following strategy when randomly deciding whether or not to split a region:
> Generate a random integer between 120 and the width of the region * 1.5.
> If the random integer is less than the width of the region then split the region.
> While this strategy works, you might find yourself asking: Why is the random number between 120 and the width of the region * 1.5? By using 120 as the lower bound for the random number, we ensure that we never split a region that is less than 120 pixels wide (or tall when splitting in the other direction), and as such, we meet the constraint that the region is big enough to split (for my arbitrary definition of big enough). Selecting a random value that could be up to 1.5 * the width of the region, but then only performing a split when the random value is less than the width of the region, provides a random chance that a larger region will not be split into smaller regions.
> Use the following strategy when splitting a region, either because it is so big that it will always get split, or because it was randomly selected to be split:
> Choose the split point, randomly, somewhere between 33% and 67% across the region (or down the region if splitting in the other direction). Choose two random split points when splitting both horizontally and vertically.
> Split the region into two smaller regions, one on the left and one on the right (or one on top and one on the bottom), or four smaller regions if splitting both horizontally and vertically
> Use recursion to fill / further split each new region
> Use the following strategy to decide which color will be used to fill a region that will not be split further:
> Select a random value, r
> > If r < 0.0833 then fill the region with red
> > Else if r < 0.1667 then fill the region with skyblue
> > Else if r < 0.25 then fill the region with yellow
> > Else fill the region with white
