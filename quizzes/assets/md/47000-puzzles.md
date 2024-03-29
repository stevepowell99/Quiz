# 📚 Puzzles in causal mapping



## Distinctive Groups



- How to find if there are important supergroups of similar groups, e.g. can we put villages A and B together into one supergroup with similar submaps and villages C and D into another?

- Can we use the same idea to locate arbitrary subsets of statements (regardless of the additional data) which produce sets of maps which *differ the most*? 

- What is a suitable cut-off for a significance test of differences between the groups, given the size of the map?

- What does it mean if a small, or a large, proportion of factors are distinctive for a group? 

- What does it mean if we get many more distinctive factors for one group than for another even when they are of similar size?

- How to interpret results for groups of very dissimilar size?

## Quantity of Evidence

- What is the relationship between plus and minus QE on the one hand and the total QE on the other?

## Joining rules

**Combination rules** showing how to build up larger maps. The rules each have two parts saying (syntax): mini-maps joined in such-and-such a way also count as causal maps and (semantics): what does the composite map mean in terms of the meanings of its elements? Combination rules are not just for actually combining information, say from different sources; more fundamentally, they enable us to interpret any arbitrarily large map by using the mini-map rule and the combination rules.

The different ways of combining elementary maps into composite maps embody some interesting social science questions.

- joining     two maps on a **shared item** (what counts as the same thing? is     "hunger" according to one person's report the same as     "starvation" in another?)
- joining     two maps on a **shared consequence item** (this is surprisingly     difficult in the general case which includes problems of multi-causation     or overdetermination). Many approaches make the assumptions that different     influences can in some sense just be added together, so that the influence     of X on Z is always independent of the influence of Y on Z. But other approaches     (like perhaps QCA) specialise in combining influences which are not     independent of one another
- joining     two maps on a **shared influence item** (usually unproblematic in approaches     like QuIP and DAGs, but in Systems Dynamics, where the boxes are     containers and the arrows represent flows, this is a big issue).
- **merging     co-terminal arrows** from different maps into one (what does it mean     when we have overlapping information from different sources about the same     causal influence? how do we combine them if they agree or if they     disagree?)
- whether     or not to allow joining two mini-maps in such a way that a **loop** is     created, and if so, what does this mean? Fuzzy Cognitive Maps specialise     in these kinds of loop.

Causal map approaches have their own versions of each of these rules. Plus, individual approaches will have their own rules about how to combine any additional information which they allow for (e.g. level of trust). 

 

## Does believing that X influences Y mean you have to believe that not-X causes not-Y? Causal mapping and counterfactuals.

We can only say C causally influenced E if it made some difference to E, which means that *believing* causation might imply *believing* a counterfactual: if the Cause had not happened, the Event would not have happened[[16\]](#_ftn16). It’s important to understand that this only makes sense in the narrowest way. It might be true that these protests were one of the direct causes of that revolution, in the narrow sense that if they hadn’t happened in just the way they did, but everything else had stayed the same, then the revolution would not have happened, at least not in the way it did happen; but it certainly doesn’t mean that protests cause revolutions or that there are no other causes of revolutions. This also doesn’t mean that the truth of this counterfactual is part of a *definition* of causation. It just means you can’t say C caused E if it didn’t make any difference to E. 

We also stress that *evidence* about causation very often does not come from *evidence* about a counterfactual. Reasoning about causes, and gathering evidence about causes, are only possible as part of a whole lot of interlocking knowledge we already have about how things work.

What if someone says, it’s the white chocolate which makes this cake so delicious, and goes on to say, it’s the dark chocolate which makes this (otherwise identical) cake so delicious. If white and dark chocolate are mutually exclusive alternatives, this doesn’t make causal sense?


## Homogenity of paths

Can we think about transitivity in terms of a metric?

Scenario 1) 20 people might have said that A links to B and another different 20 might have said that B leads to C with no overlap between the groups of people. 

Scenario 2)  the same 20 people say that A links to B *and* that B leads to C.

Conventional ways to combine the information from the different sources would produce the same diagram in both scenarios. But we want the user to see that there is some kind of weakness in scenario 1. Can't we just show this on the arrows somehow?

In the case of isolated paths with no forks in them, which of course are very rare, this wouldn't present a big problem. Assume that we are showing, as we do, now the total number of mentions for each section represented by for example the width of the arrow.

We can construct a measure which we could call "homogeneity" for each section. So if in a long path, each of the sections were mentioned by more or less the same people the stretch would have high homogeneity and you could for example show it unbroken. In contrast, for an arrow in which separate sections were mentioned by different groups of sources, it would have low homogeneity and you could show that for example with a very broken arrow with lots of gaps in it. Or by making it almost transparent. 

However I don't think this works in the much more common case when paths are of course constantly diverging and rejoining. 

For example, suppose you have an arrow from B to C and then an arrow from C to X and another from C to Y; suppose there was high homogeneity from B to C to X in the sense that both of these sections were mentioned by many sources but the section from C to Y was mentioned by a different bunch of sources. How would you mark this? 

**The problem is that homogeneity as I've described it as a metric of entire paths** and so you can't really show it in individual sections when there are forks, i.e. when one section can have different homogeneities because it is part of different paths. 

## (non-) solution 1)

What one could do is see whether there is any clustering within sources rather than within variables. So you might find there is a bunch of people who tend to mention many arrows the same and another bunch of people who mention a different set of arrows. It would certainly be possible to automatically or manually create subgroups of respondents. Then there are ways to show how different sections of paths were mentioned by these different subgroups, but it doesn't really answer the question.

## Solution 2)

You can certainly report the homogeneity of each individual path in the Report tab of the app, but that would be quite long-winded.

## Solution 3) 

You could show the same information interactively. For example when you click on B in the example above, you would see the width of the section from C to Y  shrink but the section from B to Y would stay the same. However I am not a big fan of information which you can only discover by twiddling. 

## Solution 4)

You might want to summmarise the most important results in Solution 3 e.g. in a legend on the diagram, where you could (automatically) mention individual paths with particularly high or low homogeneity. You could probably develop a metric for the overall homogeneity of a whole diagram, and you might want to mention if a whole diagram had unusually low homogeneity. 

## Solution 5)

Allow the viewer to "play" the different sources one by one, if there aren't too many of them.





