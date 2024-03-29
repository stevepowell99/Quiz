# 📚 Creating good factor labels

```{r,echo=F}
knitr::include_url("https://player.vimeo.com/video/580212681")
```


Remember that in general, factor names are case sensitive. So the app will not treat 'funding' and 'Funding' as the same.

You can use emojis 😍 and other special characters in your factor labels.

Also, semi-colons ‘;’ you can use them if you want, but note that the app will then treat this factor label as [hierarchical](#xhierarchical-coding).


## Actor-focused labels

Often you will find that it is easier and clearer if you formulate your labels to highlight who does what. So a formulation like "advocacy on renewables" might be better as "NGOs conduct advocacy campaign on renewables". If you do this, you may even find that you want to break this up into different actors, for example "**NGOs** conduct advocacy campaign on renewables" and "**Politicians** pay more attention to renewables". In particular, avoid using passive formulations like "Advocacy is carried out" or "Awareness is raised".

## Formulating factors as "semi-quantitative"



### Relax! Use heterogeneous, “in-vivo” factor labels

It might be tempting to try to formulate all factor labels in a strictly similar way, using for example language like “increased probability of …” or “positive change in …” in every case. But it is difficult to identify and agree on a satisfactory template for doing this which will capture enough of the way people really make causal explanations (in the way that quantitative social scientists hope to measure everything just with continuous variables). This is always a balancing act, but we encourage you when in doubt to stick fairly close to the actual language your sources use (so-called “in-vivo” coding), and don’t be *too* worried if your factor labels are different from one another grammatically (e.g. some express a difference like “improvement in X” and some do not.

The formulation of **factor labels** should fit the intended interpretation of the **causal links**. For example, most commonly B ➜ E is supposed to mean that B exerts in some sense an “increasing” or “decreasing” influence on E, then both B and E need to be formulated in a corresponding way. In order to ease interpretation, with a few exceptions, factors should be labelled and understood in such a way that it makes sense to say “more of this” or “this happened as opposed to not happening”: we call these semi-quantitative factors.

Consequently you should avoid a factor label like Training courses, which might be understood as a mixed bag of various causal factors to do with training courses. We would usually prefer a label such as Training courses delivered or Quality of training courses which are easier to understand as things which can increase or decrease, or happen or not happen. You may even prefer to use labels like Quality of training courses improved or Improved quality of training courses, in which the *difference made* is already included in the title.

### QuIP specific: back-chaining

In QuIP projects, most of the interview material comes in response to questions about changes in the reference period, usually the last three years. QuIP questioning continues back up the causal chain, asking what was the reason for that? … and the reason for that? That means that most influence factors will also be expressed as changes or differences (a change in F is explained by a change in E which is explained by a change in D….), whereas some are not (e.g. “unemployment”). Some analysts will try to avoid coding this kind of claim (can something which has been around a long time explain a change in the last three years?) but you may decide that this distinction does not matter too much. If something really does describe a *change* (e.g. “became unemployed”) then it should be coded.

Most QuIP testimony begins with questions about whether things have got better or worse, so most causal factors, going back up the causal chain, are likely to be semi-quantitative too.

### Examples of semi-quantitative factors

These are examples of factor labels where you can judge whether it happened more or less, whether it is higher or lower, or whether it happened versus not happened:

- Sold cow
- Earthquake happened
- (Had) good harvest
- (Level of) bank account
- (Level of) ethnic tolerance
- Quality of seeds

In some contexts, we can also talk about the *likelihood* of events, so “if people get a good harvest they are less likely to sell their cow.”

It is also perfectly acceptable and sometimes necessary to use purely qualitative labels, e.g. coping style, [see below](https://guide.causalmap.app/creating.html#examples-of-non-quantitative-factors). However, this may limit some of the analysis and reporting tools available.

## Opposed pairs of causal factors

What to do when some explanations use a causal factor phrased in a positive way and others use a similar causal factor but phrased the other way around?

“I feel good because my health is good.”

“My sister lost her job because her health is bad.”

We could code these:

Health improved ➜ Feel good

Health got worse ➜ Lost job

But we might feel we are missing the fact that the first factor in each case is arguably the same thing, just the other way around.

Solution 1: do nothing

This may mean you find yourself using pairs of opposing factors such as Better health and Worse health to capture the causal claims – things which are in a sense the opposite of one another. You might decide that is fine anyway, because you are happy to have pairs of factors like this, or because you decide that the pair of factors are not really polar opposites at all and therefore you don’t want to combine them. For example, illness is arguably not really simply the opposite of health but a quite different state with its own causal rules.

Solution 2: merge them

If you use more advanced coding styles involving “strength,” detailed in the “Extra” section of this Guide, you can avoid this by just using one factor like Better health.

Alternatively, ‘Health Improved’ and ‘Health got Worse’ could be coded as ‘Health Improved’ and ‘~Health Improved’ respectively, with ‘~’ indicating the ‘opposite’ of an improvement in health. The app will then treat links to Health Improved and ~Health Improved separately. [More information on this can be found here.](https://guide.causalmap.app/coding-opposites.html#combining-opposites)

## Formulating factors as desirable

If possible, especially when conducting evaluations or research for policy purposes, try to formulate these semi-quantitative factors so that for each one, *more* of it can be broadly thought of as *more desirable*, e.g. Health, Wellbeing; and failing that, as something *undesirable*, e.g. Psychosocial Stress, Mortality. Including factors which primarily code something undesirable as well as factors which primarily code something desirable, is often very natural, but you might sometimes have to think more carefully when making reports and summaries.

Again, it is least useful, but also perfectly acceptable and sometimes necessary, to fall back to using factor labels which are ambiguous as to their desirability, e.g. Moved house*,* if there is a lack of information or in the case of factors which simply are neutral.

## Examples of non-quantitative factors

These are examples of factors where it is very hard to quantify the occurrence in any way:

- Teaching style
- Coping strategy
- The content of the report

We call these non-quantitative factors. We might try to reword them as semi-quantitative factors like this:

- Helpfulness of teaching style
- Positiveness of report

The diagram below is an acceptable causal map, even though the intervening two variables are an example of more qualitative rather than (semi-)quantitative labels. It tells us that the training did something to the teaching style, which did something relevant to the learning style, which improved student outcomes. We don’t have a theory about the “how” of any of the arrows, and we don’t (necessarily) suggest that teaching style or learning style are things which can be measured on a single scale from e.g. “less” to “more.”



## Using flags in factor labels

Hierarchical coding is one way to structure your coding. However, sometimes you don't want to think in terms of a strict hierarchy, or maybe you have an additional set of themes which cut across that hierarchy.

````{r,echo=F}
knitr::include_url("https://player.vimeo.com/video/671894620")

````

Flags are useful in either of these cases.

Flags are just sequences of characters within a factor label to which you have given a special meaning, and which are unique and easy to search for. These can include letters, emojis or phrases. You can do coding without any such flags if you want, but it can help when searching and filtering.

A quote like “family situation is better now because of improved food availability” can be coded like this:

> More food --> Improved wellbeing

Now, maybe you are asked also to keep track of any aspects of the project which have to do with nutrition. Nutrition is not really part of your system of factors, but you would like to be able to construct some maps just to look at this aspect. So you can write this:

> More food #nutrition --> Improved wellbeing  

Similarly,  if Improved Wellbeing is one of the desired outcomes of the project, we might want to reflect that by adding a flag "(Outcome)" like this.

> More food --> Improved wellbeing (Outcome)

Then we can easily search for this and other desired outcomes.  

A flag like “men” is not suitable because it is likely to appear elsewhere (e.g. as part of “women” or “management”). To get round this, add additional characters like a hash: “#men”; this makes the flag unique.

If you use curved or square brackets around your flags, you can use one of the app filters to hide the flags for specific maps if desired.

### Hanging flags{#xhanging-flags}

A subtle trick is to use hanging flags which are additional hierarchical components. So rather than writing, say,

>More food #nutrition --> Improved wellbeing

you write

>More food; #nutrition --> Improved wellbeing

(note the `;` ), then  if you "zoom out" to the top level of the hierarchy, the flag will be neatly hidden:

>More food --> Improved wellbeing

### Flags for bundling factors

It is very convenient to use flags when you want to [bundle factors](#bundlefactors). This means you can easily collapse together one or more sets of factors sharing a common theme, regardless and independently of any hierarchical structure you are using.




### Intervention Flags

The commissioner of an evaluation will nearly always want to be able to focus on their own interventions, so it is advisable to use a consistent label or category across all factors which are noted as project inputs in order to be easily searchable. These will usually be influence factors with no parents, i.e. factors which have no incoming influence factors.

In the example below, both are similar interventions, but one is clearly attributable to an NGO.

“MSF gave us medicines which healed the children”

MSF gave medicines –> children healed

“Someone gave us medicines which healed the children”

Unknown actor gave medicines –> children healed

You have a range of options to flag this difference in the label and enable you to filter for all project-related interventions once the data is coded.

· All such interventions can be labelled with the NGO’s name

· If you are doing nested coding, you can nest such factors within a higher-level factor like ‘Interventions;’

· And/or you can use a specific intervention flag, e.g. #Intervention. (QuIP has its own flags, [I] for Implicit and [E] for Explicit).

You can use any label you want as long as you are sure it is unique, it doesn’t appear accidentally in other factor names, and you use it consistently.

### Outcome Flags

Similarly, if you wish to flag up outcomes /consequences as either desired/desirable/valued, or the inverse, you can mark these in a way which makes them easy to identify. You could use symbols like ♥ etc. in the labels to designate this, or use use QuIP-style labels [P] for positive and [N] for negative. This can be helpful when undertaking analysis; for example, displaying direct and indirect links from some or all of the intervention factors to some or all of the positive outcome factors or highlighting or counting paths leading to all negative outcome factors, etc.

Again, you can use any label you want as long as you are sure it is unique, it doesn’t appear accidentally in other factor names, and you use it consistently.

### Coding your statements in context: referring to evidence from elsewhere

When coding data from an interview it is very important to see the whole interview as your source, and not fall into the trap of only coding the statement in front you, out of context. If your statements are split into separate questions (as is often the case with QuIP), it can be misleading to code just one statement in isolation, without taking account of what the respondent says earlier or later in the interview. For this reason we recommend that all analysts read the whole interview before starting to code, and start to formulate causal stories which fit across the whole interview. In some cases, you may prefer to have the whole interview as one statement, and to code it all in one place. This is absolutely possible, and just depends how the data is imported (whatever information is on one row in the .csv file is considered a statement). If, however, the interview is too long, or you would still prefer to code question by question, you need may want to refer to information provided in another statement when coding a link. This is not necessary but can be useful when sharing the file with a commissioner who may look at quotes associated with links, and not understand the rationale for coding without further context. The same applies for different parts of a paragraphs within one statement; you may want to highlight two separate parts of a longer statement.

If the quote is not exactly verbatim in the statement, the app won’t let you save anything. (If this happens, just cancel the quote (press the X) and start again.) The app is really picky about this to ensure your quotes are accurate. However, this rule does not apply for anything between square brackets, a useful feature for this case. If the evidence for a link is mentioned in two different parts of a single statement, like this:

*Yes, we got the help from OrgX. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. And because of that help, now I have a job. qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit. Ut enim ad minima veniam.*

You can use square brackets to remove the text you don’t want. Highlight the whole piece of text you want to code, and then edit the quote in the quote window, replacing what you don’t need with […]

*Yes, we got the help from OrgX. […] And because of that help, now I have a job.*

Be careful not to introduce any new text or even spaces.

You can also use the same technique to add your own comment or quote from a statement elsewhere in the interview:

*[this is my note] Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt.*

Ellipses are also helpful to refer to another statement as part of your evidence:

*[reference to statement 36: The whole village received help from Org X] Thanks to that help we received, we are now growing our own produce.*

## Consolidating and editing factor labels

After you have created more than about 50 factors you will probably feel the need to start consolidating and renaming them and perhaps merging some.

If you look at the Mentions table in the app, you will probably find a lot of *pairs* of factors each of which is used only once, as half of this particular pair, and don’t appear linked into longer stories. We almost certainly will want to consolidate some of these factors by in some way merging them with others which have similar meaning.

Suppose you have 100 factors which are just mentioned once or twice. It’s a lot of work to do all that coding, a lot of work for almost nothing, because those very infrequent factors (we call them “tiddlers”) are unlikely to appear much in any reporting you do. With hierarchical coding, even infrequent factors can contribute to the bigger picture.

Before we even think about consolidating factors, do remember the golden rule that, **when coding a new link, always look at your existing factors and try selecting one of those before creating a new one.** Even if you think an existing factor isn’t quite a perfect fit, you can use it and adjust the factor name later. Ideally you will always be able to think of labels for your factors which are at least general enough to cover a few different cases.

It can be helpful to reformulate factor labels so that some common themes come *first*:

- “communication difficulties - intercultural”
- “communication difficulties - lack of internet”

This is useful when the factors are listed alphabetically and can be a stepping-stone to formulating factors hierarchically.

Even when doing inductive coding, some people formulate factors with a theory in mind, with some higher-level factors like “communication difficulties” or “resilient outcomes.” However we have found that it is often most interesting when you let the theory emerge rather than by starting with a preconceived template in this way. Look at your factors periodically as you code and see if there are any obvious groups.

Also, our recommended way of having your cake and eating it - keeping the detail but seeing the general trends too – is “hierarchical coding.” [See below](https://guide.causalmap.app/creating.html#simplifying-causal-maps-with-hierarchical-coding).

### Merging multiple factors into one

In some cases you will simply decide, in the face of too many factors, to merge `Engagement with communities` and `NGO engagement`into one, e.g. `Engagement`.

If you have two factors like this which are very similar but not quite the same, you can sometimes keep some detail by using a composite label which explicitly mentions both i.e.`Engagement with communities/NGOs`.
