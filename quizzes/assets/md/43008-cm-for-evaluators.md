---
bibliography: references.bib
---

# 📚 Causal mapping for evaluators{#mapping}

> Causal mapping: a way to understand how people think; and perhaps to understand how the world works


## Causal mapping for evaluators

A causal map is a way to organise a heap of claims about causal links between causal factors. Causal maps allow us to ask and answer questions like "what kind of effect will/did tweaking C make to E?", which is one of the central tasks of programme evaluators. We may be asked

-   if an intervention had, or could have, any effect on some desired outcome.

-   to assess other relevant but unintended consequences of the programme to assess the causal relevance of some intermediate step to some hard-to-measure goal (is this a good step? Are there better steps?)

-   whether C was *the* cause of E.

-   whether C had *some* causal influence on E.

-   what would have happened without some particular assumptions or events taking place

-   is this contribution larger than some comparison contribution

-   is this contribution more valuable than some comparison contribution

-   which are/were the important influences on this outcome

This means that as programme evaluators, we often have to deal with bundles or heaps of claims about causal influences and somehow combine them. For example, we might have some questionnaire data which suggests that an intervention (C) improved teachers' skills in the desired way (D), and we might have some research studies which suggest that this will have a positive impact on student outcomes (E); on the other hand we have an interview with a school director who insists overall that the intervention (C) is useless and did not influence outcomes (E) at all.

Occasionally we may have only one source of information from one method, such as a questionnaire or clinical trial, and we may even believe we have an algorithm which tells us how to make a judgement based on that source, but most often we will have several pieces of information expressed in more or less vague terms, and most often they have to be weighed up and combined based on our own best judgement. To be sure, we can develop some scoring algorithm to help us with that process, but we still need to choose and justify the algorithm. Occasionally an evaluation question can be reduced simply to a question about the *direct* influence of C on E, but most often, as suggested by theory-based evaluation approaches, we have to consider a *network* of causal factors which influence one another, mostly *indirectly* along the paths in the network.

In theory-based evaluation, we may work more deductively, with a pre-existing model or theory; we have to look for evidence for the different links in the theory, revise the theory, and then make evaluation judgements based in part on the revised theory. Or we may work more inductively, developing step by step a theory about relevant causal factors and the links of causal influence between them; this means in particular being able to identify some common causal factors within the different claims in order to combine all this information. Whichever route we take, we will have to make decisions about boundaries (what is part of the model, what is not, and who decides?) and about values (what is good, is this good, is it good enough?).

Most evaluators don't ever physically combine all the fragments of causal evidence at their disposal into one single, composite causal map. But in this documentation we argue that they are still in a sense in possession of a causal map, they just haven't drawn it yet; and perhaps they could, and should. We also argue that it is useful to think of a pile of interconnected causal information as a "map" in the sense of an abstract structure for storing causal information. Such a structure might look like a bewildering hairball if we tried to just print out all of it, but if we know the right rules for doing so, we can print out various *summary* maps and *sub*-maps to help us answer various questions. We also argue that in practice evaluators need software to help them to create, organise and store the different pieces of causal information they collect in the course of piecing together answers to evaluation questions; software which understands the translation rules and can help us with producing the right sub-map or aggregate map to answer a particular question.

The heaps of information which evaluators have to deal with, combined into a causal map, are usually composed largely of number-free judgements like "B makes a considerable contribution to E" or "S believes that E happened entirely because of B". This means that the special set of rules which are available to statisticians when processing entirely quantitative maps are not available to us. So what *do* we do, faced with such a heap of information? Even if we are not conscious of it, we make use of a larger set of generally weaker rules which are still available to us, mostly based in the end on "common sense".

A social scientist might throw up their hands in despair at the vagueness of the information we as evaluators have to deal with: causal factors are not clearly defined, links in maps are ambiguous as to whether they refer to groups or individuals, situations or events; it is not clear whether the claims are eternal or momentary, generalisable or specific, and so on. Yet, decisions have to be made, and information is required, so we do the best we can with what we have.

## Features which causal mapping approaches have in common

### Causality

But in our overall definition of causal mapping, what all approaches share is that a causal map's translation rules have to be **explicitly causal**. As Pearl points out, a genuinely causal arrow can be understood as something like "**if you do C, that will make E happen** (perhaps, with probability p)", whereas, for example, Bayesian belief networks are not strictly causal maps because the arrows say "if you observe C, you will observe E (perhaps with the probability p)". Causation is not correlation.

For Pearl and colleagues, the definitive way to check if C has an effect on E is to intervene in the system, if necessary breaking or disabling any incoming links which might determine C, and tweak E to see what difference just that tweaking makes to E.

C and E can be expressed specifically and uniquely or very generally, or anything in between. But as the claim is causal, the link has to express some kind of causal mechanism which is hardly going to make sense restricted to just one single case.

### Modularity

All these approaches share the basic idea that causal knowledge can be at least partially captured in small relatively portable nuggets of information (like "drought causes hunger" or "mosquitoes cause mosquito bites", something like the idea of a "mechanism"), and that these nuggets can be assembled into larger models of how things work, or at least of how they worked in one or a few cases.

We are for the most part not interested in total or exclusive causation but in causal influence.

## Causal maps as a summary of qualitative data analysis of textual causal claims for each link

Usually when we do causal mapping, we try to listen to (or read) what people tell us, and bit by bit ("inductively") try to identify the common elements in their narratives, such as "Health" and "Amount of exercise" for example. Different respondents will, of course, not always use exactly the same phrases and it is a really exciting and creative challenge to create and curate this list of causal factors. This is your job as causal mapping analyst. For example, if Mo says "Feeling good about the future is one thing that increases your wellbeing", is this element "feeling good about the future" the same as "confidence in the future" which Sara mentioned? Should we encode them both as the same thing, and if so, what shall we call it? Positive view of future? Does that cover both cases?

The possibility of coding links between concepts is mentioned briefly in a well-known QDA handbook (Saldaña, 2015) as a possibility, and the Axelrod school has its own coding manual describing how to highlight areas of text expressing causal connections and code them as links between causal factors, inspired by evaluative assertion analysis

This challenge is central to the overlapping field of qualitative data analysis (QDA), which often makes use of tools like NVivo, Dedoose and AtlasTI. However those tools are designed to capture general concepts like "Wellbeing" but are not as well suited to coding links *between* concepts, which is what we need for causal mapping. We believe Causal Map is the only app which is dedicated to helping you with this task.

## Causal mapping as a form of data collection

"Causal mapping" is often used as a name for a specific kind of data collection method, along with suggestions for analysis. There are a vast variety of possibilities for gathering data for causal mapping, with seemingly every author having their own suggestions, from individual interview (Ackermann & Eden, 2004) to reusing open-ended questionnaire questions (Jackson & Trochim, 2002). All of these methods can be coded using the Causal Map app.

Some examples of data collection modalities:

Individual respondents are deliberately asked for information about causal links, for example via open questions at the end of a questionnaire or via a series of interviews in which people are directly asked questions of the form "what causes what?" or "what contributed to this event?"

A set of documents is gathered (either strictly comparable documents as in a medical meta-analysis, or complementary as in a broader review often known as "deskwork," and criteria are drawn up for which sections are to be analysed (e.g., just the executive summaries).

A group of people are deliberately asked about causal links and this information is merged straight away into an overall picture, as a participatory process with the group (Penn & Barbrook-Johnson, 2019) (Markiczy & Goldberg, 1995).

When interviews are carried out, there are different ways to elicit causal claims, for example:

Backwards questioning about problems as in the "problem tree" approach: respondents are asked about a problem in their lives, and what causes it, and what causes those causes, etc.

Backwards questioning about changes, as in QuIP (Copestake, Morsink, & Remnant, 2019) and (rather differently) in Most Significant Change technique (Dart & Davies, 2003): respondents are asked about changes in their lives recently, and then for causes of changes, and then for causes of the causes, etc.

Forwards questioning about effects, as in iterative scenario planning, and in particular in ParEvo: e.g. people are asked what might happen next, and then what that would lead to, and so on.

Each of these approaches have their own detailed suggestions for how to gather and analyse data.

One popular (and cost effective!) way to source data for a causal mapping study is to reuse existing text data which was gathered for another purpose, in particular open ended questions in surveys, providing data protection agreements allow this.

Of course all of the usual procedures for ethical review, protection of respondents, gaining assent and data protection apply as for any other comparable piece of research.

## QuIP as a form of causal mapping

The QuIP is a form of data collection with some very special features:

-   Interviewers are usually blindfolded to the commissioner and to the specific intervention.

-   Respondents are asked about changes in key domains, generating a backwards chain of causal explanations ("and what influenced that? .... And what influenced *that*?").

QuIP analysis also involves qualitative, inductive coding. The above features have implications for the kind of coding we do. The causal factors in QuIP:

-   deal with actual events which happened to the respondents rather than with general principles

-   usually take the form of changes ("improved" / "decreased" etc).

Finally, in the QuIP we are interested first of all in people's *beliefs* about what causes (or caused) what, constructing a causal evidence map. Only then, as an optional next step, do we consider whether, and how much, we can deduce from that what *actually* causes (or caused) what. In particular, we are interested what we can deduce about causal paths from explicitly and implicitly identified interventions to other specific factors ("Outcomes") which are agreed to be important.

### QuIP: explanations of changes do not themselves also have to be changes

One question which comes up a lot is this: In QuIP, we ask people to describe changes in their lives over (say) the last three years, and then generate a backwards chain of causal explanations by asking "and what influenced that? .... And what influenced *that*?". So, do the *explanations* also have to be expressed as changes in the last three years? For example, someone might say this:

*I know more about water conservation now because of the radio broadcasts sponsored by Organisation X, which we didn't have three years ago. But I also know more because of the local government's agricultural officers who explain things to me. They've been around for ever, and they haven't changed their activities, it's just that I learn something new from them every time I meet them.*

We recommend including the agricultural officers as a causal factor influencing the change in knowledge, even though their activity has not changed.

This means that we also sometimes code factors like "God" and "Unemployment" which are often mentioned as causal influences even though they may not themselves have changed. Of course, we hope that the interviewers have been trained to gently question whether the respondent really means to describe a causal influence and not is not just producing an empty formula out of habit.

If we look at the same question from a perspective of quantitative statistics, we note that if we had data both from three years ago and from now (which we don't), we would indeed observe a correlation between presence of radio broadcasts and knowledge of water conservation. On the other hand, we wouldn't have any correlation between presence of radio broadcasts and input from agricultural officers, because there is no variation in the officers' input; it was the same all the time. This fact might lead us to feel that there is something illegitimate about our recommendation to include the officers' input as a causal factor. We would perhaps like to have data from a parallel world in which there were no agricultural officers over the whole three years, to see whether the knowledge increased, but we don't. But if we think more carefully, we will realise that nor do we in fact have data from three years ago on the radio broadcasts either. What we have in both cases is not a statistical contrast but our respondents' more or less implicit causal claim that it was both the addition of the radio broadcasts *and* the presence (rather than the absence) of the agricultural officers which each made a difference. The validity of people's causal information comes primarily from a whole shared knowledge map gained over time from culture, instruction and experience. For example, it is not usually the case that I think "ooh, my knowledge seems to be going up, what could have caused that?" but rather we are well aware of the causes because we are part of, inside, the whole process, and we know what it is like to gain understanding when and because someone explains something. It is true that this knowledge is sometimes updated using systematic observation of contrasting cases, whether before-against-after, or here-against-there, or even occasionally using experimental manipulation; but this is an important additional option rather than a primary or original source of causal information.

## Advantages of causal mapping

### Induct

Causal mapping aims to directly understand and collate the causal claims which people make in narrative (and other) data rather than trying to deduce causal connections using statistics or some other method. It starts with what people actually say in real-world contexts and does not rely on heavily pre-structured question formats. Urgent, unexpected, and unwelcome information is treated at face value.

In some forms of causal mapping, the map is drawn as a synthesis of the views of contributors in a participatory process. In other forms, like QuIP, each contributor is helped to produce their own map and these maps are synthesised later by an analyst. In either case, the maps do not need to follow any preconceived conceptual framework; types of causal claims are identified inductively and iteratively. This is a partly creative process, however the decisions made during synthesis are transparent as the underlying text is always available.

At least some of the *boundaries* of causal mapping research (what are we going to talk about? What are we not going to talk about?) are set by the respondents, not the researchers.

### Discover

Causal maps work on two levels. On one level, they are presentations of individual and shared cognitive structures, the maps "in people's heads" which are real (social-)psychological things which we need to know about if we want to understand, predict and influence behaviour. On the other level, they are putative, fallible maps of the actual causal world: how things work. Like all other research results, these maps may be wrong, but they usually contain at least some truth. At Causal Map we take a realist stance on both of these levels: the maps in people's heads are real, and the causal world, made up of many causal links between causal factors, is real too.

### Distinguish

It is possible for a causal map to be able to encode both general claims of the form B causes E as well as specific historical claims that B in fact caused E.

### Present

The results of ordinary qualitative research on texts is usually just more text, with maybe some tables of frequency of occurrence or co-occurrence of particular themes for particular respondents, with maybe a chart or even a network graph to present these results. Causal maps on the other hand are not additional presentations of additional analyses but are the main product of qualitative causal mapping. They are relatively intuitive and easy to understand.

### Query

A global causal map resulting from a research project can contain a large number of links and causal factors. By applying filters and other algorithms, a causal map can be queried in different ways to answer different questions, for example to simplify it, to trace specific causal paths, to identify significantly different sub-maps for different groups of sources, etc. With certain assumptions, it is possible to ask and answer questions like "which is the largest influence" or "which is the most positive effect".

### Quote

The original quote or other evidence on which each causal link is based can be stored within the link itself. That means that at every stage of causal mapping, it is possible to directly to return to the story in the original context.

### Reuse

Causal mapping also encourages reanalysis of existing narrative data which is often gathered but left unanalysed. It is highly suited to online use, e.g. gathering narratives via online interview, email, questionnaire etc., reducing airmiles and viral risk. There is no need for a foreign evaluator to travel to gather or collate data.

## When to use Causal Map

So, you've taken a look at the [**features of the app**](https://causalmap.app/the-app/), and you're getting excited about creating maps -- but is your project right for it?

Use Causal Map if you:

-   have a relatively large amount of narrative data (enough to provide at least 20-30 causal links)
-   need help to organise a large number of links and summarise them into an overview or synthesis
-   have information from more than one source (for example different respondents, different documents, or different places in one document) and the information about the source is important to you: they aren't all interchangeable
-   are interested in possible differences between the sources and groups of sources -- and/or you don't necessarily have a preconceived idea of the contents or boundaries of the map.
-   want to capture what your sources actually say, systematically and transparently

Causal Map map is not suitable if you:

-   only have a relatively small map which you can manage with traditional tools for drawing network diagrams (e.g. PowerPoint, kumu.io etc.)
-   need to analyse quantitative data and/or need to do precise mathematical modelling, e.g. of future states of a system under certain conditions
-   would like to sketch out a plan (e.g. Theory of Change or similar) without much reference to the different sources underpinning each link
