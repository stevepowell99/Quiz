# The Mentions Table{#xthe-mentions-table}

```{r,echo=F}
knitr::include_url("https://player.vimeo.com/video/596594094")
```


This table is useful when looking at how many times factors have been mentioned in links. In the Factors table, you can display how many links are coming in and out of each factor overall, but you can't for example ask how many different *sources* mentioned it, or in how many different districts.

This table loads by default with a preset; you will nearly always use this table with one of the two preset buttons at the top left. (The underlying data, which you can view if you clear the Group by and Count boxes, is a bit hard to understand, because it contains four rows for each link.)  

![image-20211122131859032](_assets/image-20211122131859032.png){width=650}

The Direction field shows you the number of times each factors was reported as an influence factor and/or a consequence factor.

- From = how many times the factor was applied as an **influence** factor, i.e. leading to another factor.
- To = how many times the factor was applied as a **consequence** factor, i.e. as a result of another factor.
- Either = how many times the factor was mentioned in **either** direction.

Remember, in each case these counts depend on what you put in the Count box. You can count link_id (which gives us the overall count, the highest number) or statement_id, source_id, district etc (which usually give a lower number).

The other preset `influence * consequence, source count` displays influence factors in the rows and consequence factors in the columns. As usual you can change what is counted by selecting a different field in the `Count` box.

This table is merely an overview which can help us to understand which factors are reported most frequently, and whether specific factors were more often cited as an influence or a consequence. To fully understand what the factors mean, they need to be seen in the context of the causal stories they appear in. This table can be useful for initial communication about which factor labels have been created and how often they have been applied, but do exercise caution when presenting this data as it only shows the factor in isolation, whereas when doing causal mapping we are most interested in the relationships between factors.

It is often interesting to see how many sources mentioned the factors without worrying about the direction, i.e. whether they were mentioned as influence or consequence. That's what the "Source counts" preset is for. This uses the field `mentions` which is frankly a bit of a hack, as it only has one value, namely "any", but it serves to construct the table we wanted:

![image-20211122133447831](_assets/image-20211122133447831.png){width=650}
