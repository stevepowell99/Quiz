# ⚡ The top menu{#xtop-menu}

This section is all about how to manage using the top menu to help you deal with the currently loaded file. Hover over the icons to see what they do.

![](_assets/image-20211025111815970.png){width=650}

You can use these buttons for quick fixes. There is also a dedicated [File Manager](#file-manager) to manage all your files.

## Current File Manager{#xpermissions}

Click the green File icon to manage your file.

![uBfaYOvRRH](_assets/uBfaYOvRRH.gif){width=650}

![](_assets/image-20211025111538915.png){width=650}

### Sharing and locking files

You can add any number of other Causal Map accounts into the "copy" and/or "view" boxes. 

- "Copy" permission means that the other user can make their own copy of this file, which they can then edit.

- "View" permission means that the other user can make only view the file.

You can also use this switch to lock the file. This means that no-one can change it, not even you, until it is unlocked again.

It is also possible to create larger groups of users. Ask us for help with this: hello@causalmap.app. 

### File memo

You can put information about your file in the `file memo` box, and it will be shown below the interactive map to tell other users what this file is about. You can use markdown to format the text, e.g. start a line with # to make level-1 heading. You can even include images using markdown.

![image-20211103133030379](_assets/image-20211103133030379.png)

This is in addition to any "newsflash" information about updates which you add using the chat widget.



## ⚡ Making your own copy of a file{#xown-copy}


```{r,echo=F}
knitr::include_url("https://player.vimeo.com/video/641927229")
```
Copying a file in Causal Map is easily done. Simply load the file you want to copy using the dropdown menu. Then click the 'Save As' button on the top left of your screen. 

![](_assets/5Najfi4Pr2.gif){width=650}

You can then append something to the end of the filename to make it yours:

![image-20211001180419448](_assets/image-20211001180419448.png){width=650}

Click the 'Save current map' button. This window will then close and your copy of the file will load up. You can now make changes to the file. 

## Restoring a previous version of your file

The app is continously saving your work and so you can restore your file to any prevision version, by clicking this icon ![image-20211115175030770](_assets/image-20211115175030770.png). This will open the below panel where you can choose which timepoint you wish to revert your map to. 

![image-20211115174934976](_assets/image-20211115174934976.png){width=650}

This panel shows a dropdown list of times when you made changes to the mapfile in GMT. Along with the size of your file which can help you identify which timepoint you want to revert to. It can be easy to forget what time you made alterations to your file, so if you're likely to want to restore a previous map it is best to note the time so that you can easily return to it.

## Sharing a link using shortlinks{#xshortlinks}

To share a link to a file click the orange arrow on the top panel. You can then add a title for this shortlink, to remind you of its content, but this is optional. The short link will be copied to your clipboard and you can send it to someone else e.g. via email. Anyone who clicks on this link will be able to see your map with the current filters applied (providing they have the correct [permissions](#xpermissions)).

![6vEfSLFOlx](_assets/6vEfSLFOlx.gif){width=650}

The server which a shortlink takes you to is (obviously?) the one in the shortlink itself. So https://causalmap.shinyapps.io/CM2test/?s=138 takes you to the Test server whereas if you change the server name to https://causalmap.shinyapps.io/CausalMap2?s=138 that will recreate the same filter on the Production server.

Shortlinks also store information about when the link was created and who by. 

The [Gallery](#xga) includes all shortlinks, initially filtered to show just shortlinks for the current file.

You can use markdown syntax to format your title, for example

- Leave an empty line (press Enter *twice*) to create a new line
- Use single stars around a word or phrase to get italics: `This will be in *italics*`
- Use double single stars around a word or phrase to get bold: `This will be in **bold**`




## Uploading your data

![image-20211115175648298](_assets/image-20211115175648298.png){width=650}

The above buttons relate to uploading your data, see [this section](#ximport) for more information.

## Chat about this file

By clicking on the icon of two speech bubbles, a panel containing chat about the file will appear. You can see the comments you and other people with access to this file have added. From here you can also add updates on this file, this function is useful when working collaboratively.

![image-20211115175519249](_assets/image-20211115175519249.png)



> Why did I make this version of that file? Is it different from version xzy? Did we already import those new statements into this version??

This panel can help with these kinds of problems. You and your colleagues -- anyone else with access to the current file -- can type notes and updates about the file. These updates will be listed here sequentially. 

Also, these same notes and updates are listed in the main Updates panel, along with automatic updates about when files were opened and edited.

![image-20211018121359141](_assets/image-20211018121359141.png)

So you can use the filters just to search for manual and automatic updates about particular files, or on particular dates; or you can just search for type=chat to view recent chat about any files or some particular files:

![image-20211122063223836](_assets/image-20211122063223836.png)
