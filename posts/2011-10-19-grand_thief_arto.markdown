---
title: "Grand Thief Arto"
tags: Games
favorite: true
---

Here's me and Li's game for our school course. The game isn't tweaked too much but it's a game with some fairly cool ideas.

# Grand Thief Arto

![](/images/games/arto1.png)
![](/images/games/arto2.png)
![](/images/games/arto3.png)

# Instructions

Your goal is to collect loot until you can escape through the entry point when you've collected enough to complete the level.

There are touchpads and lasers you need to shutdown by walking next to a computer or an electric board and shut them down from there. Otherwise you need to open doors and then just run around and collect.

You can either play by launching the bash script "play" directly in the folder or launch the game with:

    java base.GameFrame
    or
    java -Xss2048k -Xms64m -Xmx1024m base.GameFrame

Or whatever your preferred way of launching java apps is. You obviously need the java runtime, otherwise you might fetch it from [here](http://www.java.com/en/download/index.jsp).

You might need to create the binary files for it. Use the bash script "create" or do:

    javac base.GameFrame

But it might not be necessary.

**Controls**  
Arrow keys - Walk around  
Space - Do action  
Esc - Menu
