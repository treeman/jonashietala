---
layout: post
title: "Summer job at Configura"
tags: Programming
---

This is a wrap up of my 5 weeks at [Configura][] as a summer internship. There were 6 interns and we had two teams doing different things. I worked with two other awesome guys and it was great!

The Premise
===========

We had two assignments this summer. One was to create an optimized version of an octree (more on that later). We had a reference implementation in CM, but we wanted to take it down to C++ and to optimize it and make it stable and robust. This was a self-contained assignment which could be used for various things in their application.

The second assignment was to use our octree and simplify meshes by removing invisible triangles from them. Here again we had a reference implementation to work with but we were going to make it faster and more robust.

On top of that we had to make debugging and visualization tools and a test suite.


The Result
==========

<a href="/images/configura14/argonath1.png"><img src="/images/configura14/argonath1.png" width=600/></a>

In this picture we can see the original model on the left. The green outline to the right are the triangles we deemed visible and the red mesh corresponds to the invisible triangles.

<a href="/images/configura14/argonath2.png"><img src="/images/configura14/argonath2.png" width=600/></a>

Here we only show the removed triangles and we can see that there's a dude inside the statue, which we mostly remove. This model has about 730 000 triangles and we managed to remove about 300 000 of them, which is pretty cool.

<a href="/images/configura14/adolf1.png"><img src="/images/configura14/adolf1.png" width=600/></a>

This is another interesting example. We can see that inside this model, we have a high resolution spring with nearly 3500 triangles!

As for speed, we significantly improved the runtime of both the octree and the ray casting. For a specific model with 20 000 triangles which took around 15 minutes with a small depth, we managed to get it down to about a second with a larger depth. We achieved similar results with the ray casting.

In the end our bosses seemed pleased, so I consider the summer job a success. I did suck at Disc Golf though.


Execution
=========

Octree
------

An [octree][] is an extension of a [quadtree][] from 2D to 3D. The simple explanation is that we have a box, and try to insert triangles (or points, or whatever) into it. When the box has enough triangles we split it into 8 parts (4 with a quadtree) and then insert the triangles into the new boxes if they intersect them.

The octree is used in various ways, for a fast traversal with a ray, mesh healing, finding neighbours or other things.

<a href="/images/configura14/octree1.png"><img src="/images/configura14/octree1.png" width=400/></a> <a href="/images/configura14/octree2.png"><img src="/images/configura14/octree2.png" width=400/></a>

Here we can see the root bounding box and the subsequent subdivision into smaller boxes. The green boxes have none or few triangles, the yellow and red have more triangles in them. Because we force our wctree to have a maximum depth some nodes like the red ones on top of the model can have a lot of triangles in them without further subdividing. This is basically a trade off between the memory and time usage versus how good the octree becomes.

<a href="/images/configura14/socket1.png"><img src="/images/configura14/socket1.png" width=400/></a> <a href="/images/configura14/socket2.png"><img src="/images/configura14/socket2.png" width=400/></a>

This octree is very sparse at the top, which makes sense as there are no triangles there, and it's dense in the middle. Interestingly we can see something sticking out inside the model, which means there are more triangles there. Indeed, the model has the actual holes, we just can't see them.

<a href="/images/configura14/octree_debug.png"><img src="/images/configura14/octree_debug.png" width=600/></a>

In this picture we can see one of our debug tools. This is a single large triangle spanning over almost the whole model and the generated octree. The colored boxes show which nodes the triangle is inserted into, which can be an invaluable debugging tool when changing how triangle-box intersection and octree insertion works.

It also looks cool.

These are the basic optimization steps we did:

1. Implement it in C++

    The performance boost you can get by moving down closer to the metal should not be underestimated.

2. Use custom memory pools for the triangles, nodes and even the sequences holding them.
3. Implement the AABB triangle intersection tests with the separating axis theorem.

    We had a link to a nice explanation, but now I can't find it.

4. Cache the above triangle projections.

    They will be used *a lot*. Use a cache to reuse them when promoting nodes.

5. Don't start insert from the root and search down, but walk up until we find something we know we're inside.

    This exploits the fact that the triangles in the meshes are usually "close" to each other. We tried to presort the triangles using a fancy thing called the [Hilbert curve][] which one can realize using [Morton codes][] (using [fancy bit manipulations][morton-stack]), but unfortunately the presorting pass wasn't worth it.

Ray casting
-----------

Our strategy for finding invisible triangles is quite simple: we cast rays towards the still invisible triangles from outside the model, and see what we can hit. All triangles our rays hit, are visible and the rest we can remove.

<a href="/images/configura14/casting.png"><img src="/images/configura14/casting.png" width=600/></a>

This is another of our debug tools where we can select a triangle (pointed by the arrow) and all the ray we try to cast towards it. In this case we fail to hit the triangle and instead we hit all the blue/purple triangles instead. This is a failed triangle, but I think it's a nice illustration of how we tried to find invisible triangles.

The first pass of rays we only cast one ray along the triangle's normal towards the visible side of the triangle. During the second pass we start to cast rays in a hemisphere, gradually increasing the density of the rays.

For each ray we traverse the octree and for every box the ray intersects we check the triangles inside against the ray. This is fast because don't have to check against all other triangles at every step, but only against a fixed amount if the octree is well formed.


Conclusions
==========

This was my second summer at [Configura][] and it's a pretty cool company. They gave us a lot of freedom and we got to work on pretty cool things and they have their own programming language which is fun to work with.

We rewrote the implementations from scratch, we battled with precision errors and edge cases but in the end our mission was a success. It was a very rewarding experience as the task was quite complex, but we managed to complete it in good style. We got to work with visualization on a high level and we also got to really dive into some low-level optimizations.

Oh, and something cool we hacked in during my last day: A better triangle flipper to turn triangles face up.

<a href="/images/configura14/flipper1.png"><img src="/images/configura14/flipper1.png" width=400/></a> <a href="/images/configura14/flipper2.png"><img src="/images/configura14/flipper2.png" width=400/></a>

Left: Triangles turned inwards  
Right: Triangles turned outwards (without blindly copying everything)


[Octree]: http://en.wikipedia.org/wiki/Octree "Octree"
[Quadtree]: http://en.wikipedia.org/wiki/Quadtree "Quadtree"

[Hilbert curve]:http://en.wikipedia.org/wiki/Hilbert_curve "Hilbert curve"
[Morton codes]: http://en.wikipedia.org/wiki/Z-order_curve "Morton Code, Z-order curve"
[morton-stack]: http://stackoverflow.com/questions/1024754/how-to-compute-a-3d-morton-number-interleave-the-bits-of-3-intsa "Morton codes, stackoverflow"
[Configura]: http://www.configura.com/ "Configura"

