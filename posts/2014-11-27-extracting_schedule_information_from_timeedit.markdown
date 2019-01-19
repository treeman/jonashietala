---
layout: post
title: "Extracting schedule information from timeedit"
tags: Programming
---

At [liu][] we use [timeedit][] to track our schedules. Recently they updated their interface and improved some parts. It's now possible to create a prenumeration of a collection of courses exported as `csv` which can then be imported to other calendar apps. But they also started to obfuscate their urls.

This is the schedule for a single course `TGTU49` in the interval "now - 2015-01-17":

<https://se.timeedit.net/web/liu/db1/schema/ri157XQQ519Z50Qv27045gZ6y7Y7206Q6Y45Y7.html>

This is for the courses `TGTU49` and `TATA49` in the same interval:

<https://se.timeedit.net/web/liu/db1/schema/ri15YXQ2519Z55Qv2X0457Q6y4Y120876Y45Y7gQ6076775Z7.html>

There seem to be some logic behind the urls, but first, let's look at how they handle search.

Using [firebug][] we can intercept the json requests when we click the search button at

<https://se.timeedit.net/web/liu/db1/schema/ri1Q7.html>

We find a GET request to

<https://se.timeedit.net/web/liu/db1/schema/objects.html?max=15&fr=t&partajax=t&im=f&sid=3&l=sv&search_text=TGTU49&types=219&fe=132.0&fe=115.20132,20141,20142>

which has has some search results formatted as html:

```{.html}
<div id="objectbasketitemX0" data-id="363972.219" data-idonly="363972" data-type="219" data-name="TGTU49, Teknikhistoria, 1, TGTU49 1445-1503 Valla"  class="clickable2 searchObject   " >
  <table id="table0" cellspacing="0" cellpadding="0" class="infotable infotablenormal">

    <td>
    <div id="info0">
    <div class="infoboxtitle ">
      TGTU49, Teknikhistoria, 1, TGTU49 1445-1503 Valla
    </div>
    <div class="objectfieldsextrawrap">
    <table cellspacing="0" class="objectfieldsextra">
    </table></div>

    </div>
  </td>
  </tr></table>
</div>
```

`types=219` means we're searching for a course and if can change it to `types=205` we can search for student groups instead. If we change `objects.html` to `objects.json` we even get a json response:

```{.json}
{"Kurskod":"TGTU49","Kursnamn":"Teknikhistoria","Omgång":"1","Kurstillfälle":"TGTU49 1445-1503 Valla"}
```

json looks much better and it's a lot easier to parse, but unfortunately if we instead search for "TGTU" we only get one match but using html we get six. What's worse, there's a lot less information given with json. `data-id` for example seems important as it's the unique id signifying the course.

As I'm writing this I tried `.txt` as well and we get a much larger json response which contains `data-id`, among other things.

Investigating the page source in ta schedule page we find

```{.html}
<span class="hidden" id="links_base" data-base="https://se.timeedit.net/web/liu/db1/schema/ri.html?h=t&amp;sid=3&amp;p=0.m%2C20150117.x&amp;objects=363730.219&amp;ox=0&amp;types=0&amp;fe=0"></span>
```

Which seems to suggest that we can send a request directly to <https://se.timeedit.net/web/liu/db1/schema/ri.html> with url parameters.

For example

<https://se.timeedit.net/web/liu/db1/schema/ri.html?sid=3&p=140703-141231&objects=363972.219>

Is a search for `TGTU49` (data-id: 363972.219) between 2014-07-03 and 2014-12-31 and

<https://se.timeedit.net/web/liu/db1/schema/ri.html?sid=3&p=140703-141231&objects=363972.219%2C363741.219>

searches for `TGTU49` (data-id: 363972.219) and `TATA49` (data-id: 363741.219).

`sid` is an important flag here which specifies what information you're interested in.

```
sid               Information given
------------------------------------------------------
3                 Date Course Local Type Teacher Group
4                 Date Type Teacher Local
others            Date
```

There's a bunch of other parameters which we can simply ignore. This time if we replace `.html` to `.json` we get a nice json request:

```{.json}
{
   "columnheaders":[ "Kurs", "Lokal", "Undervisningstyp", "Lärare", "Studentgrupp", "Fria grupper", "Information till student", "Studentförening", "URL" ],
   "info":{ "reservationlimit":50, "reservationcount":17 },
   "reservations":[
      {
         "id":"361731",
         "startdate":"2014-11-04",
         "starttime":"13:15",
         "enddate":"2014-11-04",
         "endtime":"15:00",
         "columns":[ "TGTU49", "KEY1", "Föreläsning", "Dick Magnusson", "KA3, EMM3, EL3, PRO2", "", "", "", "" ]
      }, ...  ]
}
```

We can also get information formatted as `.txt` and `.csv`. For parsing purposes the best one should be json, as we can use rust's [json deriving][], but then we need to create a bunch of structs. `.csv` is actually very easy to parse and it looks like this:

```{.csv}
TimeEdit, Linköpings universitet
"TGTU49, Teknikhistoria, 1, TGTU49 1445-1503 Valla", 2014-07-03 - 2014-12-31 ,
Startdatum, Starttid, Slutdatum, Sluttid, Kurs, Lokal, Undervisningstyp, Lärare, Studentgrupp, Fria grupper, Information till student, Studentförening, URL
2014-11-04, 13:15, 2014-11-04, 15:00, TGTU49, KEY1, Föreläsning, Dick Magnusson, "KA3, EMM3, EL3, PRO2", , , ,
...
```

Which is what I did. For a more stable library I should probably switch to json parsing, but there it is.

I made a [rust crate][libtimeedit] which uses the outlined approach to retrieve scheduling information from timeedit. There's a bunch of things I'm not supporting, but it's enough for my needs. I made a simple [cli][liuschema] and integrated a `.schema` command into my [irc bot][].

[liu]: #
[timeedit]: #
[firebug]: #
[json deriving]: http://doc.rust-lang.org/serialize/json/ "rust json serialization doc"
[libtimeedit]: https://github.com/treeman/libtimeedit "3rd party api for timeedit"
[liuschema]: https://github.com/treeman/liuschema "timeedit cli"
[irc bot]: https://github.com/treeman/rustbot "an irc bot in rust"

