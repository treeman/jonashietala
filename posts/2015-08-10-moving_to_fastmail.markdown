---
layout: post
title: "Moving to FastMail"
tags: FastMail
---

About two years ago after [Lavabit's shutdown][lavabit] I searched for a replacement email provider. For some reason I settled on [eumx][] which I used since then. Yesterday I got an email saying the recurring billing could not be renewed as I had my old credit card registered and it got me thinking of moving on. I wasn't particularly unhappy but I had some annoyances. Their webmails' were a bit obnoxious to use but I could live with it. Something more alarming is that [they don't use ssl consistently][eumx_support]. I remember I pointed out this to them but at the moment it's not fixed (or the error has returned).

Whatever. [FastMail][] often gets mentioned as a Gmail alternative and I decided to use them. Their webmail feels very, very good and it's also possible to pay with [Bitcoin][]! This actually marks the first real purchase I've made with bitcoins and it was all very painless.

It was easy to migrate my mail from eumx by using their migrate service from IMAP `ssl.eumx.net`. Migrating from Gmail gave me more trouble as Gmail denied me access. After creating an app specific password I finally made it work. For more help see <https://www.fastmail.com/help/receive/migrate.html>.

Registering my own domain was very easy with the [instructions][domain]. I could not find instructions to setup DKIM specific for [loopia][], but it was easy enough to figure out.

In a subdomain `mesmtp._domainkey` set a TXT record with the value of the public key found in Advanced -> Virtual Domains -> DKIM signing keys.  You can check the settings with `dig`:

```{.bash}
$ dig mesmtp._domainkey.jonashietala.se TXT

; <<>> DiG 9.9.3-P2 <<>> mesmtp._domainkey.jonashietala.se TXT
;; global options: +cmd
;; Got answer:
;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 29301
;; flags: qr rd ra ad; QUERY: 1, ANSWER: 1, AUTHORITY: 0, ADDITIONAL: 1

;; OPT PSEUDOSECTION:
; EDNS: version: 0, flags:; udp: 1280
;; QUESTION SECTION:
;mesmtp._domainkey.jonashietala.se. IN	TXT

;; ANSWER SECTION:
mesmtp._domainkey.jonashietala.se. 3600	IN TXT	"v=DKIM1\; k=rsa\; 
    p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCpqEJlGriDgRE6qjys7e424xv5K9LAJrvTQ8/K8Lj4h
```

Where you should see the public key. After the settings have propagated the Set field under DKIM signing keys should change from `[]` to `[*]`.

I'm also trying out two factor authentication with [Google Authenticator][gauth]. Although FastMail implements it in a strange way, using a base password and then appending the OTP from the authenticator. It's a bit annoying but it works I guess.

Overall [FastMail][] seems very good, the little I've used it, and it might be worth taking a look at.

[lavabit]: https://en.wikipedia.org/wiki/Lavabit "Lavabit's history"
[eumx]: http://www.eumx.net "eumx.net"
[eumx_support]: http://www.eumx.net/contact.php "eumx.net support"
[FastMail]: https://www.fastmail.com/ "FastMail"
[bitcoin]: https://en.wikipedia.org/wiki/Bitcoin "Bitcoin"
[loopia]: https://loopia.se "Loopia"
[domain]: https://www.fastmail.com/help/receive/domains.html "Custom domain with FastMail"
[gauth]: https://en.wikipedia.org/wiki/Google_Authenticator "Google Authenticator"

